library(dplyr)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)
library(tidyr)


### Loading the data -------------------------------------------------------------------------------
source('SensorData_oo.R')
data = pantrydb()

#Helper function to fix the problematic events
fix_long_door_events <- function(events,
                                 other_events,
                                 raw_logs,
                                 door_col = "door1_open",
                                 max_duration_mins = 2.5,
                                 other_door_max_mins = 2.5,
                                 time_window_secs = 30,
                                 min_reduction_secs = 10,
                                 tz = NULL,
                                 verbose = FALSE) {
  

  # Resolve dynamic door columns ---------------------
  open_col  <- paste0(door_col, ".x")
  close_col <- paste0(door_col, ".y")
  
  # Normalize timestamps ---------------------
  if (!is.null(tz)) {
    events       <- events       %>% mutate(openTS = with_tz(openTS, tz), closeTS = with_tz(closeTS, tz))
    other_events <- other_events %>% mutate(openTS = with_tz(openTS, tz), closeTS = with_tz(closeTS, tz))
    raw_logs     <- raw_logs     %>% mutate(timestamp = with_tz(timestamp, tz))
  }
  
  events       <- events       %>% mutate(openTS = as.POSIXct(openTS), closeTS = as.POSIXct(closeTS))
  other_events <- other_events %>% mutate(openTS = as.POSIXct(openTS), closeTS = as.POSIXct(closeTS))
  raw_logs     <- raw_logs     %>% mutate(timestamp = as.POSIXct(timestamp))
  

  # Coerce IDs to character ---------------------
  events       <- events       %>% mutate(openID = as.character(openID), closeID = as.character(closeID))
  other_events <- other_events %>% mutate(openID = as.character(openID), closeID = as.character(closeID))
  
  # Compute durations ---------------------
  events <- events %>% mutate(duration = difftime(closeTS, openTS, units = "mins"))
  

  # Temporary event key ---------------------
  events <- events %>% mutate(event_key = paste0(openID, "__", closeID))
  

  # Precompute next real openTS ---------------------
  events_sorted <- events %>% arrange(openTS)
  events_sorted <- events_sorted %>% mutate(next_openTS = lead(openTS))
  

  # Identify targets ---------------------
  targets <- events_sorted %>% filter(as.numeric(duration) > max_duration_mins)
  
  corrected_rows <- vector("list", nrow(targets))
  report_rows    <- vector("list", nrow(targets))
  

  # Template columns ---------------------
  template_cols <- union(colnames(events),
                         c("is_synthetic", "source"))
  
  make_row <- function(x) {
    missing <- setdiff(template_cols, names(x))
    if (length(missing) > 0) x[missing] <- NA
    x <- x[template_cols]
    x
  }
  
  # Loop over targets ---------------------
  for (i in seq_len(nrow(targets))) {
    
    trow <- targets[i, ]
    orig_openID  <- trow$openID
    orig_closeID <- trow$closeID
    g <- trow$group
    
    openTS_trow   <- trow$openTS
    closeTS_orig  <- trow$closeTS
    orig_duration_secs <- as.numeric(difftime(closeTS_orig, openTS_trow, units = "secs"))
    

    # Candidate from other door ---------------------
    d_other <- other_events %>%
      filter(closeTS > closeTS_orig) %>%
      arrange(closeTS) %>%
      slice(1)
    
    d_other_time <- if (nrow(d_other) == 0) NA else d_other$closeTS
    
    if (!is.na(d_other_time)) {
      if (difftime(d_other_time, openTS_trow, units = "mins") > other_door_max_mins)
        d_other_time <- NA
    }
    
    # Candidate from raw logs ---------------------
    repeating_time <- raw_logs %>%
      filter(timestamp > openTS_trow + 1, !is_event) %>%
      arrange(timestamp) %>%
      slice(1) %>%
      pull(timestamp)
    
    if (length(repeating_time) == 0 ||
        difftime(repeating_time, openTS_trow, units = "mins") > max_duration_mins)
      repeating_time <- NA
    
    # Combine candidates ---------------------
    candidate_times <- c(d_other_time, repeating_time)
    candidate_times <- candidate_times[!is.na(candidate_times)]
    closest_time <- if (length(candidate_times) > 0) min(candidate_times) else NA
    
    created <- list()
    action <- "skipped"
    reason <- NA_character_
    
    long_threshold_secs <- 2 * max_duration_mins * 60
    
    # Case logic ---------------------
    if (length(candidate_times) == 0 && orig_duration_secs > long_threshold_secs) {
      
      closeA <- openTS_trow + dminutes(max_duration_mins)
      openB  <- closeTS_orig - dminutes(max_duration_mins)
      
      created <- list(
        tibble(
          openID = paste0(orig_openID, "_A"),
          openTS = openTS_trow,
          !!open_col := TRUE,
          group = g,
          closeID = paste0(orig_openID, "_A_close"),
          closeTS = closeA,
          !!close_col := FALSE,
          duration = difftime(closeA, openTS_trow, units = "mins"),
          is_synthetic = TRUE,
          source = "split_partA"
        ),
        tibble(
          openID = paste0(orig_openID, "_B"),
          openTS = openB,
          !!open_col := TRUE,
          group = g,
          closeID = paste0(orig_openID, "_B_close"),
          closeTS = closeTS_orig,
          !!close_col := FALSE,
          duration = difftime(closeTS_orig, openB, units = "mins"),
          is_synthetic = TRUE,
          source = "split_partB"
        )
      )
      
      action <- "split_into_two"
      reason <- "no_candidate_>5"
      
    } else if (length(candidate_times) == 0 && orig_duration_secs <= long_threshold_secs) {
      
      closeA <- openTS_trow + dminutes(max_duration_mins)
      
      created <- list(
        tibble(
          openID = paste0(orig_openID, "_A"),
          openTS = openTS_trow,
          !!open_col := TRUE,
          group = g,
          closeID = paste0(orig_openID, "_A_close"),
          closeTS = closeA,
          !!close_col := FALSE,
          duration = difftime(closeA, openTS_trow, units = "mins"),
          is_synthetic = TRUE,
          source = "replaced_closing"
        )
      )
      
      action <- "replaced_closing"
      reason <- "no_candidate_<5"
      
    } else if (length(candidate_times) > 0 && orig_duration_secs > long_threshold_secs) {
      
      closeA <- closest_time
      openB  <- closeTS_orig - dminutes(max_duration_mins)
      
      created <- list(
        tibble(
          openID = paste0(orig_openID, "_A"),
          openTS = openTS_trow,
          !!open_col := TRUE,
          group = g,
          closeID = paste0(orig_openID, "_A_close"),
          closeTS = closeA,
          !!close_col := FALSE,
          duration = difftime(closeA, openTS_trow, units = "mins"),
          is_synthetic = TRUE,
          source = "split_partA"
        ),
        tibble(
          openID = paste0(orig_openID, "_B"),
          openTS = openB,
          !!open_col := TRUE,
          group = g,
          closeID = paste0(orig_openID, "_B_close"),
          closeTS = closeTS_orig,
          !!close_col := FALSE,
          duration = difftime(closeTS_orig, openB, units = "mins"),
          is_synthetic = TRUE,
          source = "split_partB"
        )
      )
      
      action <- "split_into_two"
      reason <- "candidate_>5"
      
    } else if (length(candidate_times) > 0 && orig_duration_secs <= long_threshold_secs) {
      
      closeA <- closest_time
      
      created <- list(
        tibble(
          openID = paste0(orig_openID, "_A"),
          openTS = openTS_trow,
          !!open_col := TRUE,
          group = g,
          closeID = paste0(orig_openID, "_A_close"),
          closeTS = closeA,
          !!close_col := FALSE,
          duration = difftime(closeA, openTS_trow, units = "mins"),
          is_synthetic = TRUE,
          source = "split_partA"
        )
      )
      
      action <- "replaced_closing"
      reason <- "candidate_<5"
    }
    
    # Validate created rows ---------------------
    if (length(created) > 0) {
      invalid <- any(vapply(created, function(r) r$openTS >= r$closeTS, logical(1)))
      if (invalid) {
        created <- list()
        action <- "ambiguous"
        reason <- "created_row_invalid_time_order"
      } else {
        next_open <- trow$next_openTS
        if (!is.na(next_open)) {
          conflict <- any(vapply(created, function(r) r$closeTS >= next_open, logical(1)))
          if (conflict) {
            created <- list()
            action <- "ambiguous"
            reason <- "created_rows_conflict_with_next_real_open"
          }
        }
      }
    }
    
    # Store results ---------------------
    if (length(created) == 0) {
      corrected_rows[[i]] <- make_row(
        trow %>% mutate(is_synthetic = FALSE, source = "unchanged")
      )
    } else {
      corrected_rows[[i]] <- vctrs::vec_rbind(!!!lapply(created, make_row))
    }
    
    report_rows[[i]] <- tibble(
      original_openID = orig_openID,
      original_closeID = orig_closeID,
      openTS = openTS_trow,
      closeTS_orig = closeTS_orig,
      action = action,
      reason = reason
    )
  }
  
  # Combine corrected rows ---------------------
  corrected_tbl <- vctrs::vec_rbind(!!!corrected_rows)
  
  # Non-targets ---------------------
  non_targets <- events_sorted %>%
    filter(!(event_key %in% targets$event_key)) %>%
    mutate(is_synthetic = FALSE, source = "unchanged")
  
  # Final table ---------------------
  final_events <- vctrs::vec_rbind(non_targets, corrected_tbl) %>%
    arrange(openTS) %>%
    mutate(
      duration = difftime(closeTS, openTS, units = "mins"),
      event_key = paste0(openID, "__", closeID)
    ) %>%
    select(-any_of(c("original_openID", "next_openTS")))
  
  report_tbl <- vctrs::vec_rbind(!!!report_rows)
  
  if (verbose) {
    message("targets found: ", nrow(targets))
    message("corrected rows created: ", nrow(corrected_tbl))
    message("non-target rows kept: ", nrow(non_targets))
    message("final total rows: ", nrow(final_events))
  }
  
  list(corrected_events = final_events, report = report_tbl)
}



door_events = function(
    chosen_pantry = "BeaconHill", 
    data = data
) {
  
  ### Choosing and Cleaning Data ----------------------------------------------------------
  chosen = chosen_pantry
  data_clean = data %>% filter(device_id == chosen)
  
  data_clean <- data_clean %>% 
    mutate(timestamp = ymd_hms(timestamp, tz = "UTC"),
           timestamp = with_tz(timestamp, "America/Los_Angeles"))
  
  ### Pantry-Specific Logic (BeaconHill) --------------------------------------------------
  if (chosen == "BeaconHill") {
    cutoff <- ymd_hms("2026-01-29 16:26:30", tz = "America/Los_Angeles")
    restart <- ymd_hms("2026-02-24 12:00:00", tz = "America/Los_Angeles")
    
    data_clean <- data_clean %>%
      mutate(scale1 = if_else(timestamp >= cutoff & timestamp <= restart, 0, scale1))
  }
  
  # --- ROBUSTNESS CHECK: Ensure columns exist ---
  # If a door column is missing, create it as FALSE
  if (!"door1_open" %in% names(data_clean)) data_clean$door1_open <- FALSE
  if (!"door2_open" %in% names(data_clean)) data_clean$door2_open <- FALSE
  
  ### Helper to process a specific door ----------------------------------------------------
  process_door <- function(df, door_col_name, group_id) {
    # Extract just the relevant columns
    door_data <- df %>%
      select(id, timestamp, !!sym(door_col_name)) %>%
      rename(is_open = !!sym(door_col_name))
    
    # If no open events ever occur for this door, return an empty structured tibble
    if (sum(door_data$is_open, na.rm = TRUE) == 0) {
      return(tibble(openID=character(), openTS=as.POSIXct(character()), 
                    closeID=character(), closeTS=as.POSIXct(character()), 
                    group=character(), duration=numeric()))
    }
    
    opens <- door_data %>% filter(is_open) %>% select(openID = id, openTS = timestamp) %>% mutate(group = group_id)
    closes <- door_data %>% filter(!is_open) %>% select(closeID = id, closeTS = timestamp) %>% mutate(group = group_id)
    
    res <- opens %>%
      left_join(closes, by = "group", relationship = "many-to-many") %>%
      filter(openTS < closeTS) %>%
      group_by(openTS) %>%
      slice_min(closeTS, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      group_by(closeID) %>%
      slice_min(openTS, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(duration = difftime(closeTS, openTS, unit = "mins"))
    
    return(res)
  }
  
  ### Process both doors -------------------------------------------------------------------
  d1_open_close <- process_door(data_clean, "door1_open", "d1")
  d2_open_close <- process_door(data_clean, "door2_open", "d2")
  
  ### Fix long events ----------------------------------------------------------------------
  # fix_long_door_events already handles empty 'other_events' because of 
  # the 'nrow(d_other) == 0' check inside the loop.
  
  d1_res <- fix_long_door_events(
    d1_open_close, d2_open_close, data_clean, door_col = "door1_open"
  )
  
  d2_res <- fix_long_door_events(
    d2_open_close, d1_open_close, data_clean, door_col = "door2_open"
  )
  
  list(d1_events = d1_res, d2_events = d2_res)
}


pantry_activities <- function(d1_events, d2_events, gap_mins = 1.5) {
  
  # 1. Combine fixed events from both doors
  # We use the results from fix_long_door_events (corrected_events)
  all_intervals <- bind_rows(
    d1_events$corrected_events %>% transmute(type = "d1", start = openTS, end = closeTS, id_open = openID, id_close = closeID),
    d2_events$corrected_events %>% transmute(type = "d2", start = openTS, end = closeTS, id_open = openID, id_close = closeID)
  ) %>% 
    arrange(start)
  
  if (nrow(all_intervals) == 0) return(tibble())
  
  # 2. Vectorized logic to identify new clusters
  # Instead of a for-loop (which is slow in R), we check if the current start 
  # is greater than the running maximum end of previous rows + gap
  processed <- all_intervals %>%
    mutate(
      prev_max_end = lag(cummax(as.numeric(end)), default = 0),
      is_new_cluster = as.numeric(start) > (prev_max_end + (gap_mins * 60)),
      cluster_id = cumsum(is_new_cluster)
    )
  
  # 3. Summarize into activities
  activities <- processed %>%
    group_by(cluster_id) %>%
    summarise(
      activity_start = min(start),
      activity_end = max(end),
      event_count = n(),
      door_types = list(unique(type)),
      open_ids = list(unique(id_open)),
      close_ids = list(unique(id_close)),
      .groups = "drop"
    ) %>%
    mutate(activity_duration = difftime(activity_end, activity_start, units = "mins"))
  
  return(activities)
}

data %>% distinct(device_id)
a = door_events(chosen_pantry = "Greenwood", data = data)

c = pantry_activities(a[["d1_events"]], a[['d2_events']])


# Graphically -----

## events ----
df_steps <- a$d1_events$corrected_events %>%
  select(openTS, closeTS) %>%
  mutate(
    open = 1,
    close = 0
  ) %>%
  pivot_longer(
    cols = c(openTS, closeTS),
    names_to = "event",
    values_to = "timestamp"
  ) %>%
  mutate(
    value = if_else(event == "openTS", 1, 0)
  ) %>%
  arrange(timestamp)

ggplot(df_steps, aes(x = timestamp, y = value)) +
  geom_step(direction = "hv") +
  scale_y_continuous(breaks = c(0, 1)) +
  scale_x_datetime(
    date_labels = "%d/%m",
    date_breaks = "1 day"
  ) +
  labs(
    title = "Door Open/Closed Over Time",
    x = "Date",
    y = "Door State (1 = Open, 0 = Closed)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Activities ----

df_steps2 <- c %>%
  select(cluster_id, activity_start, activity_end) %>%
  mutate(
    start_val = 1,
    end_val   = 0
  ) %>%
  pivot_longer(
    cols = c(activity_start, activity_end),
    names_to = "event",
    values_to = "timestamp"
  ) %>%
  mutate(
    value = if_else(event == "activity_start", 1, 0)
  ) %>%
  arrange(timestamp)

ggplot(df_steps2, aes(x = timestamp, y = value)) +
  geom_step(direction = "hv") +
  scale_y_continuous(breaks = c(0, 1)) +
  scale_x_datetime(
    date_labels = "%d/%m",
    date_breaks = "1 day"
  ) +
  labs(
    title = "Activity Windows Over Time",
    x = "Date",
    y = "Activity State (1 = Active, 0 = Inactive)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )