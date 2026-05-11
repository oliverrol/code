library(dplyr)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)
library(tidyr)


### Loading the data -------------------------------------------------------------------------------
source('SensorData_oo.R')
# data = pantrydb()

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
    events       <- events       |>mutate(openTS = with_tz(openTS, tz), closeTS = with_tz(closeTS, tz))
    other_events <- other_events |>mutate(openTS = with_tz(openTS, tz), closeTS = with_tz(closeTS, tz))
    raw_logs     <- raw_logs     |>mutate(timestamp = with_tz(timestamp, tz))
  }
  
  events       <- events       |>mutate(openTS = as.POSIXct(openTS), closeTS = as.POSIXct(closeTS))
  other_events <- other_events |>mutate(openTS = as.POSIXct(openTS), closeTS = as.POSIXct(closeTS))
  raw_logs     <- raw_logs     |>mutate(timestamp = as.POSIXct(timestamp))
  

  # Coerce IDs to character ---------------------
  events       <- events       |>mutate(openID = as.character(openID), closeID = as.character(closeID))
  other_events <- other_events |>mutate(openID = as.character(openID), closeID = as.character(closeID))
  
  # Compute durations ---------------------
  events <- events |>mutate(duration = difftime(closeTS, openTS, units = "mins"))
  

  # Temporary event key ---------------------
  events <- events |>mutate(event_key = paste0(openID, "__", closeID))
  

  # Precompute next real openTS ---------------------
  events_sorted <- events |>arrange(openTS)
  events_sorted <- events_sorted |>mutate(next_openTS = lead(openTS))
  

  # Identify targets ---------------------
  targets <- events_sorted |>filter(as.numeric(duration) > max_duration_mins)
  
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
    d_other <- other_events |>
      filter(closeTS > closeTS_orig) |>
      arrange(closeTS) |>
      slice(1)
    
    d_other_time <- if (nrow(d_other) == 0) NA else d_other$closeTS
    
    if (!is.na(d_other_time)) {
      if (difftime(d_other_time, openTS_trow, units = "mins") > other_door_max_mins)
        d_other_time <- NA
    }
    
    # Candidate from raw logs ---------------------
    repeating_time <- raw_logs |>
      filter(timestamp > openTS_trow + 1, !is_event) |>
      arrange(timestamp) |>
      slice(1) |>
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
        trow |>mutate(is_synthetic = FALSE, source = "unchanged")
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
  non_targets <- events_sorted |>
    filter(!(event_key %in% targets$event_key)) |>
    mutate(is_synthetic = FALSE, source = "unchanged")
  
  # Final table ---------------------
  final_events <- vctrs::vec_rbind(non_targets, corrected_tbl) |>
    arrange(openTS) |>
    mutate(
      duration = difftime(closeTS, openTS, units = "mins"),
      event_key = paste0(openID, "__", closeID)
    ) |>
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
  data_clean = data
  
  data_clean <- data_clean |>
    mutate(timestamp = ymd_hms(timestamp, tz = "UTC"),
           timestamp = with_tz(timestamp, "America/Los_Angeles"))
  
  ### Pantry-Specific Logic (BeaconHill) --------------------------------------------------
  if (chosen == "BeaconHill") {
    cutoff <- ymd_hms("2026-01-29 16:26:30", tz = "America/Los_Angeles")
    restart <- ymd_hms("2026-02-24 12:00:00", tz = "America/Los_Angeles")
    
    data_clean <- data_clean |>
      mutate(scale1 = if_else(timestamp >= cutoff & timestamp <= restart, 0, scale1))
  }
  
  # --- ROBUSTNESS CHECK: Ensure columns exist ---
  # If a door column is missing, create it as FALSE
  if (!"door1_open" %in% names(data_clean)) data_clean$door1_open <- FALSE
  if (!"door2_open" %in% names(data_clean)) data_clean$door2_open <- FALSE
  
  ### Helper to process a specific door ----------------------------------------------------
  process_door <- function(df, door_col_name, group_id) {
    # Extract just the relevant columns
    door_data <- df |>
      select(id, timestamp, !!sym(door_col_name)) |>
      rename(is_open = !!sym(door_col_name))
    
    # If no open events ever occur for this door, return an empty structured tibble
    if (sum(door_data$is_open, na.rm = TRUE) == 0) {
      return(tibble(openID=character(), openTS=as.POSIXct(character()), 
                    closeID=character(), closeTS=as.POSIXct(character()), 
                    group=character(), duration=numeric()))
    }
    
    opens <- door_data |>filter(is_open) |>select(openID = id, openTS = timestamp) |>mutate(group = group_id)
    closes <- door_data |>filter(!is_open) |>select(closeID = id, closeTS = timestamp) |>mutate(group = group_id)
    
    res <- opens |>
      left_join(closes, by = "group", relationship = "many-to-many") |>
      filter(openTS < closeTS) |>
      group_by(openTS) |>
      slice_min(closeTS, n = 1, with_ties = FALSE) |>
      ungroup() |>
      group_by(closeID) |>
      slice_min(openTS, n = 1, with_ties = FALSE) |>
      ungroup() |>
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
    d1_events$corrected_events |>transmute(type = "d1", start = openTS, end = closeTS, id_open = openID, id_close = closeID),
    d2_events$corrected_events |>transmute(type = "d2", start = openTS, end = closeTS, id_open = openID, id_close = closeID)
  ) |>
    arrange(start)
  
  if (nrow(all_intervals) == 0) return(tibble())
  
  # 2. Vectorized logic to identify new clusters
  # Instead of a for-loop (which is slow in R), we check if the current start 
  # is greater than the running maximum end of previous rows + gap
  processed <- all_intervals |>
    mutate(
      prev_max_end = lag(cummax(as.numeric(end)), default = 0),
      is_new_cluster = as.numeric(start) > (prev_max_end + (gap_mins * 60)),
      cluster_id = cumsum(is_new_cluster)
    )
  
  # 3. Summarize into activities
  activities <- processed |>
    group_by(cluster_id) |>
    summarise(
      activity_start = min(start),
      activity_end = max(end),
      event_count = n(),
      door_types = list(unique(type)),
      open_ids = list(unique(id_open)),
      close_ids = list(unique(id_close)),
      .groups = "drop"
    ) |>
    mutate(activity_duration = difftime(activity_end, activity_start, units = "mins"))
  
  return(activities)
}

# # data |>distinct(device_id)
# # a = door_events(chosen_pantry = "Greenwood", data = data)
# # 
# # c = pantry_activities(a[["d1_events"]], a[['d2_events']])
# 
# 
# # Graphically -----
# 
# ## events ----
# # df_steps <- a$d1_events$corrected_events |>
# #   select(openTS, closeTS) |>
# #   mutate(
# #     open = 1,
# #     close = 0
# #   ) |>
# #   pivot_longer(
# #     cols = c(openTS, closeTS),
# #     names_to = "event",
# #     values_to = "timestamp"
# #   ) |>
# #   mutate(
# #     value = if_else(event == "openTS", 1, 0)
# #   ) |>
# #   arrange(timestamp)
# 
# ggplot(df_steps, aes(x = timestamp, y = value)) +
#   geom_step(direction = "hv") +
#   scale_y_continuous(breaks = c(0, 1)) +
#   scale_x_datetime(
#     date_labels = "%d/%m",
#     date_breaks = "1 day"
#   ) +
#   labs(
#     title = "Door Open/Closed Over Time",
#     x = "Date",
#     y = "Door State (1 = Open, 0 = Closed)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
# 
# ## Activities ----
# 
# # df_steps2 <- c |>
# #   select(cluster_id, activity_start, activity_end) |>
# #   mutate(
# #     start_val = 1,
# #     end_val   = 0
# #   ) |>
# #   pivot_longer(
# #     cols = c(activity_start, activity_end),
# #     names_to = "event",
# #     values_to = "timestamp"
# #   ) |>
# #   mutate(
# #     value = if_else(event == "activity_start", 1, 0)
# #   ) |>
# #   arrange(timestamp)
# 
# ggplot(df_steps2, aes(x = timestamp, y = value)) +
#   geom_step(direction = "hv") +
#   scale_y_continuous(breaks = c(0, 1)) +
#   scale_x_datetime(
#     date_labels = "%d/%m",
#     date_breaks = "1 day"
#   ) +
#   labs(
#     title = "Activity Windows Over Time",
#     x = "Date",
#     y = "Activity State (1 = Active, 0 = Inactive)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )




# Figuring out the weight changes -----

# ============================================================================
# COMPREHENSIVE EXAMPLE: All 4 Cases with Weight Tracking
# ============================================================================

library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

# ============================================================================
# HELPER FUNCTION: Get closest sensor reading
# ============================================================================

get_closest_reading <- function(timestamp, logs, direction = "before") {
  "
  direction: before = find the most recent reading <= timestamp
             after  = find the next reading >= timestamp
  "
  
  if (direction == "before") {
    reading <- logs |>
      filter(timestamp <= !!timestamp) |>
      arrange(desc(timestamp)) |>
      slice(1)
  } else {
    reading <- logs |>
      filter(timestamp >= !!timestamp) |>
      arrange(timestamp) |>
      slice(1)
  }
  
  # Return NA list if no reading found
  if (nrow(reading) == 0) {
    return(tibble(
      scale1 = NA_real_,
      scale2 = NA_real_,
      scale3 = NA_real_,
      scale4 = NA_real_,
      timestamp = NA_POSIXct_
    ))
  }
  
  reading |>
    select(scale1, scale2, scale3, scale4, timestamp) |>
    slice(1)
}

# ============================================================================
# MAIN FUNCTION: Activities with weight tracking
# ============================================================================

activities_with_weights <- function(activities, d1_events, d2_events, raw_logs) {
  
  
  # Negative scale readings mean the scale is empty (sensor drift below zero). Clamp to 0.
  raw_logs = raw_logs |>
    mutate(timestamp = ymd_hms(timestamp, tz = "UTC"),
           timestamp = with_tz(timestamp, "America/Los_Angeles"),
           across(c(scale1, scale2, scale3, scale4), ~ pmax(.x, 0)))
  # 1. Combine corrected events from both doors
  all_corrected_events <- bind_rows(
    d1_events$corrected_events |>mutate(door = "d1"),
    d2_events$corrected_events |>mutate(door = "d2")
  )
  
  # 2. Unnest IDs and ensure they are characters for the join
  activities_expanded= activities |>
    unnest(open_ids) |>
    unnest(close_ids) |>
    mutate(
      open_ids = as.character(open_ids),
      close_ids = as.character(close_ids)
    )
  
  # 3. Join with corrected events
  activities_with_events <- activities_expanded |>
    left_join(
      all_corrected_events |>select(openID, openTS, closeID, closeTS, source, door),
      by = c("open_ids" = "openID", "close_ids" = "closeID")
    )
  
  # 4. Summarize by activity (cluster_id)
  activities_with_readings <- activities_with_events |>
    group_by(cluster_id) |>
    summarise(
      activity_start = first(activity_start),
      activity_end = first(activity_end),
      activity_duration = first(activity_duration),
      event_count = first(event_count),
      
      # WRAP IN list() TO HANDLE THE VECTOR INSIDE THE CELL
      door_types = list(first(door_types)), 
      
      case_split_partA = sum(source == "split_partA", na.rm = TRUE),
      case_split_partB = sum(source == "split_partB", na.rm = TRUE),
      case_replaced_closing = sum(source == "replaced_closing", na.rm = TRUE),
      case_unchanged = sum(source == "unchanged", na.rm = TRUE),
      .groups = "drop"
    ) |>
    # 5. Fetch readings
    mutate(
      start_reading = map(activity_start, ~get_closest_reading(.x, raw_logs, "before")),
      end_reading = map(activity_end, ~get_closest_reading(.x, raw_logs, "after"))
    ) |>
    # 6. Unnest with simplified names
    # We use names_sep = "_" so the columns become start_reading_scale1, etc.
    unnest(start_reading, names_sep = "_") |>
    unnest(end_reading, names_sep = "_")
  
  # 7. Final Calculation using the corrected column names
  activities_summary <- activities_with_readings |>
    mutate(
      case_synthetic_events = case_split_partA + case_split_partB + case_replaced_closing,
      
      # Note the new column names: prefix_originalName
      delta_scale1 = end_reading_scale1 - start_reading_scale1,
      delta_scale2 = end_reading_scale2 - start_reading_scale2,
      delta_scale3 = end_reading_scale3 - start_reading_scale3,
      delta_scale4 = end_reading_scale4 - start_reading_scale4,
      
      total_weight_change = delta_scale1 + delta_scale2 + delta_scale3 + delta_scale4
    )
  
  return(activities_summary)
}