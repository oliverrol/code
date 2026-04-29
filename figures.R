library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(tidyr)
library(scales)

source("SensorData_oo.R")
source("event_estimate.R")

sel_pantries <- c("Greenwood", "BeaconHill", "StPaulChurchPantry", "HallerLakePantry")

pantry_labels <- c(
  Greenwood           = "Greenwood",
  BeaconHill          = "Beacon Hill",
  StPaulChurchPantry  = "St. Paul Church",
  HallerLakePantry    = "Haller Lake"
)

palette <- c(
  Greenwood           = "#2166ac",
  BeaconHill          = "#4dac26",
  StPaulChurchPantry  = "#d6604d",
  HallerLakePantry    = "#8073ac"
)

base_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = "grey30"),
    plot.title         = element_text(face = "bold", size = 14, margin = margin(b = 8)),
    plot.subtitle      = element_text(color = "grey40", size = 11, margin = margin(b = 12))
  )

# =============================================================================
# Data: fetch and process
# =============================================================================

all_data <- pantrydb()

all_activities <- map_dfr(sel_pantries, function(pantry) {
  data <- all_data |> filter(device_id == pantry)

  if (nrow(data) == 0) {
    warning("No data found for '", pantry, "' — skipping.")
    return(NULL)
  }

  events <- tryCatch(
    door_events(chosen_pantry = pantry, data = data),
    error = function(e) {
      warning("door_events() failed for '", pantry, "': ", e$message, " — skipping.")
      NULL
    }
  )
  if (is.null(events)) return(NULL)

  if (nrow(events$d1_events$corrected_events) == 0 &&
      nrow(events$d2_events$corrected_events) == 0) {
    warning("No door events for '", pantry, "' — skipping.")
    return(NULL)
  }

  activities <- tryCatch(
    pantry_activities(d1_events = events$d1_events, d2_events = events$d2_events, gap_mins = 1.5),
    error = function(e) {
      warning("pantry_activities() failed for '", pantry, "': ", e$message, " — skipping.")
      NULL
    }
  )
  if (is.null(activities) || nrow(activities) == 0) {
    warning("No activities for '", pantry, "' — skipping.")
    return(NULL)
  }

  activities |> mutate(pantry = pantry)
})

# =============================================================================
# Compute summary metrics per pantry
# =============================================================================

today_date <- today()

raw_metrics <- all_data |>
  mutate(date = as_date(timestamp)) |>
  group_by(pantry = device_id) |>
  summarise(
    first_log_date = min(date, na.rm = TRUE),
    active_days    = n_distinct(date),
    .groups        = "drop"
  ) |>
  filter(pantry %in% sel_pantries) |>
  mutate(
    total_days_in_range = as.integer(today_date - first_log_date) + 1,
    non_active_days     = total_days_in_range - active_days,
    pct_non_active      = non_active_days / total_days_in_range
  )

activity_metrics <- all_activities |>
  count(pantry, name = "total_activities")

summary <- raw_metrics |>
  left_join(activity_metrics, by = "pantry") |>
  mutate(
    total_activities       = coalesce(total_activities, 0L),
    activities_per_active_day = if_else(
      active_days > 0,
      total_activities / active_days,
      NA_real_
    ),
    pantry_label = pantry_labels[pantry],
    pantry_label = factor(pantry_label, levels = pantry_labels)
  )

# =============================================================================
# Chart 1: Absolute activity count per pantry
# =============================================================================

fig1 <- ggplot(summary, aes(x = pantry_label, y = total_activities, fill = pantry)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = total_activities), vjust = -0.5, size = 4, color = "grey30") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Total Pantry Activities",
    subtitle = "Count of detected visit events per pantry",
    x        = NULL,
    y        = "Number of Activities"
  ) +
  base_theme

print(fig1)

# =============================================================================
# Chart 2: Activities per active day
# =============================================================================

fig2 <- ggplot(summary, aes(x = pantry_label, y = activities_per_active_day, fill = pantry)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = number(activities_per_active_day, accuracy = 0.01)),
    vjust = -0.5, size = 4, color = "grey30"
  ) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Activities per Active Day",
    subtitle = "Normalised by days with at least one sensor log",
    x        = NULL,
    y        = "Activities / Active Day"
  ) +
  base_theme

print(fig2)

# =============================================================================
# Chart 3: Non-active days per pantry
# =============================================================================

fig3 <- ggplot(summary, aes(x = pantry_label, y = non_active_days, fill = pantry)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(non_active_days, "\n(", percent(pct_non_active, accuracy = 1), ")")),
    vjust = -0.3, size = 3.5, color = "grey30", lineheight = 1.1
  ) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Non-Active Days per Pantry",
    subtitle = "Days with no sensor logs since first recorded reading",
    x        = NULL,
    y        = "Number of Days"
  ) +
  base_theme

print(fig3)

# =============================================================================
# Time-feature data prep
# =============================================================================

acts <- all_activities |>
  mutate(
    day_of_week  = wday(activity_start, label = TRUE, abbr = FALSE, week_start = 1),
    hour_of_day  = hour(activity_start),
    day_of_month = mday(activity_start),
    pantry_label = factor(pantry_labels[pantry], levels = pantry_labels)
  )

facet_theme <- base_theme +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey94", color = NA),
    panel.spacing    = unit(1, "lines")
  )

# =============================================================================
# Charts 4 & 5: Day of week
# =============================================================================

dow_all <- acts |> count(day_of_week)

fig4 <- ggplot(dow_all, aes(x = day_of_week, y = n)) +
  geom_col(fill = "#2166ac", width = 0.65) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5, color = "grey30") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Activities by Day of Week — All Pantries",
    subtitle = "Combined across Greenwood, Beacon Hill, St. Paul Church, Haller Lake",
    x        = NULL,
    y        = "Number of Activities"
  ) +
  base_theme

print(fig4)

dow_per <- acts |> count(pantry_label, day_of_week)

fig5 <- ggplot(dow_per, aes(x = day_of_week, y = n, fill = pantry_label)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3, color = "grey30") +
  scale_fill_manual(values = setNames(palette, pantry_labels)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~pantry_label, ncol = 2, scales = "free_y") +
  labs(
    title    = "Activities by Day of Week — Per Pantry",
    subtitle = "Y-axis free to highlight within-pantry patterns",
    x        = NULL,
    y        = "Number of Activities"
  ) +
  facet_theme

print(fig5)

# =============================================================================
# Charts 6 & 7: Time of day
# =============================================================================

tod_all <- acts |> count(hour_of_day)

fig6 <- ggplot(tod_all, aes(x = hour_of_day, y = n)) +
  geom_col(fill = "#2166ac", width = 0.85) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d:00", seq(0, 23, 2))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Activities by Time of Day — All Pantries",
    subtitle = "Hour of activity start (local time)",
    x        = NULL,
    y        = "Number of Activities"
  ) +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fig6)

tod_per <- acts |> count(pantry_label, hour_of_day)

fig7 <- ggplot(tod_per, aes(x = hour_of_day, y = n, fill = pantry_label)) +
  geom_col(width = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = setNames(palette, pantry_labels)) +
  scale_x_continuous(breaks = seq(0, 23, 4), labels = sprintf("%02d:00", seq(0, 23, 4))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~pantry_label, ncol = 2, scales = "free_y") +
  labs(
    title    = "Activities by Time of Day — Per Pantry",
    subtitle = "Y-axis free to highlight within-pantry patterns",
    x        = NULL,
    y        = "Number of Activities"
  ) +
  facet_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fig7)

# =============================================================================
# Charts 8 & 9: Day of month
# =============================================================================

dom_all <- acts |> count(day_of_month)

fig8 <- ggplot(dom_all, aes(x = day_of_month, y = n)) +
  geom_col(fill = "#2166ac", width = 0.8) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 31)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Activities by Day of Month — All Pantries",
    subtitle = "Combined across all pantries",
    x        = "Day of Month",
    y        = "Number of Activities"
  ) +
  base_theme

print(fig8)

dom_per <- acts |> count(pantry_label, day_of_month)

fig9 <- ggplot(dom_per, aes(x = day_of_month, y = n, fill = pantry_label)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = setNames(palette, pantry_labels)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 31)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~pantry_label, ncol = 2, scales = "free_y") +
  labs(
    title    = "Activities by Day of Month — Per Pantry",
    subtitle = "Y-axis free to highlight within-pantry patterns",
    x        = "Day of Month",
    y        = "Number of Activities"
  ) +
  facet_theme

print(fig9)

# =============================================================================
# Weight data: fetch total_weight_change per activity for all pantries
# =============================================================================

all_activities_weighted <- map_dfr(sel_pantries, function(pantry) {
  data <- all_data |> filter(device_id == pantry)
  if (nrow(data) == 0) return(NULL)

  events <- tryCatch(
    door_events(chosen_pantry = pantry, data = data),
    error = function(e) NULL
  )
  if (is.null(events)) return(NULL)

  activities <- tryCatch(
    pantry_activities(d1_events = events$d1_events, d2_events = events$d2_events, gap_mins = 1.5),
    error = function(e) NULL
  )
  if (is.null(activities) || nrow(activities) == 0) return(NULL)

  weighted <- tryCatch(
    activities_with_weights(activities, events$d1_events, events$d2_events, data),
    error = function(e) NULL
  )
  if (is.null(weighted)) return(NULL)

  weighted |> mutate(pantry = pantry)
})

acts_typed <- all_activities_weighted |>
  filter(!is.na(total_weight_change)) |>
  mutate(
    activity_type = case_when(
      total_weight_change > 0 ~ "Donation",
      total_weight_change < 0 ~ "Consumption",
      TRUE                    ~ "No Change"
    ),
    activity_type = factor(activity_type, levels = c("Donation", "Consumption", "No Change")),
    pantry_label  = factor(pantry_labels[pantry], levels = pantry_labels)
  )

type_palette <- c(Donation = "#4dac26", Consumption = "#d6604d", `No Change` = "#bababa")

# =============================================================================
# Chart 10: Donation vs Consumption — All Pantries
# =============================================================================

type_all <- acts_typed |> count(activity_type)

fig10 <- ggplot(type_all, aes(x = activity_type, y = n, fill = activity_type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 4, color = "grey30") +
  scale_fill_manual(values = type_palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Activities by Type — All Pantries",
    subtitle = "Donation = net weight gained; Consumption = net weight lost",
    x        = NULL,
    y        = "Number of Activities"
  ) +
  base_theme

print(fig10)

# =============================================================================
# Chart 11: Donation vs Consumption — Per Pantry
# =============================================================================

type_per <- acts_typed |> count(pantry_label, activity_type)

fig11 <- ggplot(type_per, aes(x = activity_type, y = n, fill = activity_type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3, color = "grey30") +
  scale_fill_manual(values = type_palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~pantry_label, ncol = 2, scales = "free_y") +
  labs(
    title    = "Activities by Type — Per Pantry",
    subtitle = "Donation = net weight gained; Consumption = net weight lost",
    x        = NULL,
    y        = "Number of Activities"
  ) +
  facet_theme

print(fig11)

# =============================================================================
# Chart 12: Average weight per activity type — All Pantries
# =============================================================================

avg_weight_all <- acts_typed |>
  filter(activity_type != "No Change") |>
  group_by(activity_type) |>
  summarise(avg_lbs = mean(abs(total_weight_change)), .groups = "drop")

fig12 <- ggplot(avg_weight_all, aes(x = activity_type, y = avg_lbs, fill = activity_type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = number(avg_lbs, accuracy = 0.1, suffix = " lbs")),
    vjust = -0.4, size = 4, color = "grey30"
  ) +
  scale_fill_manual(values = type_palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Average Weight Change per Activity — All Pantries",
    subtitle = "Mean absolute weight change (lbs) for donations and consumptions",
    x        = NULL,
    y        = "Average Weight Change (lbs)"
  ) +
  base_theme

print(fig12)

# =============================================================================
# Chart 13: Average weight per activity type — Per Pantry
# =============================================================================

avg_weight_per <- acts_typed |>
  filter(activity_type != "No Change") |>
  group_by(pantry_label, activity_type) |>
  summarise(avg_lbs = mean(abs(total_weight_change)), .groups = "drop")

fig13 <- ggplot(avg_weight_per, aes(x = activity_type, y = avg_lbs, fill = activity_type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = number(avg_lbs, accuracy = 0.1, suffix = " lbs")),
    vjust = -0.4, size = 3, color = "grey30"
  ) +
  scale_fill_manual(values = type_palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~pantry_label, ncol = 2, scales = "free_y") +
  labs(
    title    = "Average Weight Change per Activity — Per Pantry",
    subtitle = "Mean absolute weight change (lbs) for donations and consumptions",
    x        = NULL,
    y        = "Average Weight Change (lbs)"
  ) +
  facet_theme

print(fig13)
