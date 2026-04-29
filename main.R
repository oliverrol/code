library(dplyr)
library(purrr)

source("SensorData_oo.R")
source("event_estimate.R")

pantry = "BeaconHill"

data = pantrydb()
data = data |> filter(device_id == pantry)
if (nrow(data) == 0) stop("No data found for pantry '", pantry, "'. Check the pantry name and DB connection.")

events = door_events(chosen_pantry = pantry, data = data)
if (nrow(events$d1_events$corrected_events) == 0 && nrow(events$d2_events$corrected_events) == 0)
  stop("No door events found for pantry '", pantry, "'. The data may be missing door sensor readings.")

activities = pantry_activities(d1_events = events$d1_events,
                               d2_events = events$d2_events,
                               gap_mins = 1.5)
if (nrow(activities) == 0) stop("No activities found for pantry '", pantry, "'. Check door event results.")

processed = activities_with_weights(activities, events$d1_events, events$d2_events, data)
processed = processed |> mutate(
  total_weight_change = round(total_weight_change, 2),
  start_weight = start_reading_scale1 + start_reading_scale2 + start_reading_scale3 + start_reading_scale4,
  end_weight = end_reading_scale1 + end_reading_scale2 + end_reading_scale3 + end_reading_scale4
)
processed = processed |> mutate(
  total_weight_change_abs = abs(total_weight_change)
)
