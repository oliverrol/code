library(dplyr)
library(purrr)

source("SensorData_oo.R")
source("event_estimate.R")

data = pantrydb()

events = door_events(chosen_pantry = "BeaconHill", data = data)

activities = pantry_activities(d1_events = events$d1_events, 
                               d2_events = events$d2_events,
                               gap_mins = 1.5)

# Figuring out the weight changes -----

safe_lookup <- function(id, vec_id, vec_value) {
  if (is.na(id)) return(NA_real_)
  out <- vec_value[vec_id == id]
  if (length(out) == 1) return(out)
  return(NA_real_)
}

activities <- activities %>%
  rowwise() %>%
  mutate(
    start_id = first(open_ids),
    end_id   = last(close_ids),
    
    start_id_clean = if (grepl("(_A|_B)$", start_id)) NA else start_id,
    end_id_clean   = if (grepl("(_A|_B)$", end_id))   NA else end_id,
    
    start_sensor1 = safe_lookup(start_id_clean, data$id, data$scale1),
    end_sensor1   = safe_lookup(end_id_clean,   data$id, data$scale1),
    
    start_sensor2 = safe_lookup(start_id_clean, data$id, data$scale2),
    end_sensor2   = safe_lookup(end_id_clean,   data$id, data$scale2),
    
    start_sensor3 = safe_lookup(start_id_clean, data$id, data$scale3),
    end_sensor3   = safe_lookup(end_id_clean,   data$id, data$scale3),
    
    start_sensor4 = safe_lookup(start_id_clean, data$id, data$scale4),
    end_sensor4   = safe_lookup(end_id_clean,   data$id, data$scale4)
  ) %>%
  ungroup()


activities = activities %>% mutate(
  previous_sensor1 = lag(end_sensor1),
  previous_sensor2 = lag(end_sensor2),
  previous_sensor3 = lag(end_sensor3),
  previous_sensor4 = lag(end_sensor4),
  drift_sensor1 = previous_sensor1 - start_sensor1,
  drift_sensor2 = previous_sensor2 - start_sensor2,
  drift_sensor3 = previous_sensor3 - start_sensor3,
  drift_sensor4 = previous_sensor4 - start_sensor4
)
