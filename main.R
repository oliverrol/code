library(dplyr)
library(purrr)

source("SensorData_oo.R")
source("event_estimate.R")

pantry = "BeaconHill"
#Get all the data
data = pantrydb()
data = data %>% filter(device_id == pantry)

#Choose the pantry, clean the events and get the activities
events = door_events(chosen_pantry = pantry, data = data)

activities = pantry_activities(d1_events = events$d1_events, 
                               d2_events = events$d2_events,
                               gap_mins = 1.5)

processed = activities_with_weights(activities, events$d1_events, events$d2_events, data)