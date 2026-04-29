
rm(list=ls())

library(DBI)
library(odbc)
library(dplyr)


## Connect to Azure SQL

con <- dbConnect(
  odbc(),
  Driver = "/opt/homebrew/lib/libmsodbcsql.18.dylib",  # ARM driver detected
  Server = "micropantry-sql-server.database.windows.net",
  Database = "pantry-sql",
  UID = "StudentUser",
  PWD = "StudentPassword123!",
  Port = 1433,
  Encrypt = "yes",
  TrustServerCertificate = "yes"
)

## Which devices (pantries) are contained in server:
devices <- dbGetQuery(con, "
  SELECT DISTINCT device_id
  FROM dbo.PantryLogs
") %>% pull(device_id)
devices # display devices

## Select devices
sel_devices <- c("Greenwood","BeaconHill","StPaulChurchPantry","HallerLakePantry")

all_data <- list()

for(dev in sel_devices){
  cat("Fetching data for device:", dev, "...\n")
  
  query <- paste0("
    SELECT *
    FROM dbo.PantryLogs
    WHERE device_id = '", dev, "'
    ORDER BY timestamp
  ")
  
  device_data <- dbGetQuery(con, query)
  
  # Save CSV for this device
  #csv_file <- paste0(dev, "_pantry_data.csv")
  #write.csv(device_data, csv_file, row.names = FALSE)
  #cat("Saved:", csv_file, "\n")
  
  # Optionally store in list
  all_data[[dev]] <- device_data
}

## Disconnect from server
dbDisconnect(con)

## Combine in one dataset
act <- bind_rows(all_data)



### PROCESS DATA ----------------------


## Convert timestamp from UTC to Seattle time
act$timestamp <- as.POSIXct(format(act$timestamp, tz = "America/Los_Angeles", usetz = TRUE), tz = "America/Los_Angeles")


## Check latest logged data
green <- act[act$device_id=="Greenwood" & !is.na(act$timestamp),c("device_id","timestamp","air_temp","door1_open","door2_open","batt_percent","scale1","scale2","scale3","scale4")]
paul <- act[act$device_id=="StPaulChurchPantry" & !is.na(act$timestamp),c("device_id","timestamp","air_temp","door1_open","door2_open","batt_percent","scale1","scale2","scale3","scale4")]
beacon <- act[act$device_id=="BeaconHill" & !is.na(act$timestamp),c("device_id","timestamp","air_temp","door1_open","door2_open","batt_percent","scale1","scale2","scale3","scale4")]
haller <- act[act$device_id=="HallerLakePantry" & !is.na(act$timestamp),c("device_id","timestamp","air_temp","door1_open","door2_open","batt_percent","scale1","scale2","scale3","scale4")]

green[order(green$timestamp, decreasing = T),][1:80,]
paul[order(paul$timestamp, decreasing = T),][1:80,]
beacon[order(beacon$timestamp, decreasing = T),][1:80,]
haller[order(haller$timestamp, decreasing = T),][1:80,]

### SAVE DATA ----------------------

write.csv(all_combined, "all_pantries_data.csv", row.names = FALSE)













