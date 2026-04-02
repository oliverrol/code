library(DBI)
library(odbc)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(fuzzyjoin)


pantrydb = function () {
  now = today()
  con <- dbConnect(
    odbc(),
    Driver   = Sys.getenv("SQL_DRIVER"),
    Server   = Sys.getenv("SQL_SERVER"),
    Database = Sys.getenv("SQL_DATABASE"),
    UID      = Sys.getenv("SQL_USER"),
    PWD      = Sys.getenv("SQL_PASSWORD"),
    Encrypt  = "yes"
  )
  
  data <- dbGetQuery(con, "SELECT * FROM PantryLogs")
  
  #writing all the table to the /data folder
  write.csv(data, paste("../data/data_",now,".csv",sep = "" ))
  return(data)
}





