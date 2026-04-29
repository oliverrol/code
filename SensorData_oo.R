library(DBI)
library(odbc)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(fuzzyjoin)

readRenviron("~/.Renviron")
pantrydb = function () {
  now = today()

  driver <- Sys.getenv("SQL_DRIVER", unset = NA)
  if (is.na(driver) || driver == "") {
    driver <- if (Sys.info()[["sysname"]] == "Darwin") {
      "/opt/homebrew/lib/libmsodbcsql.18.dylib"
    } else {
      "ODBC Driver 18 for SQL Server"
    }
  }

  con <- dbConnect(
    odbc(),
    Driver                 = driver,
    Server                 = Sys.getenv("SQL_SERVER"),
    Database               = Sys.getenv("SQL_DATABASE"),
    UID                    = Sys.getenv("SQL_USER"),
    PWD                    = Sys.getenv("SQL_PASSWORD"),
    Encrypt                = "yes",
    TrustServerCertificate = "yes"
  )
  on.exit(dbDisconnect(con))

  data <- dbGetQuery(con, "SELECT * FROM PantryLogs")

  write.csv(data, paste0("../data/data_", now, ".csv"), row.names = FALSE)
  return(data)
}





