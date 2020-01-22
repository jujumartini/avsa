# Ctrl + Shift + F10 to restart session
# Ctrl + A to Select all

# load packages
library(tidyverse)
library(lubridate)
library(padr)
library(equivalence)
library(irr)
library(lme4)
library(here)

# check to see any tidyverse conflicts
tidyverse_conflicts()

# check working directory. If not using an Rproject, use the here function 
# before each path
here()

# read in on off log and clean
on_off_log <- read.table(file = "./3_data/raw/visit_on_off_log.csv",
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

on_off_log$date_on <- paste(on_off_log$date_on_month,
                     on_off_log$date_on_day,
                     on_off_log$date_on_year,
                     sep="/")
on_off_log$time_on <- paste(on_off_log$time_on_hour,
                     on_off_log$time_on_minute,
                     on_off_log$time_on_seconds,
                     sep=":")
on_off_log$date_off <- paste(on_off_log$date_off_month,
                      on_off_log$date_off_day,
                      on_off_log$date_off_year,
                      sep="/")
on_off_log$time_off <- paste(on_off_log$time_off_hour,
                      on_off_log$time_off_minute,
                      on_off_log$time_off_seconds,
                      sep=":")
on_off_log$date_time_on <- paste(on_off_log$date_on,
                          on_off_log$time_on,
                          sep=" ")
on_off_log$date_time_off <- paste(on_off_log$date_off,
                           on_off_log$time_off,
                           sep=" ")
on_off_log$date_time_on <- strptime(on_off_log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
on_off_log$date_time_off <- strptime(on_off_log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
on_off_log$date_time_on <- force_tz(on_off_log$date_time_on,
                             tz = "America/Chicago")
on_off_log$date_time_off <- force_tz(on_off_log$date_time_off,
                              tz = "America/Chicago")
