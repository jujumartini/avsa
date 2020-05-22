
# read_on_off_log - Test 2 ------------------------------------------------

read_on_off_log <- function(path,
                            name_log) {
  
  # path <- "./3_data/raw/"
  # name_log <- "visit_on_off_log.csv"
  
  on_off_raw <- suppressMessages(vroom(file = paste0(path,
                                                     name_log),
                                       delim = ","))
  
  colnames(on_off_raw) <- str_replace_all(colnames(on_off_raw),
                                          pattern = "\\.",
                                          replacement = "_")
  
  on_off_raw$date_time_on <- 
    make_datetime(
      year  = on_off_raw$date_on_year,
      month = on_off_raw$date_on_month,
      day   = on_off_raw$date_on_day,
      hour  = on_off_raw$time_on_hour,
      min   = on_off_raw$time_on_minute,
      sec   = on_off_raw$time_on_seconds,
      tz    = "America/Chicago"
    )
  on_off_raw$date_time_off <- 
    make_datetime(
      year  = on_off_raw$date_off_year,
      month = on_off_raw$date_off_month,
      day   = on_off_raw$date_off_day,
      hour  = on_off_raw$time_off_hour,
      min   = on_off_raw$time_off_minute,
      sec   = on_off_raw$time_off_seconds,
      tz    = "America/Chicago"
    )
  
  assign("log_on_off",
         on_off_raw,
         envir = .GlobalEnv)

}

# process_anno - Test 1 --------------------------------------------------------

corr_timstamps_path <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv"
on_off_log <- "visit_on_off_log.csv"

test = "FLAC_1002V3_POSTURE_CHANG.CSV"

# read in timestamps csv and change to times
corr_times <- read.csv(file = corr_timstamps_path)
corr_times$StopWatch_YMD_HMS <- ymd_hms(corr_times$StopWatch_YMD_HMS, 
                                        tz="America/Chicago")
corr_times$Corr_Picture_YMD_HMS <- ymd_hms(corr_times$Corr_Picture_YMD_HMS, 
                                           tz="America/Chicago")

# diff col
corr_times$Difference <- NA
corr_times$Difference <- with(corr_times,
                              difftime(StopWatch_YMD_HMS,
                                       Corr_Picture_YMD_HMS,
                                       units = "secs"))

# read in on off log and clean
log <- read.table(file = paste0("./3_data/raw/", on_off_log),
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

log$date_on <- paste(log$date_on_month,
                     log$date_on_day,
                     log$date_on_year,
                     sep="/")
log$time_on <- paste(log$time_on_hour,
                     log$time_on_minute,
                     log$time_on_seconds,
                     sep=":")
log$date_off <- paste(log$date_off_month,
                      log$date_off_day,
                      log$date_off_year,
                      sep="/")
log$time_off <- paste(log$time_off_hour,
                      log$time_off_minute,
                      log$time_off_seconds,
                      sep=":")
log$date_time_on <- paste(log$date_on,
                          log$time_on,
                          sep=" ")
log$date_time_off <- paste(log$date_off,
                           log$time_off,
                           sep=" ")
log$date_time_on <- strptime(log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
log$date_time_off <- strptime(log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
log$date_time_on <- force_tz(log$date_time_on,
                             tz = "America/Chicago")
log$date_time_off <- force_tz(log$date_time_off,
                              tz = "America/Chicago")

# sbs function for code times
sbs <- function(i) {
  
  new <- seq.POSIXt(mer_anno$NEWstarttime[i], mer_anno$NEWendtime[i], by = "sec")
  annotation <- rep(mer_anno$annotation[i], length(new))
  data.frame(time = new, annotation = annotation)
  
}

# for loop begins
raw_anno <- read.table(file = paste0("./3_data/raw/annotation/", test),
                       header = T,
                       sep = ",")

raw_anno$startTime <- ymd_hms(raw_anno$startTime,
                              tz="UTC")
raw_anno$endTime <- ymd_hms(raw_anno$endTime,
                            tz="UTC")
raw_anno$startTime <- with_tz(raw_anno$startTime,
                              tz = "America/Chicago")
raw_anno$endTime <- with_tz(raw_anno$endTime,
                            tz = "America/Chicago")


# for later
file_name = test
id <- as.integer(substr(test, 6, 9))
visit <- as.integer(substr(test, 11, 11))

# merge times and raw
raw_anno$ID <- id
raw_anno$Visit = visit
mer_anno <- merge(raw_anno, corr_times, by = c("ID", "Visit"))

# add diff to times
mer_anno$NEWstarttime <- NA
mer_anno <- mer_anno %>%
  mutate(NEWstarttime = if_else(!is.na(Difference),
                                startTime + Difference,
                                startTime))
mer_anno$NEWendtime <- NA
mer_anno <- mer_anno %>%
  mutate(NEWendtime = if_else(!is.na(Difference),
                              endTime + Difference,
                              endTime))

# to POSIXlt for padding later 
mer_anno$NEWstarttime <- strptime(mer_anno$NEWstarttime,
                                  format="%Y-%m-%d %H:%M:%OS")
mer_anno$NEWendtime <- strptime(mer_anno$NEWendtime,
                                format="%Y-%m-%d %H:%M:%OS")

# write a "check" csv file to see if stopwatch matches NEW start time
write.table(mer_anno,
            file = paste0("./3_data/processed/anno_check/", file_name),
            sep = ",",
            row.names = F)

# sbs
n <- nrow(mer_anno)
l <- lapply(1:n, sbs)
sbs_anno <- Reduce(rbind, l) %>% 
  pad()

# changing NA's to transition;gap
levels <- levels(sbs_anno$annotation)
levels[length(levels) + 1] <- "transition;gap"
sbs_anno$annotation <- factor(sbs_anno$annotation,
                              levels = levels)
sbs_anno$annotation[is.na(sbs_anno$annotation)] <- "transition;gap"

# clean
sbs_anno$ID <- id
sbs_anno$Visit <- visit
sbs_anno <-sbs_anno[, c("ID",
                        "Visit",
                        "time",
                        "annotation")]

# on off times
on_off <- log[log$ID == id, ]
on_off <- on_off[on_off$Visit == visit, ]

#	if on/off times recorded - loop through and label time monitor is not worn
if(dim(on_off)[1]>0) {
  
  sbs_anno$off <- 1
  
  on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
  class(on)
  off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")
  n <- dim(sbs_anno)[1]
  class(sbs_anno$time)
  inds <- (1:n)[(sbs_anno$time >= on) & (sbs_anno$time <= off)]
  
  if (length(inds)>0) {
    
    sbs_anno$off[inds] <- 0
    
  }
  
} else if(dim(on_off)[1]==0) {
    
    sbs_anno$off <- "No.On.Off.Log"	
    
  }

# Clean
class(sbs_anno$time)
attr(sbs_anno$time, "tzone")
vis_anno <- sbs_anno[sbs_anno$off == 0, ]
vis_anno <- vis_anno[ , !(names(vis_anno) %in% "off")]
vis_anno$annotation <- as.character(vis_anno$annotation) #change to character for next step
vis_anno$annotation[vis_anno$annotation == "posture;0006 sitting"] <- "0" 
vis_anno$annotation[vis_anno$annotation == "posture;0007 standing"] <- "1" 
vis_anno$annotation[vis_anno$annotation == "posture;0008 movement"] <- "2"
vis_anno$annotation[!(vis_anno$annotation %in% c("0", "1", "2"))] <- "3"

# write table
write.table(vis_anno,
            file = paste0("./3_data/processed/anno_clean/", file_name),
            sep = ",",
            row.names = F)


# process_anno - Test 2 ---------------------------------------------------
# test to see what happens if timestamp or visit times or wrong

corr_timstamps_path <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv"
on_off_log <- "visit_on_off_log.csv"

test = "FLAC_1053V2_POSTURE_SMITH.CSV"

# read in timestamps csv and change to times
corr_times <- read.csv(file = corr_timstamps_path)
corr_times$StopWatch_YMD_HMS <- ymd_hms(corr_times$StopWatch_YMD_HMS, 
                                        tz="America/Chicago")
corr_times$Corr_Picture_YMD_HMS <- ymd_hms(corr_times$Corr_Picture_YMD_HMS, 
                                           tz="America/Chicago")

# diff col
corr_times$Difference <- NA
corr_times$Difference <- with(corr_times,
                              difftime(StopWatch_YMD_HMS,
                                       Corr_Picture_YMD_HMS,
                                       units = "secs"))

# read in on off log and clean
log <- read.table(file = paste0("./3_data/raw/", on_off_log),
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

log$date_on <- paste(log$date_on_month,
                     log$date_on_day,
                     log$date_on_year,
                     sep="/")
log$time_on <- paste(log$time_on_hour,
                     log$time_on_minute,
                     log$time_on_seconds,
                     sep=":")
log$date_off <- paste(log$date_off_month,
                      log$date_off_day,
                      log$date_off_year,
                      sep="/")
log$time_off <- paste(log$time_off_hour,
                      log$time_off_minute,
                      log$time_off_seconds,
                      sep=":")
log$date_time_on <- paste(log$date_on,
                          log$time_on,
                          sep=" ")
log$date_time_off <- paste(log$date_off,
                           log$time_off,
                           sep=" ")
log$date_time_on <- strptime(log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
log$date_time_off <- strptime(log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
log$date_time_on <- force_tz(log$date_time_on,
                             tz = "America/Chicago")
log$date_time_off <- force_tz(log$date_time_off,
                              tz = "America/Chicago")

# sbs function for code times
sbs <- function(i) {
  
  new <- seq.POSIXt(mer_anno$NEWstarttime[i], mer_anno$NEWendtime[i], by = "sec")
  annotation <- rep(mer_anno$annotation[i], length(new))
  data.frame(time = new, annotation = annotation)
  
}

# for loop begins
raw_anno <- read.table(file = paste0("./3_data/raw/annotation/", test),
                       header = T,
                       sep = ",")

raw_anno$startTime <- ymd_hms(raw_anno$startTime,
                              tz="UTC")
raw_anno$endTime <- ymd_hms(raw_anno$endTime,
                            tz="UTC")
raw_anno$startTime <- with_tz(raw_anno$startTime,
                              tz = "America/Chicago")
raw_anno$endTime <- with_tz(raw_anno$endTime,
                            tz = "America/Chicago")


# for later
file_name = test
id <- as.integer(substr(test, 6, 9))
visit <- as.integer(substr(test, 11, 11))

# merge times and raw
raw_anno$ID <- id
raw_anno$Visit = visit
mer_anno <- merge(raw_anno, corr_times, by = c("ID", "Visit"))

# add diff to times
mer_anno$NEWstarttime <- NA
mer_anno <- mer_anno %>%
  mutate(NEWstarttime = if_else(!is.na(Difference),
                                startTime + Difference,
                                startTime))
mer_anno$NEWendtime <- NA
mer_anno <- mer_anno %>%
  mutate(NEWendtime = if_else(!is.na(Difference),
                              endTime + Difference,
                              endTime))

# to POSIXlt for padding later 
mer_anno$NEWstarttime <- strptime(mer_anno$NEWstarttime,
                                  format="%Y-%m-%d %H:%M:%OS")
mer_anno$NEWendtime <- strptime(mer_anno$NEWendtime,
                                format="%Y-%m-%d %H:%M:%OS")

# write a "check" csv file to see if stopwatch matches NEW start time
write.table(mer_anno,
            file = paste0("./3_data/processed/anno_check/", file_name),
            sep = ",",
            row.names = F)

# sbs
n <- nrow(mer_anno)
l <- lapply(1:n, sbs)
sbs_anno <- Reduce(rbind, l) %>% 
  pad()

# changing NA's to transition;gap
levels <- levels(sbs_anno$annotation)
levels[length(levels) + 1] <- "transition;gap"
sbs_anno$annotation <- factor(sbs_anno$annotation,
                              levels = levels)
sbs_anno$annotation[is.na(sbs_anno$annotation)] <- "transition;gap"

# clean
sbs_anno$ID <- id
sbs_anno$Visit <- visit
sbs_anno <-sbs_anno[, c("ID",
                        "Visit",
                        "time",
                        "annotation")]

# on off times
on_off <- log[log$ID == id, ]
on_off <- on_off[on_off$Visit == visit, ]
on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
class(on)
off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")

#	label off times
sbs_anno$off <- 1
n <- dim(sbs_anno)[1]
class(sbs_anno$time)
inds <- (1:n)[(sbs_anno$time >= on) & (sbs_anno$time <= off)]

if (length(inds)>0) {
  
  sbs_anno$off[inds] <- 0
  
} else {
  
  message("Stopwatch Timestamp or on-off entry is incorrect")
  
}

# Clean
class(sbs_anno$time)
attr(sbs_anno$time, "tzone")
vis_anno <- sbs_anno[sbs_anno$off == 0, ]
vis_anno <- vis_anno[ , !(names(vis_anno) %in% "off")]
vis_anno$annotation <- as.character(vis_anno$annotation) #change to character for next step
vis_anno$annotation[vis_anno$annotation == "posture;0006 sitting"] <- "0" 
vis_anno$annotation[vis_anno$annotation == "posture;0007 standing"] <- "1" 
vis_anno$annotation[vis_anno$annotation == "posture;0008 movement"] <- "2"
vis_anno$annotation[!(vis_anno$annotation %in% c("0", "1", "2"))] <- "3"

# write table
write.table(vis_anno,
            file = paste0("./3_data/processed/anno_clean/", file_name),
            sep = ",",
            row.names = F)



# process_anno - Test 3 ---------------------------------------------------
# test for when a timestamp in not available

corr_timstamps_path <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv"
on_off_log <- "visit_on_off_log.csv"

test = "FLAC_1085V1_POSTURE_CHANG.CSV"

# read in timestamps csv and change to times
corr_times <- read.csv(file = corr_timstamps_path)
corr_times$StopWatch_YMD_HMS <- ymd_hms(corr_times$StopWatch_YMD_HMS, 
                                        tz="America/Chicago")
corr_times$Corr_Picture_YMD_HMS <- ymd_hms(corr_times$Corr_Picture_YMD_HMS, 
                                           tz="America/Chicago")

# diff col
corr_times$Difference <- NA
corr_times$Difference <- with(corr_times,
                              difftime(StopWatch_YMD_HMS,
                                       Corr_Picture_YMD_HMS,
                                       units = "secs"))

# read in on off log and clean
log <- read.table(file = paste0("./3_data/raw/", on_off_log),
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

log$date_on <- paste(log$date_on_month,
                     log$date_on_day,
                     log$date_on_year,
                     sep="/")
log$time_on <- paste(log$time_on_hour,
                     log$time_on_minute,
                     log$time_on_seconds,
                     sep=":")
log$date_off <- paste(log$date_off_month,
                      log$date_off_day,
                      log$date_off_year,
                      sep="/")
log$time_off <- paste(log$time_off_hour,
                      log$time_off_minute,
                      log$time_off_seconds,
                      sep=":")
log$date_time_on <- paste(log$date_on,
                          log$time_on,
                          sep=" ")
log$date_time_off <- paste(log$date_off,
                           log$time_off,
                           sep=" ")
log$date_time_on <- strptime(log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
log$date_time_off <- strptime(log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
log$date_time_on <- force_tz(log$date_time_on,
                             tz = "America/Chicago")
log$date_time_off <- force_tz(log$date_time_off,
                              tz = "America/Chicago")

# sbs function for code times
sbs <- function(i) {
  
  new <- seq.POSIXt(mer_anno$NEWstarttime[i], mer_anno$NEWendtime[i], by = "sec")
  annotation <- rep(mer_anno$annotation[i], length(new))
  data.frame(time = new, annotation = annotation)
  
}

# for loop begins
raw_anno <- read.table(file = paste0("./3_data/raw/annotation/", test),
                       header = T,
                       sep = ",")

raw_anno$startTime <- ymd_hms(raw_anno$startTime,
                              tz="UTC")
raw_anno$endTime <- ymd_hms(raw_anno$endTime,
                            tz="UTC")
raw_anno$startTime <- with_tz(raw_anno$startTime,
                              tz = "America/Chicago")
raw_anno$endTime <- with_tz(raw_anno$endTime,
                            tz = "America/Chicago")


# for later
file_name = test
id <- as.integer(substr(test, 6, 9))
visit <- as.integer(substr(test, 11, 11))

# merge times and raw
raw_anno$ID <- id
raw_anno$Visit = visit
mer_anno <- merge(raw_anno, corr_times, by = c("ID", "Visit")) 

# to get relevant error message rather than generic one
if (dim(mer_anno)[1] == 0) {
  
  stop("Error: Annotation does not have an entry in Timestamps.csv")
  
}

# add diff to times
mer_anno$NEWstarttime <- NA
mer_anno <- mer_anno %>%
  mutate(NEWstarttime = if_else(!is.na(Difference),
                                startTime + Difference,
                                startTime))
mer_anno$NEWendtime <- NA
mer_anno <- mer_anno %>%
  mutate(NEWendtime = if_else(!is.na(Difference),
                              endTime + Difference,
                              endTime))

# to POSIXlt for padding later 
mer_anno$NEWstarttime <- strptime(mer_anno$NEWstarttime,
                                  format="%Y-%m-%d %H:%M:%OS")
mer_anno$NEWendtime <- strptime(mer_anno$NEWendtime,
                                format="%Y-%m-%d %H:%M:%OS")

# write a "check" csv file to see if stopwatch matches NEW start time
write.table(mer_anno,
            file = paste0("./3_data/processed/anno_check/", file_name),
            sep = ",",
            row.names = F)

# sbs
n <- nrow(mer_anno)
l <- lapply(1:n, sbs)
sbs_anno <- Reduce(rbind, l) %>% 
  pad()

# changing NA's to transition;gap
levels <- levels(sbs_anno$annotation)
levels[length(levels) + 1] <- "transition;gap"
sbs_anno$annotation <- factor(sbs_anno$annotation,
                              levels = levels)
sbs_anno$annotation[is.na(sbs_anno$annotation)] <- "transition;gap"

# clean
sbs_anno$ID <- id
sbs_anno$Visit <- visit
sbs_anno <-sbs_anno[, c("ID",
                        "Visit",
                        "time",
                        "annotation")]

# on off times
on_off <- log[log$ID == id, ]
on_off <- on_off[on_off$Visit == visit, ]
on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
class(on)
off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")

#	label off times
sbs_anno$off <- 1
n <- dim(sbs_anno)[1]
class(sbs_anno$time)
inds <- (1:n)[(sbs_anno$time >= on) & (sbs_anno$time <= off)]

if (length(inds)>0) {
  
  sbs_anno$off[inds] <- 0
  
} else {
  
  message("Stopwatch Timestamp or on-off entry is incorrect")
  
}

# Clean
class(sbs_anno$time)
attr(sbs_anno$time, "tzone")
vis_anno <- sbs_anno[sbs_anno$off == 0, ]
vis_anno <- vis_anno[ , !(names(vis_anno) %in% "off")]
vis_anno$annotation <- as.character(vis_anno$annotation) #change to character for next step
vis_anno$annotation[vis_anno$annotation == "posture;0006 sitting"] <- "0" 
vis_anno$annotation[vis_anno$annotation == "posture;0007 standing"] <- "1" 
vis_anno$annotation[vis_anno$annotation == "posture;0008 movement"] <- "2"
vis_anno$annotation[!(vis_anno$annotation %in% c("0", "1", "2"))] <- "3"

# write table
write.table(vis_anno,
            file = paste0("./3_data/processed/anno_clean/", file_name),
            sep = ",",
            row.names = F)


# process_anno - Test 4 ---------------------------------------------------
# test with on off log in 1_load.R script
corr_timstamps_path <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv"

test = "FLAC_1085V1_POSTURE_CHANG.CSV"

# read in timestamps csv and change to times
corr_times <- read.csv(file = corr_timstamps_path)
corr_times$StopWatch_YMD_HMS <- ymd_hms(corr_times$StopWatch_YMD_HMS, 
                                        tz="America/Chicago")
corr_times$Corr_Picture_YMD_HMS <- ymd_hms(corr_times$Corr_Picture_YMD_HMS, 
                                           tz="America/Chicago")

# diff col
corr_times$Difference <- NA
corr_times$Difference <- with(corr_times,
                              difftime(StopWatch_YMD_HMS,
                                       Corr_Picture_YMD_HMS,
                                       units = "secs"))

# sbs function for code times
sbs <- function(i) {
  
  new <- seq.POSIXt(mer_anno$NEWstarttime[i],
                    mer_anno$NEWendtime[i], by = "sec")
  annotation <- rep(mer_anno$annotation[i],
                    length(new))
  data.frame(time = new,
             annotation = annotation)
  
}

# create processed anno files
print(test)

raw_anno <- read.table(file = paste0("./3_data/raw/annotation/", test),
                       header = T,
                       sep = ",")

raw_anno$startTime <- ymd_hms(raw_anno$startTime,
                              tz="UTC")
raw_anno$endTime <- ymd_hms(raw_anno$endTime,
                            tz="UTC")
raw_anno$startTime <- with_tz(raw_anno$startTime,
                              tz = "America/Chicago")
raw_anno$endTime <- with_tz(raw_anno$endTime,
                            tz = "America/Chicago")

# merge times and raw
id <- as.integer(substr(test, 6, 9))
visit <- as.integer(substr(test, 11, 11))
raw_anno$ID <- id
raw_anno$Visit = visit
mer_anno <- merge(raw_anno,
                  corr_times,
                  by = c("ID",
                         "Visit"))

# check#1: See if timestamp was entered
if (dim(mer_anno)[1] == 0) {
  
  message("Error: Annotation does not have an entry in Timestamps.csv")
  
} else {
  
  # add diff to times
  mer_anno$NEWstarttime <- NA
  mer_anno <- mer_anno %>%
    mutate(NEWstarttime = if_else(!is.na(Difference),
                                  startTime + Difference,
                                  startTime))
  mer_anno$NEWendtime <- NA
  mer_anno <- mer_anno %>%
    mutate(NEWendtime = if_else(!is.na(Difference),
                                endTime + Difference,
                                endTime))
  
  # to POSIXlt for padding later 
  mer_anno$NEWstarttime <- strptime(mer_anno$NEWstarttime,
                                    format="%Y-%m-%d %H:%M:%OS")
  mer_anno$NEWendtime <- strptime(mer_anno$NEWendtime,
                                  format="%Y-%m-%d %H:%M:%OS")
  
  # write a "check" csv file to see if stopwatch matches NEW start time
  write.table(mer_anno,
              file = paste0("./3_data/processed/anno_check/", id, "V", visit, ".csv"),
              sep = ",",
              row.names = F)
  
  # sbs
  n <- nrow(mer_anno)
  l <- lapply(1:n, sbs)
  sbs_anno <- Reduce(rbind, l) %>% 
    pad()
  
  # changing NA's to transition;gap
  levels <- levels(sbs_anno$annotation)
  levels[length(levels) + 1] <- "transition;gap"
  sbs_anno$annotation <- factor(sbs_anno$annotation,
                                levels = levels)
  sbs_anno$annotation[is.na(sbs_anno$annotation)] <- "transition;gap"
  
  # on off times
  on_off <- on_off_log[on_off_log$ID == id & on_off_log$Visit == visit, ]
  on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
  off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")
  
  #	label off times
  sbs_anno$off <- 1
  n <- dim(sbs_anno)[1]
  class(sbs_anno$time)
  inds <- (1:n)[(sbs_anno$time >= on) & (sbs_anno$time <= off)]
  sbs_anno$off[inds] <- 0
  
  # check#2: see if off times were actually labeled
  inds_worn <- (1:(dim(sbs_anno)[1]))[sbs_anno$off==0]
  i <- length(inds_worn)
  if(i == 0) {
    
    message("Error: Stopwatch Timestamp or on-off entry is incorrect")
    
  } else {
    
    # Clean - avsa specific
    vis_anno <- sbs_anno[sbs_anno$off == 0, ] # remove off times
    vis_anno$time <- as.POSIXct(vis_anno$time, 
                                tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
    vis_anno$ID <- id # add in ID
    vis_anno$Visit <- visit # add in Visit number
    vis_anno$annotation <- as.character(vis_anno$annotation) #change to character for next step
    vis_anno$annotation[vis_anno$annotation == "posture;0006 sitting"] <- "0" 
    vis_anno$annotation[vis_anno$annotation == "posture;0007 standing"] <- "1" 
    vis_anno$annotation[vis_anno$annotation == "posture;0008 movement"] <- "2"
    vis_anno$annotation[!(vis_anno$annotation %in% c("0", "1", "2"))] <- "3"
    vis_anno <-vis_anno[, c("ID",
                            "Visit",
                            "time",
                            "annotation")]
    
    # write table
    write.table(vis_anno,
                file = paste0("./3_data/processed/anno_clean/", id, "V", visit, ".csv"),
                sep = ",",
                row.names = F)
  }
}



# process_anno - Test 5 ---------------------------------------------------
# break into sequences for easy traceback on where errors will occur 

# anno_file_list <- list_anno
# corr_times <- timestamps
# on_off_log <- log_on_off
fpa_img_raw <- "./3_data/raw/annotation/"
fpa_img_clean <- "./3_data/processed/anno_clean/"
fnm_img_raw <- anno_file_list[1]

cnt_img_processed <- 1

# create processed anno files
for (i in seq_along(anno_file_list)) {
  
  fnm_img_raw <- anno_file_list[i]
  
  stu_sub_vis <- sub("\\_P.*",
                   "",
                   fnm_img_raw)
  id <- as.integer(substr(fnm_img_raw, 6, 9))
  visit <- as.integer(substr(fnm_img_raw, 11, 11))
  
  message("Processing File #", i, " : ", stu_sub_vis, "...",
          appendLF = FALSE)
  
  #### STEP 1: raw
  message("reading...",
          appendLF = FALSE)
  
  tib_img_raw <- vroom(file = paste0(fpa_img_raw, 
                                     fnm_img_raw),
                       delim = ",")
  
  # raw_anno <- read.table(file = paste0(fpa_img_raw, 
  #                                      fnm_img_raw),
  #                        header = T,
  #                        sep = ",")
  # 
  # raw_anno$startTime <- ymd_hms(raw_anno$startTime,
  #                               tz="UTC")
  # raw_anno$endTime <- ymd_hms(raw_anno$endTime,
  #                             tz="UTC")
  tib_img_raw$startTime <- with_tz(tib_img_raw$startTime,
                                tzone = "America/Chicago")
  tib_img_raw$endTime <- with_tz(tib_img_raw$endTime,
                              tzone = "America/Chicago")
  
  # merge times and raw
  tib_img_raw$ID <- id
  tib_img_raw$Visit = visit
  
  tib_img_mer <- merge(tib_img_raw,
                       corr_times,
                       by = c("ID",
                              "Visit"))
  
  # check#1: See if timestamp was entered
  if (all(is.na(tib_img_mer$Difference))) {
    
    message("\n")
    
    warning("IMG File ", stu_sub_vis, ":\n",
            "    Annotation file does not have an entry in Timestamps.csv\n",
            call. = FALSE)
    
    next()
    
  }
  
  # add diff to times
  tib_img_mer$NEWstarttime <- tib_img_mer$startTime + tib_img_mer$Difference
  tib_img_mer$NEWendtime <- tib_img_mer$endTime + tib_img_mer$Difference
  
  # Need to strptime as there are fractional seconds in the start/end times.
  # padr cannot fill in gaps with fractional seconds.
  
  # seconds(tib_img_mer$NEWstarttime[1])
  
  tib_img_mer$NEWstarttime <- 
    strptime(tib_img_mer$NEWstarttime,
             format = "%Y-%m-%d %H:%M:%OS") %>% 
    as.POSIXct(tz = "America/Chicago")
  tib_img_mer$NEWendtime <- 
    strptime(tib_img_mer$NEWendtime,
             format = "%Y-%m-%d %H:%M:%OS") %>% 
    as.POSIXct(tz = "America/Chicago")
  
  # write a "check" csv file to see if stopwatch matches NEW start time
  write.table(tib_img_mer,
              file = paste0("./3_data/processed/anno_check/",id, "V", visit, ".csv"),
              sep = ",",
              row.names = F)
  
  # STEP 2: second by second
  message("sec-by-sec...",
          appendLF = FALSE)  
  
  cnt_sbs <- 1
  nrw_img_mer <- nrow(tib_img_mer)

  for (i in seq_len(nrw_img_mer)) {
  
    sbs_dtm_img <- seq.POSIXt(from = tib_img_mer$NEWstarttime[i],
                              to = tib_img_mer$NEWendtime[i],
                              by = "sec")
    sbs_ano_img <- rep(tib_img_mer$annotation[i],
                       times = length(sbs_dtm_img))
    
    tib_img_sbs_part <- 
      tibble(
        time       = sbs_dtm_img,
        annotation = sbs_ano_img,
        .rows      = nrow(sbs_dtm_img)
      )
    
    if (cnt_sbs == 1) {
      
      lst_sbs_part <- list(tib_img_sbs_part)
      
    } else if (cnt_sbs > 1) {
      
      lst_sbs_part[[i]] <- tib_img_sbs_part
      
    }
    
    cnt_sbs <- cnt_sbs + 1
    
  }
  
  tib_img_sbs <- 
    suppressMessages(bind_rows(lst_sbs_part) %>% 
    pad())
  
  tib_img_sbs$date <- date(tib_img_sbs$time)
  
  # Need to change NA's to gap just in case for any int subsetting.
  tib_img_sbs$annotation[is.na(tib_img_sbs$annotation)] <- "gap"
  
  ####	STEP 3: Label on off times
  message("off-times...",
          appendLF = FALSE)

  tib_img_sbs$off <- 1
  
  tib_on_off <- on_off_log[on_off_log$ID == id & on_off_log$Visit == visit, ]
  nrw_on_off <- nrow(tib_on_off)
  dbl_sec_worn <- vector(mode = "double",
                         length = nrw_on_off)
  
  #	If on/off times recorded - loop through and label time monitor is not worn.
  for (i in seq_len(nrw_on_off)) {
    
    dtm_on  <- tib_on_off$date_time_on[i]
    dtm_off <- tib_on_off$date_time_off[i]
    
    # Can use integer subsetting as time should never have an NA at this point.
    ind_on <- which(
      (tib_img_sbs$time >= dtm_on) &
      (tib_img_sbs$time <= dtm_off)
    )
    

    # tells us if the on interval was actually in the AP file. Substract
    # one because it includes on time.
    dbl_sec_worn[i] <- length(ind_on) - 1
    
    if (length(ind_on) > 0) {
      
      tib_img_sbs$off[ind_on] <- 0
      
      # If on interval goes past midnight, make all the dates the first date
      # of the on interval.
      tib_img_sbs$date[ind_on] <- tib_img_sbs$date[ind_on][1]
      
    }
  }
  
  #### CHECK 3: see if off times were actually labeled.
  dbl_sec_on <- tib_on_off$seconds_on

  # In the event an on interval overlapped another on interval (take out extra
  # "zero" seconds).
  int_sec_applied <- sum(tib_img_sbs$off == 0) - nrw_on_off
  int_sec_worn_all <- as.integer(sum(dbl_sec_worn))
  
  if (int_sec_applied == 0) {
    
    # none of the AP file has any of the log entries.
    message("\n")
    warning("ID ", id, ":\n",
            "    Event file and on_off_log entries do not match at all.\n",
            "    AP likely not worn at all or corrupt.\n",
            call. = FALSE)
    
    next() 
    
  }  else if (all(dbl_sec_on == dbl_sec_worn) == FALSE) {
    
    # AP partially has on off entry and does not have the rest after.
    message("\n")
    
    ind_mis_part <- which(dbl_sec_on != dbl_sec_worn &
                             dbl_sec_worn != -1)
    
    ind_mis_full <- which(dbl_sec_on != dbl_sec_worn &
                             dbl_sec_worn == -1)
    
    chr_entries_log <- apply(on_off_log[, 1:4],
                              MARGIN = 1, 
                              FUN = paste,
                              collapse = " ")
    chr_entries_visit <- apply(tib_on_off[, 1:4],
                                MARGIN = 1, 
                                FUN = paste,
                                collapse = " ")
    
    ind_log_part <- which(
      chr_entries_log %in% 
        chr_entries_visit[ind_mis_part]
    )
    ind_log_full <- which(
      chr_entries_log %in% 
        chr_entries_visit[ind_mis_full]
    )
    
    chr_mis_time_part <- paste(
      format.POSIXct(on_off_log$date_time_on[ind_log_part],
                     format = "%m/%d/%Y %H:%M:%S"),
      format.POSIXct(on_off_log$date_time_off[ind_log_part],
                     format = "%m/%d/%Y %H:%M:%S"),
      sep = " - "
    )
    
    chr_mis_time_full <- paste(
      format.POSIXct(on_off_log$date_time_on[ind_log_full],
                     format = "%m/%d/%Y %H:%M:%S"),
      format.POSIXct(on_off_log$date_time_off[ind_log_full],
                     format = "%m/%d/%Y %H:%M:%S"),
      sep = " - "
    )
    
    for (i in seq_along(ind_log_part)) {
      
      mis_num    <- ind_log_part[i]
      mis_on_off <- chr_mis_time_part[i]
      
      warning("ID ", id, ":\n",
              "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
              "    Interval partially found in Event File.\n",
              "    Fix entry or remove file.\n",
              call. = FALSE)
      
    }
    
    for (i in seq_along(ind_log_full)) {
      
      mis_num    <- ind_log_full[i]
      mis_on_off <- chr_mis_time_full[i]
      
      warning("ID ", id, ":\n",
              "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
              "    Interval not found at all in Event File.\n",
              "    Fix entry or remove file.\n",
              call. = FALSE)
      
    }
    
    next()
    
  } else if (int_sec_applied != int_sec_worn_all) {
    
    # on intervals are in AP but one on interval overlaps another interval.
    message("\n")
    warning("ID ", id, ":\n",
            "    Event file and on_off_log entries partially match.\n",
            "    One or more log entries overlap each other.\n",
            "    Check on_off entries.\n",
            call. = FALSE)
    
    next()
    
  }
  
  #### STEP 4 : Clean (AVSA specific)
  message("results...\n")
  
  tib_img_vis <- tib_img_sbs[tib_img_sbs$off == 0, ]
  
  attributes(tib_img_vis$ID)
  class(tib_img_vis$annotation)
  
  tib_img_vis$id <- id
  tib_img_vis$visit <- visit

  uncodeable <- c("uncodeable;0001 camera start time",
                  "uncodeable;0002 camera stop time",
                  "uncodeable;0003 camera taken or turned off",
                  "uncodeable;0004 image dark/blurred/obscured")
  
  ind_sit <- which(tib_img_vis$annotation == "posture;0006 sitting")
  ind_sta <- which(tib_img_vis$annotation == "posture;0007 standing")
  ind_mov <- which(tib_img_vis$annotation == "posture;0008 movement")
  ind_gap <- which(tib_img_vis$annotation == "gap")
  ind_tra <- which(tib_img_vis$annotation == "transition;0005 less than 3 images")
  ind_unc <- which(tib_img_vis$annotation %in% uncodeable)
  
  if(length(ind_sit) +
    length(ind_sta) +
    length(ind_mov) +
    length(ind_gap) +
    length(ind_tra) +
    length(ind_unc) != 
    nrow(tib_img_vis)) {
    
    stop()
    
  }
   
  tib_img_vis$annotation[ind_sit] <- "0"
  tib_img_vis$annotation[ind_sta] <- "1"
  tib_img_vis$annotation[ind_mov] <- "2"
  tib_img_vis$annotation[ind_gap] <- "3"
  tib_img_vis$annotation[ind_tra] <- "4"
  tib_img_vis$annotation[ind_unc] <- "5"
  
  # tib_img_vis$annotation[tib_img_vis$annotation == "posture;0006 sitting"] <- "0"
  # tib_img_vis$annotation[tib_img_vis$annotation == "posture;0007 standing"] <- "1"
  # tib_img_vis$annotation[tib_img_vis$annotation == "posture;0008 movement"] <- "2"
  # tib_img_vis$annotation[tib_img_vis$annotation == "gap"] <- "3"
  # tib_img_vis$annotation[tib_img_vis$annotation == "transition;0005 less than 3 images"] <- "4"
  # tib_img_vis$annotation[tib_img_vis$annotation %in% uncodeable] <- "5"
  
  tib_img_vis <-tib_img_vis[, c("id",
                                "visit",
                                "time",
                                "date",
                                "annotation")]
  
  # write table
  vroom_write(tib_img_vis,
              path = paste0(fpa_img_clean,
                            stu_sub_vis,
                            ".csv"),
              delim = ",")
  
  cnt_img_processed <- cnt_img_processed + 1
  
}

message("--------------------------------Done Processing--------------------------------\n",
        "                              ", count_ap - 1, " files processed.\n")


# process_annotation ------------------------------------------------------
{
  
  # fls_img_raw <- list_anno
  # tib_cor_time <- timestamps
  # log_on_off <- on_off_log
  # fpa_img_raw <- "./3_data/raw/annotation"
  # fpa_img_clean <- "./3_data/processed/anno_clean"
  # which(str_detect(fls_img_raw, pattern = "1068V1"))
  # fnm_img_raw <- fls_img_raw[1]
  
  cnt_img_processed <- 1
  
  # Loop for each raw annotation file.
  for (i in seq_along(fls_img_raw)) {
    
    fnm_img_raw <- fls_img_raw[i]
    
    stu_sub_vis <-
      sub("\\_P.*",
          "",
          fnm_img_raw)
    id <- 
      fnm_img_raw %>%
      str_sub(start = 6,
              end = 9) %>%
      as.integer()
    visit <-
      fnm_img_raw %>%
      str_sub(start = 11,
              end = 11) %>%
      as.integer()
    message("Processing File #", i, " ", stu_sub_vis, ": ",
            appendLF = FALSE)
    
    # STEP 1: raw ----
    message("reading...",
            appendLF = FALSE)
    tib_img_raw <- suppressMessages(
      c(fpa_img_raw, fnm_img_raw) %>%
        paste(collapse = "/") %>%
        vroom(delim = ",",
              progress = FALSE)
    )
    #   suppressMessages(
    #   vroom(
    #     file = paste0(
    #       fpa_img_raw, 
    #       fnm_img_raw
    #     ),
    #     delim = ",",
    #     progress = FALSE
    #   )
    # )
    
    # raw_anno <- read.table(file = paste0(fpa_img_raw, 
    #                                      fnm_img_raw),
    #                        header = T,
    #                        sep = ",")
    # 
    # raw_anno$start_time <- ymd_hms(raw_anno$start_time,
    #                               tz="UTC")
    # raw_anno$endTime <- ymd_hms(raw_anno$endTime,
    #                             tz="UTC")
    # with_tz(tib_img_raw$start_time,
    #         tzone = "America/Chicago")
    # with_tz(tib_img_raw$end_time,
    #         tzone = "America/Chicago")
    # strptime(tib_img_mer$new_start_time,
    #          format = "%Y-%m-%d %H:%M:%OS") %>% 
    # as.POSIXct(tz = "America/Chicago")
    # strptime(tib_img_mer$new_end_time,
    #          format = "%Y-%m-%d %H:%M:%OS") %>% 
    # as.POSIXct(tz = "America/Chicago")
    # which(
    #   (tib_img_sbs$time >= dtm_on) &
    #     (tib_img_sbs$time <= dtm_off)
    # )
    
    # Consistency: make all lowercase and _ seperator.
    colnames(tib_img_raw) <-
      colnames(tib_img_raw) %>%
      str_to_lower() %>%
      str_replace(pattern = "time",
                  replacement = "_time")
    
    # Oxford Image Browser outputs in UTC. Convert to CDT/CST to see if
    # correction factors work interactively.
    tib_img_raw$start_time <-
      tib_img_raw$start_time %>%
      lubridate::with_tz(tzone = "America/Chicago")
    tib_img_raw$end_time <-
      tib_img_raw$end_time %>%
      lubridate::with_tz(tzone = "America/Chicago")
    
    # merge times and raw
    tib_img_raw$id <- id
    tib_img_raw$visit = visit
    
    tib_img_mer <-
      merge(tib_img_raw,
            tib_cor_time,
            by = c("id",
                   "visit"))
    
    # check#1: See if timestamp was entered
    if (all(is.na(tib_img_mer$difference))) {
      
      message("\n")
      
      warning("IMG File ", stu_sub_vis, ":\n",
              "    Annotation file does not have an entry in Timestamps.csv\n",
              call. = FALSE)
      
      next()
      
    }
    
    # add diff to times
    tib_img_mer$new_start_time <-
      tib_img_mer$start_time + tib_img_mer$difference
    tib_img_mer$new_end_time <-
      tib_img_mer$end_time + tib_img_mer$difference
    
    # Need to strptime as there are fractional seconds in the start/end times.
    # padr cannot fill in gaps with fractional seconds.
    
    # seconds(tib_img_mer$new_start_time[1])
    
    tib_img_mer$new_start_time <- 
      tib_img_mer$new_start_time %>% 
      strptime(format = "%Y-%m-%d %H:%M:%OS") %>% 
      as.POSIXct(tz = "America/Chicago")
    tib_img_mer$new_end_time <- 
      tib_img_mer$new_end_time %>% 
      strptime(format = "%Y-%m-%d %H:%M:%OS") %>% 
      as.POSIXct(tz = "America/Chicago")
    
    # write a "check" csv file to see if stopwatch matches NEW start time
    write.table(tib_img_mer,
                file = paste0("./3_data/processed/anno_check/",id, "V", visit, ".csv"),
                sep = ",",
                row.names = F)
    
    # STEP 2: second by second ----
    message("sec-by-sec...",
            appendLF = FALSE)
    
    cnt_sbs <- 1
    nrw_img_mer <- nrow(tib_img_mer)
    
    for (i in seq_len(nrw_img_mer)) {
      
      sbs_dtm_img <-
        seq.POSIXt(
          from = tib_img_mer$new_start_time[i],
          to = tib_img_mer$new_end_time[i],
          by = "sec"
        )
      sbs_ano_img <- 
        rep(
          tib_img_mer$annotation[i],
          times = length(sbs_dtm_img)
        )
      tib_img_sbs_part <- 
        tibble(
          time       = sbs_dtm_img,
          annotation = sbs_ano_img,
          .rows      = nrow(sbs_dtm_img)
        )
      
      if (cnt_sbs == 1) {
        
        lst_sbs_part <- list(tib_img_sbs_part)
        
      } else if (cnt_sbs > 1) {
        
        lst_sbs_part[[i]] <- tib_img_sbs_part
        
      }
      
      cnt_sbs <- cnt_sbs + 1
      
    }
    
    tib_img_sbs <- suppressMessages(
      bind_rows(lst_sbs_part) %>% 
        pad()
    )
    
    tib_img_sbs$date <- date(tib_img_sbs$time)
    
    # Need to change NA's to gap just in case for any int subsetting.
    tib_img_sbs$annotation[is.na(tib_img_sbs$annotation)] <- "gap"
    
    # STEP 3: Label on off times ----
    message("off-times...",
            appendLF = FALSE)
    
    tib_img_sbs$off <- 1
    tib_on_off <-
      log_on_off[log_on_off$id == id & log_on_off$visit == visit, ]
    nrw_on_off <- nrow(tib_on_off)
    dbl_sec_applied <-
      vector(mode = "double",
             length = nrw_on_off)
    
    #	If on/off times recorded - loop through and label time monitor is not worn.
    for (i in seq_len(nrw_on_off)) {
      
      dtm_on  <- tib_on_off$date_time_on[i]
      dtm_off <- tib_on_off$date_time_off[i]
      
      # Can use integer subsetting as time should never have an NA at this point.
      ind_on <- 
        tib_img_sbs$time %>%
        dplyr::between(left = dtm_on,
                       right = dtm_off) %>%
        which()
      
      # Tells us if the on interval was actually applied to AP (IMG) file. Substract
      # one because it includes on time.
      dbl_sec_applied[i] <- length(ind_on) - 1
      
      if (length(ind_on) > 0) {
        
        tib_img_sbs$off[ind_on] <- 0
        
        # If on interval goes past midnight, make all the dates the first date
        # of the on interval.
        tib_img_sbs$date[ind_on] <- tib_img_sbs$date[ind_on][1]
        
      }
    }
    
    #### CHECK 3: see if off times were actually labeled.
    dbl_sec_on <- tib_on_off$seconds_on
    
    # In the event an on interval overlapped another on interval (take out extra
    # "zero" seconds).
    int_sec_worn <- 
      sum(tib_img_sbs$off == 0) - nrw_on_off
    int_sec_applied_all <- 
      dbl_sec_applied %>% 
      sum() %>% 
      as.integer()
    
    if (int_sec_applied_all <= 0 ||
        int_sec_worn <= 0) {
      
      # none of the AP (IMG) file has any of the log entries.
      message("\n")
      warning(
        "IMG File ", stu_sub_vis, ":\n",
        "    Event file and on_off_log entries do not match at all.\n",
        "    AP likely not worn at all or corrupt.\n",
        call. = FALSE
      )
      
      next() 
      
    }  else if (all(dbl_sec_on == dbl_sec_applied) == FALSE) {
      
      # For IMG file, still process IMG file but give warnings to check IMG set.
      dtm_img_last <- 
        tib_img_mer$new_end_time[nrw_img_mer]
      lgl_1st_img_ok <- 
        dtm_img_last < tib_on_off$date_time_off[nrw_on_off]
      dtm_img_first <- 
        tib_img_mer$new_start_time[1]
      lgl_last_img_ok <- 
        dtm_img_first < tib_on_off$date_time_on[1]
      
      if (lgl_1st_img_ok == TRUE &
          lgl_last_img_ok == TRUE) {
        
        # Autographer likely died early.
        warning(
          "IMG File ", stu_sub_vis, ":\n",
          "    Last IMG timestamp before visit end time.\n",
          "    IMG file still processed BUT make sure no off stopwatch/clipboard\n",
          "    is seen in IMG set.\n",
          call. = FALSE
        )
        
      }
      
      if (lgl_1st_img_ok == FALSE &
          lgl_last_img_ok == FALSE) {
        
        # Autographer was not started before visit.
        warning(
          "IMG File ", stu_sub_vis, ":\n",
          "    First IMG timestamp is after visit start time.\n",
          "    IMG file still processed BUT make sure no on stopwatch/clipboard\n",
          "    is seen in IMG set.\n",
          call. = FALSE
        )
        
      }
      
      if (lgl_1st_img_ok == FALSE &
          lgl_last_img_ok == TRUE) {
        
        # Autographer was not started before visit and died early. Suspect.
        warning(
          "IMG File ", stu_sub_vis, ":\n",
          "    Last IMG timestamp before visit end time AND\n",
          "    First IMG timestamp is after visit start time.\n",
          "    IMG file still processed BUT make sure no on or off\n",
          "    stopwatch/clipboard is seen in IMG set.\n",
          call. = FALSE
        )
        
      }
      
      # # AP (IMG) partially has on off entry and does not have the rest after.
      # message("\n")
      # 
      # ind_mis_part <- which(dbl_sec_on != dbl_sec_applied &
      #                         dbl_sec_applied != -1)
      # 
      # ind_mis_full <- which(dbl_sec_on != dbl_sec_applied &
      #                         dbl_sec_applied == -1)
      # 
      # chr_entries_log <- apply(log_on_off[, 1:4],
      #                          MARGIN = 1, 
      #                          FUN = paste,
      #                          collapse = " ")
      # chr_entries_visit <- apply(tib_on_off[, 1:4],
      #                            MARGIN = 1, 
      #                            FUN = paste,
      #                            collapse = " ")
      # 
      # ind_log_part <- which(
      #   chr_entries_log %in% 
      #     chr_entries_visit[ind_mis_part]
      # )
      # ind_log_full <- which(
      #   chr_entries_log %in% 
      #     chr_entries_visit[ind_mis_full]
      # )
      # 
      # chr_mis_time_part <- paste(
      #   format.POSIXct(log_on_off$date_time_on[ind_log_part],
      #                  format = "%m/%d/%Y %H:%M:%S"),
      #   format.POSIXct(log_on_off$date_time_off[ind_log_part],
      #                  format = "%m/%d/%Y %H:%M:%S"),
      #   sep = " - "
      # )
      # 
      # chr_mis_time_full <- paste(
      #   format.POSIXct(log_on_off$date_time_on[ind_log_full],
      #                  format = "%m/%d/%Y %H:%M:%S"),
      #   format.POSIXct(log_on_off$date_time_off[ind_log_full],
      #                  format = "%m/%d/%Y %H:%M:%S"),
      #   sep = " - "
      # )
      # 
      # for (i in seq_along(ind_log_part)) {
      #   
      #   mis_num    <- ind_log_part[i]
      #   mis_on_off <- chr_mis_time_part[i]
      #   
      #   warning("IMG File ", stu_sub_vis, ":\n",
      #           "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
      #           "    Interval partially found in Event File.\n",
      #           "    Fix entry or remove file.\n",
      #           call. = FALSE)
      #   
      # }
      # 
      # for (i in seq_along(ind_log_full)) {
      #   
      #   mis_num    <- ind_log_full[i]
      #   mis_on_off <- chr_mis_time_full[i]
      #   
      #   warning("IMG File ", stu_sub_vis, ":\n",
      #           "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
      #           "    Interval not found at all in Event File.\n",
      #           "    Fix entry or remove file.\n",
      #           call. = FALSE)
      #   
      # }
      # 
      # next()
      
    } else if (int_sec_worn != int_sec_applied_all) {
      
      # on intervals are in AP (IMG) but one on interval overlaps another interval.
      message("\n")
      warning(
        "IMG File ", stu_sub_vis, ":\n",
        "    Event file and on_off_log entries partially match.\n",
        "    One or more log entries overlap each other.\n",
        "    Check on_off entries.\n",
        call. = FALSE
      )
      
      next()
      
    }
    
    # STEP 4 : Clean (AVSA specific) ----
    message("cleaning...\n")
    
    tib_img_vis <- tib_img_sbs[tib_img_sbs$off == 0,]
    
    # attributes(tib_img_vis$ID)
    # class(tib_img_vis$annotation)
    
    tib_img_vis$id <- id
    tib_img_vis$visit <- visit
    
    uncodeable <- 
      c("uncodeable;0001 camera start time",
        "uncodeable;0002 camera stop time",
        "uncodeable;0003 camera taken or turned off",
        "uncodeable;0004 image dark/blurred/obscured")
    
    ind_sit <-
      which(tib_img_vis$annotation == "posture;0006 sitting")
    ind_sta <- 
      which(tib_img_vis$annotation == "posture;0007 standing")
    ind_mov <- 
      which(tib_img_vis$annotation == "posture;0008 movement")
    ind_gap <- 
      which(tib_img_vis$annotation == "gap")
    ind_tra <- 
      which(tib_img_vis$annotation == "transition;0005 less than 3 images")
    ind_unc <- 
      which(tib_img_vis$annotation %in% uncodeable)
    
    if(length(ind_sit) +
       length(ind_sta) +
       length(ind_mov) +
       length(ind_gap) +
       length(ind_tra) +
       length(ind_unc) != 
       nrow(tib_img_vis)) {
      
      stop("CHHHHHHHHHHIIIIIITTT")
      
    }
    
    tib_img_vis$annotation[ind_sit] <- "0"
    tib_img_vis$annotation[ind_sta] <- "1"
    tib_img_vis$annotation[ind_mov] <- "2"
    tib_img_vis$annotation[ind_gap] <- "3"
    tib_img_vis$annotation[ind_tra] <- "4"
    tib_img_vis$annotation[ind_unc] <- "5"
    
    # tib_img_vis$annotation[tib_img_vis$annotation == "posture;0006 sitting"] <- "0"
    # tib_img_vis$annotation[tib_img_vis$annotation == "posture;0007 standing"] <- "1"
    # tib_img_vis$annotation[tib_img_vis$annotation == "posture;0008 movement"] <- "2"
    # tib_img_vis$annotation[tib_img_vis$annotation == "gap"] <- "3"
    # tib_img_vis$annotation[tib_img_vis$annotation == "transition;0005 less than 3 images"] <- "4"
    # tib_img_vis$annotation[tib_img_vis$annotation %in% uncodeable] <- "5"
    
    tib_img_vis <- 
      tib_img_vis[, c("id",
                      "visit",
                      "time",
                      "date",
                      "annotation")]
    
    # write table
    fnm_img_clean <- 
      paste0(stu_sub_vis,
             ".csv")
    vroom_write(
      tib_img_vis,
      path = paste(fpa_img_clean,
                   fnm_img_clean,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    cnt_img_processed <- cnt_img_processed + 1
    
  }
  
  message(
    "--------------------------------Done Processing--------------------------------\n",
    "                              ", cnt_img_processed - 1, " files processed.\n"
  )
  
}



# process_ap - Test 1 -----------------------------------------------------
# test with ap file before 11/2018
on_off_log <- "visit_on_off_log.csv"

test <- "1002V3-AP740063 16Feb18 9-35am for 4h 24m-AOSD-CL08090134-Events.csv"

# read in on off log and clean
log <- read.table(file = paste0("./3_data/raw/", on_off_log),
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

log$date_on <- paste(log$date_on_month,
                     log$date_on_day,
                     log$date_on_year,
                     sep="/")
log$time_on <- paste(log$time_on_hour,
                     log$time_on_minute,
                     log$time_on_seconds,
                     sep=":")
log$date_off <- paste(log$date_off_month,
                      log$date_off_day,
                      log$date_off_year,
                      sep="/")
log$time_off <- paste(log$time_off_hour,
                      log$time_off_minute,
                      log$time_off_seconds,
                      sep=":")
log$date_time_on <- paste(log$date_on,
                          log$time_on,
                          sep=" ")
log$date_time_off <- paste(log$date_off,
                           log$time_off,
                           sep=" ")
log$date_time_on <- strptime(log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
log$date_time_off <- strptime(log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
log$date_time_on <- force_tz(log$date_time_on,
                             tz = "America/Chicago")
log$date_time_off <- force_tz(log$date_time_off,
                              tz = "America/Chicago")

# for loop begins
raw_ap <- read.table(file = paste0("./3_data/raw/events/", test),
                     header = T,
                     sep = ",",
                     stringsAsFactors = F)
raw_ap <- raw_ap[,(1:6)]
names(raw_ap) <- c("time",
                   "datacount",
                   "interval",
                   "activity",
                   "cumulativesteps",
                   "methrs")

# Change from Julian time to GMT
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap[,2] <- as.numeric(as.character(raw_ap[,2]))
raw_ap[,3] <- as.numeric(as.character(raw_ap[,3]))
raw_ap[,4] <- as.numeric(as.character(raw_ap[,4]))
raw_ap[,5] <- as.numeric(as.character(raw_ap[,5]))*2 #event files have half the actual number of steps for some reason
raw_ap[,6] <- as.numeric(as.character(raw_ap[,6]))

t <- dim(raw_ap)[1]
class(raw_ap$time)
raw_ap <- raw_ap[!(raw_ap[,"time"] == "1899-12-30"), ]
raw_ap <- raw_ap[!(raw_ap[,"time"] == "0"), ]
n <- dim(raw_ap)[1]		

if(is.character(raw_ap$time) == T & t == n) {
  
  raw_ap$time <- as.numeric(raw_ap$time)
  raw_ap$time <- as.POSIXct(as.Date(raw_ap$time,
                                    origin = "1899-12-30"))
  raw_ap$time <- as.POSIXlt(raw_ap$time,
                            tz = "UTC")
  
  # for some reason, converting to UTC actually makes it relevant time zone
  raw_ap$time <- force_tz(raw_ap$time,
                          tz = "America/Chicago")
  raw_ap$time <- strptime(raw_ap$time,
                          format = "%Y-%m-%d %H:%M:%S")

}

# correction factor for ap files after 11/01/2018
if (date(raw_ap$time)[1] > as.Date("2018-11-01")) {

  raw_ap$time <- raw_ap$time + 3106.8918*24*60*60 ### CORRECTION FACTOR ###

}

# second by second
n <- dim(raw_ap)[1]

# maybe use this code for noldus!!
time_each_event <- as.vector(difftime(strptime(raw_ap$time[seq_len(n - 1) + 1],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      strptime(raw_ap$time[seq_len(n - 1)],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      units = "secs"))
time_each_event <- c(time_each_event,
                     round(raw_ap[n,"interval"],
                           0))
time_each_event[is.na(time_each_event) == T] <- 1

# sbs variables
te <- length(time_each_event)
events <- rep((1:te),
              time_each_event)
acts <- rep(raw_ap$activity,
            time_each_event)
l <- length(acts)
ap_start <- strptime(raw_ap$time[1],
                       format="%Y-%m-%d %H:%M:%S")
times <- ap_start + (0:(l - 1))

# The met hours per second in the interval.
raw_ap$interval <- as.numeric(raw_ap$interval)
raw_ap$methrs <- as.numeric(raw_ap$methrs)

met_hrs <- raw_ap$methrs / raw_ap$interval 	
met_hrs <- rep(met_hrs,
               time_each_event)

# To compute mets per second in the interval, multiply methours by 3600 sec/hour and divide by number of seconds.
mets <- raw_ap$methrs * 3600 / raw_ap$interval
mets <- rep(mets,
            time_each_event)
steps <- rep(raw_ap$cumulativesteps,
             time_each_event)

# Make 15-sec epoch variable and METs
fifteen_sec_times <- ap_start + (15 * rep(0:(floor(l / 15)),
                                            each = 15,
                                            length = l))
fifteen_sec_mets <- tapply(mets,
                           INDEX = fifteen_sec_times,
                           FUN = mean)
fifteen_sec_mets <- rep(fifteen_sec_mets,
                        each = 15,
                        length = l)

# Make 1-min epoch variable and METs
one_min_times <- ap_start + (60 * rep(0:(floor(l / 60)),
                                        each = 60,
                                        length = l))
one_min_mets <- tapply(mets,
                       INDEX = one_min_times,
                       FUN = mean)
one_min_mets <- rep(one_min_mets,
                    each = 60,
                    length = l)

date <- substring(format(times), 1, 10)

# data frame
sbs_ap <- data.frame(time = NA,
                     date = NA,
                     ap_posture = NA,
                     mets = NA,
                     met_hours = NA,
                     steps = NA,
                     num_events = NA,
                     stringsAsFactors = F)
sbs_ap <- sbs_ap[-1,]
sbs_ap <- merge(sbs_ap,
                data.frame(time = times,
                           date = date,
                           ap_posture = acts,
                           mets = mets,
                           fifteen_sec_mets = fifteen_sec_mets,
                           one_min_mets = one_min_mets,
                           met_hours = met_hrs,
                           steps = steps,
                           num_events = events,
                           stringsAsFactors = F),
                all = T)
sbs_ap$mets <- signif(sbs_ap$mets,
                      digits = 3)

# on off times
id <- substr(test, 1, 4)
visit <- substr(test, 6, 6)
on_off <- log[log$ID == id, ]
on_off <- on_off[on_off$Visit == visit, ]

if (dim(on_off)[1] == 0) {
  
  sbs_ap$off <- "Subject/Visit not in on_off_log"		
  message("Subject/Visit not in on_off_log")
  
} else {
  
  on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
  off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")
  
  #	label off times
  sbs_ap$off <- 1
  l <- dim(sbs_ap)[1]
  class(sbs_ap$time)
  inds <- (1:l)[(sbs_ap$time >= on) & (sbs_ap$time <= off)]
  
  if (length(inds) > 0) {
    
    sbs_ap$off[inds] <- 0
    
  } else {
    
    sbs_ap$off <- "AP and on_off do not match"
    message("AP and on_off do not match")
    
  }
}

# Clean - avsa specific
vis_ap$time <- 
  as.POSIXct(vis_ap$time, 
             tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
vis_ap <- 
  sbs_ap[!(sbs_ap$off == 1), ] #remove non-visit time#
vis_ap$ID <- as.numeric(id) #add in ID
vis_ap$Visit <- as.numeric(visit) #add in visit
vis_ap <- 
  vis_ap[ , c("ID",
              "Visit",
              "time",
              "ap_posture")]

# write data frame
write.table(vis_ap,
            file = paste0("./3_data/processed/ap_clean/", id, "V", visit, ".csv"),
            sep = ",",
            row.names = F)


# process_ap - Test 2 -----------------------------------------------------
# test with ap file after 11/2018, n-y dst
on_off_log <- "visit_on_off_log.csv"

test <- "1087V2-AP740077 15Dec10 3-39pm for 3h 58m-AOSD-CL08090134-Events.csv"

# read in on off log and clean
log <- read.table(file = paste0("./3_data/raw/", on_off_log),
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

log$date_on <- paste(log$date_on_month,
                     log$date_on_day,
                     log$date_on_year,
                     sep="/")
log$time_on <- paste(log$time_on_hour,
                     log$time_on_minute,
                     log$time_on_seconds,
                     sep=":")
log$date_off <- paste(log$date_off_month,
                      log$date_off_day,
                      log$date_off_year,
                      sep="/")
log$time_off <- paste(log$time_off_hour,
                      log$time_off_minute,
                      log$time_off_seconds,
                      sep=":")
log$date_time_on <- paste(log$date_on,
                          log$time_on,
                          sep=" ")
log$date_time_off <- paste(log$date_off,
                           log$time_off,
                           sep=" ")
log$date_time_on <- strptime(log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
log$date_time_off <- strptime(log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
log$date_time_on <- force_tz(log$date_time_on,
                             tz = "America/Chicago")
log$date_time_off <- force_tz(log$date_time_off,
                              tz = "America/Chicago")

# for loop begins
raw_ap <- read.table(file = paste0("./3_data/raw/events/", test),
                     header = T,
                     sep = ",",
                     stringsAsFactors = F)
raw_ap <- raw_ap[,(1:6)]
names(raw_ap) <- c("time",
                   "datacount",
                   "interval",
                   "activity",
                   "cumulativesteps",
                   "methrs")

# Change from Julian time to GMT
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap[,2] <- as.numeric(as.character(raw_ap[,2]))
raw_ap[,3] <- as.numeric(as.character(raw_ap[,3]))
raw_ap[,4] <- as.numeric(as.character(raw_ap[,4]))
raw_ap[,5] <- as.numeric(as.character(raw_ap[,5]))*2 #event files have half the actual number of steps for some reason
raw_ap[,6] <- as.numeric(as.character(raw_ap[,6]))

t <- dim(raw_ap)[1]
raw_ap <- raw_ap[!(raw_ap[,"time"] == "1899-12-30"), ]
raw_ap <- raw_ap[!(raw_ap[,"time"] == "0"), ]
n <- dim(raw_ap)[1]		
class(raw_ap$time)

if(is.character(raw_ap$time) == T & t == n) {
  
  raw_ap$time <- as.numeric(raw_ap$time)
  raw_ap$time <- as.POSIXct(as.Date(raw_ap$time,
                                    origin = "1899-12-30"))
  raw_ap$time <- as.POSIXlt(raw_ap$time,
                            tz = "UTC")
  
  # for some reason, converting to UTC actually makes it relevant time zone but w/o daylight savings
  raw_ap$time <- force_tz(raw_ap$time,
                          tz = "America/Chicago")
  raw_ap$time <- strptime(raw_ap$time,
                          format = "%Y-%m-%d %H:%M:%S")
  
}

# check#1: correction factor + dst offset
id <- substr(test, 1, 4)
visit <- substr(test, 6, 6)
on_off <- log[log$ID == id & log$Visit == visit, ]
dim(on_off)[1]
date_time_visit <- on_off$date_time_on
date_time_file <- raw_ap$time[1]

if (dim(on_off)[1] == 0) {
  
  message("Subject/Visit not in on_off_log")
  
} else {
  
  # correction factor for ap files after 11/01/2018
  on_off$date_time_on > as.Date("2018-11-01")
  
  if (date_time_visit > as.Date("2018-11-01")) {
    
    ### CORRECTION FACTOR ###
    raw_ap$time <- raw_ap$time + 3106.8918*24*60*60
    
    ### after testing all files were at least 6 sec off ###
    raw_ap$time <- raw_ap$time + 6 
    
  }
}

# daylight savings
x <- ymd_hms("2010-11-06, 10:00:00") %>% 
  strptime(.,
           format = "%Y-%m-%d %H:%M:%S")
dst(x)
dst(date_time_file)
dst(date_time_visit)

isTRUE(dst(date_time_file))
isFALSE(dst(date_time_file))

isTRUE(dst(date_time_visit))
isFALSE(dst(date_time_visit))

if (all(isFALSE(dst(date_time_file)), 
        isTRUE(dst(date_time_visit)))) {
  
  # n-y: substract 1 hour because it is ahead
  raw_ap$time <- raw_ap$time - 60*60 
  
} else if (all(isTRUE(dst(date_time_file)), 
               isFALSE(dst(date_time_visit)))) {
  
  # y-n: add 1 hour because it is behind
  raw_ap$time <- raw_ap$time + 60*60
  
}

# second by second
n <- dim(raw_ap)[1]
time_each_event <- as.vector(difftime(strptime(raw_ap$time[seq_len(n - 1) + 1],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      strptime(raw_ap$time[seq_len(n - 1)],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      units = "secs"))
time_each_event <- c(time_each_event,
                     round(raw_ap[n,"interval"],
                           0))
time_each_event[is.na(time_each_event) == T] <- 1

# sbs variables
te <- length(time_each_event)
events <- rep((1:te),
              time_each_event)
acts <- rep(raw_ap$activity,
            time_each_event)
l <- length(acts)
ap_start <- strptime(raw_ap$time[1],
                     format="%Y-%m-%d %H:%M:%S")
times <- ap_start + (0:(l - 1))

# The met hours per second in the interval.
raw_ap$interval <- as.numeric(raw_ap$interval)
raw_ap$methrs <- as.numeric(raw_ap$methrs)

met_hrs <- raw_ap$methrs / raw_ap$interval 	
met_hrs <- rep(met_hrs,
               time_each_event)

# To compute mets per second in the interval, multiply methours by 3600 sec/hour and divide by number of seconds.
mets <- raw_ap$methrs * 3600 / raw_ap$interval
mets <- rep(mets,
            time_each_event)
steps <- rep(raw_ap$cumulativesteps,
             time_each_event)

# Make 15-sec epoch variable and METs
fifteen_sec_times <- ap_start + (15 * rep(0:(floor(l / 15)),
                                          each = 15,
                                          length = l))
fifteen_sec_mets <- tapply(mets,
                           INDEX = fifteen_sec_times,
                           FUN = mean)
fifteen_sec_mets <- rep(fifteen_sec_mets,
                        each = 15,
                        length = l)

# Make 1-min epoch variable and METs
one_min_times <- ap_start + (60 * rep(0:(floor(l / 60)),
                                      each = 60,
                                      length = l))
one_min_mets <- tapply(mets,
                       INDEX = one_min_times,
                       FUN = mean)
one_min_mets <- rep(one_min_mets,
                    each = 60,
                    length = l)

date <- substring(format(times), 1, 10)

# data frame
sbs_ap <- data.frame(time = NA,
                     date = NA,
                     ap_posture = NA,
                     mets = NA,
                     met_hours = NA,
                     steps = NA,
                     num_events = NA,
                     stringsAsFactors = F)
sbs_ap <- sbs_ap[-1,]
sbs_ap <- merge(sbs_ap,
                data.frame(time = times,
                           date = date,
                           ap_posture = acts,
                           mets = mets,
                           fifteen_sec_mets = fifteen_sec_mets,
                           one_min_mets = one_min_mets,
                           met_hours = met_hrs,
                           steps = steps,
                           num_events = events,
                           stringsAsFactors = F),
                all = T)
sbs_ap$mets <- signif(sbs_ap$mets,
                      digits = 3)

# on off times
on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")

#	label off times
sbs_ap$off <- 1
l <- dim(sbs_ap)[1]
class(sbs_ap$time)
inds <- (1:l)[(sbs_ap$time >= on) & (sbs_ap$time <= off)]
sbs_ap$off[inds] <- 0

# check#2: see if off times were actually labeled
inds_worn <- (1:(dim(sbs_ap)[1]))[sbs_ap$off==0]
i <- length(inds_worn)
if(i == 0) {
  
  sbs_ap$off <- "AP and on.off.log do not match"
  message("AP and on_off do not match")
  
} else {
  
  # Clean - avsa specific
  vis_ap <- 
    sbs_ap[!(sbs_ap$off == 1), ] #remove non-visit time#
  vis_ap$time <- 
    as.POSIXct(vis_ap$time, 
               tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
  vis_ap$ID <- as.numeric(id) #add in ID
  vis_ap$Visit <- as.numeric(visit) #add in visit
  vis_ap <- 
    vis_ap[ , c("ID",
                "Visit",
                "time",
                "ap_posture")]
  
  # write data frame
  write.table(vis_ap,
              file = paste0("./3_data/processed/ap_clean/", id, "V", visit, ".csv"),
              sep = ",",
              row.names = F)
  
}


# process_ap - Test 3 -----------------------------------------------------
# test with ap file after 11/2018, y-n dst
on_off_log <- "visit_on_off_log.csv"

test <- "1053V1-AP740068 12Aug10 11-03am for 4h 43m-AOSD-CL08090134-Events.csv"

# read in on off log and clean
log <- read.table(file = paste0("./3_data/raw/", on_off_log),
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

log$date_on <- paste(log$date_on_month,
                     log$date_on_day,
                     log$date_on_year,
                     sep="/")
log$time_on <- paste(log$time_on_hour,
                     log$time_on_minute,
                     log$time_on_seconds,
                     sep=":")
log$date_off <- paste(log$date_off_month,
                      log$date_off_day,
                      log$date_off_year,
                      sep="/")
log$time_off <- paste(log$time_off_hour,
                      log$time_off_minute,
                      log$time_off_seconds,
                      sep=":")
log$date_time_on <- paste(log$date_on,
                          log$time_on,
                          sep=" ")
log$date_time_off <- paste(log$date_off,
                           log$time_off,
                           sep=" ")
log$date_time_on <- strptime(log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
log$date_time_off <- strptime(log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
log$date_time_on <- force_tz(log$date_time_on,
                             tz = "America/Chicago")
log$date_time_off <- force_tz(log$date_time_off,
                              tz = "America/Chicago")

# for loop begins
raw_ap <- read.table(file = paste0("./3_data/raw/events/", test),
                     header = T,
                     sep = ",",
                     stringsAsFactors = F)
raw_ap <- raw_ap[,(1:6)]
names(raw_ap) <- c("time",
                   "datacount",
                   "interval",
                   "activity",
                   "cumulativesteps",
                   "methrs")

# Change from Julian time to GMT
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap[,2] <- as.numeric(as.character(raw_ap[,2]))
raw_ap[,3] <- as.numeric(as.character(raw_ap[,3]))
raw_ap[,4] <- as.numeric(as.character(raw_ap[,4]))
raw_ap[,5] <- as.numeric(as.character(raw_ap[,5]))*2 #event files have half the actual number of steps for some reason
raw_ap[,6] <- as.numeric(as.character(raw_ap[,6]))

t <- dim(raw_ap)[1]
raw_ap <- raw_ap[!(raw_ap[,"time"] == "1899-12-30"), ]
raw_ap <- raw_ap[!(raw_ap[,"time"] == "0"), ]
n <- dim(raw_ap)[1]		
class(raw_ap$time)

if(is.character(raw_ap$time) == T & t == n) {
  
  raw_ap$time <- as.numeric(raw_ap$time)
  raw_ap$time <- as.POSIXct(as.Date(raw_ap$time,
                                    origin = "1899-12-30"))
  raw_ap$time <- as.POSIXlt(raw_ap$time,
                            tz = "UTC")
  
  # for some reason, converting to UTC actually makes it relevant time zone but w/o daylight savings
  raw_ap$time <- force_tz(raw_ap$time,
                          tz = "America/Chicago")
  raw_ap$time <- strptime(raw_ap$time,
                          format = "%Y-%m-%d %H:%M:%S")
  
}

# check#1: correction factor + dst offset
id <- substr(test, 1, 4)
visit <- substr(test, 6, 6)
on_off <- log[log$ID == id & log$Visit == visit, ]
dim(on_off)[1]
date_time_visit <- on_off$date_time_on
date_time_file <- raw_ap$time[1]

if (dim(on_off)[1] == 0) {
  
  message("Subject/Visit not in on_off_log")
  
} else if (date_time_visit > as.Date("2018-11-01")) { # correction factor for ap files after 11/01/2018
    
  ### CORRECTION FACTOR ###
  raw_ap$time <- raw_ap$time + 3106.8918*24*60*60
  
  ### after testing all files were at least 6 sec off ###
  raw_ap$time <- raw_ap$time + 6 
  
}

# daylight savings
x <- ymd_hms("2010-11-06, 10:00:00") %>% 
  strptime(.,
           format = "%Y-%m-%d %H:%M:%S")
dst(x)
dst(date_time_file)
dst(date_time_visit)

isTRUE(dst(date_time_file))
isFALSE(dst(date_time_file))

isTRUE(dst(date_time_visit))
isFALSE(dst(date_time_visit))

if (all(isFALSE(dst(date_time_file)), 
        isTRUE(dst(date_time_visit)))) {
  
  # n-y: substract 1 hour because it is ahead
  raw_ap$time <- raw_ap$time - 60*60 
  
} else if (all(isTRUE(dst(date_time_file)), 
               isFALSE(dst(date_time_visit)))) {
  
  # y-n: add 1 hour because it is behind
  raw_ap$time <- raw_ap$time + 60*60
  
}

# second by second
n <- dim(raw_ap)[1]
time_each_event <- as.vector(difftime(strptime(raw_ap$time[seq_len(n - 1) + 1],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      strptime(raw_ap$time[seq_len(n - 1)],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      units = "secs"))
time_each_event <- c(time_each_event,
                     round(raw_ap[n,"interval"],
                           0))
time_each_event[is.na(time_each_event) == T] <- 1

# sbs variables
te <- length(time_each_event)
events <- rep((1:te),
              time_each_event)
acts <- rep(raw_ap$activity,
            time_each_event)
l <- length(acts)
ap_start <- strptime(raw_ap$time[1],
                     format="%Y-%m-%d %H:%M:%S")
times <- ap_start + (0:(l - 1))

# The met hours per second in the interval.
raw_ap$interval <- as.numeric(raw_ap$interval)
raw_ap$methrs <- as.numeric(raw_ap$methrs)

met_hrs <- raw_ap$methrs / raw_ap$interval 	
met_hrs <- rep(met_hrs,
               time_each_event)

# To compute mets per second in the interval, multiply methours by 3600 sec/hour and divide by number of seconds.
mets <- raw_ap$methrs * 3600 / raw_ap$interval
mets <- rep(mets,
            time_each_event)
steps <- rep(raw_ap$cumulativesteps,
             time_each_event)

# Make 15-sec epoch variable and METs
fifteen_sec_times <- ap_start + (15 * rep(0:(floor(l / 15)),
                                          each = 15,
                                          length = l))
fifteen_sec_mets <- tapply(mets,
                           INDEX = fifteen_sec_times,
                           FUN = mean)
fifteen_sec_mets <- rep(fifteen_sec_mets,
                        each = 15,
                        length = l)

# Make 1-min epoch variable and METs
one_min_times <- ap_start + (60 * rep(0:(floor(l / 60)),
                                      each = 60,
                                      length = l))
one_min_mets <- tapply(mets,
                       INDEX = one_min_times,
                       FUN = mean)
one_min_mets <- rep(one_min_mets,
                    each = 60,
                    length = l)

date <- substring(format(times), 1, 10)

# data frame
sbs_ap <- data.frame(time = NA,
                     date = NA,
                     ap_posture = NA,
                     mets = NA,
                     met_hours = NA,
                     steps = NA,
                     num_events = NA,
                     stringsAsFactors = F)
sbs_ap <- sbs_ap[-1,]
sbs_ap <- merge(sbs_ap,
                data.frame(time = times,
                           date = date,
                           ap_posture = acts,
                           mets = mets,
                           fifteen_sec_mets = fifteen_sec_mets,
                           one_min_mets = one_min_mets,
                           met_hours = met_hrs,
                           steps = steps,
                           num_events = events,
                           stringsAsFactors = F),
                all = T)
sbs_ap$mets <- signif(sbs_ap$mets,
                      digits = 3)

# on off times
on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")

#	label off times
sbs_ap$off <- 1
l <- dim(sbs_ap)[1]
class(sbs_ap$time)
inds <- (1:l)[(sbs_ap$time >= on) & (sbs_ap$time <= off)]
sbs_ap$off[inds] <- 0

# check#2: see if off times were actually labeled
inds_worn <- (1:(dim(sbs_ap)[1]))[sbs_ap$off==0]
i <- length(inds_worn)
if(i == 0) {
  
  sbs_ap$off <- "AP and on.off.log do not match"
  message("AP and on_off do not match")
  
} else {
  
  # Clean - avsa specific
  vis_ap <- 
    sbs_ap[!(sbs_ap$off == 1), ] #remove non-visit time#
  vis_ap$time <- 
    as.POSIXct(vis_ap$time, 
               tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
  vis_ap$ID <- as.numeric(id) #add in ID
  vis_ap$Visit <- as.numeric(visit) #add in visit
  vis_ap <- 
    vis_ap[ , c("ID",
                "Visit",
                "time",
                "ap_posture")]
  
  # write data frame
  write.table(vis_ap,
              file = paste0("./3_data/processed/ap_clean/", id, "V", visit, ".csv"),
              sep = ",",
              row.names = F)
}


# process_ap - Test 4 -----------------------------------------------------
# test with on off log in 1_load.R script
test <- "1053V1-AP740068 12Aug10 11-03am for 4h 43m-AOSD-CL08090134-Events.csv"

print(test)

raw_ap <- read.table(file = paste0("./3_data/raw/events/", test),
                     header = T,
                     sep = ",",
                     stringsAsFactors = F)
raw_ap <- raw_ap[,(1:6)]
names(raw_ap) <- c("time",
                   "datacount",
                   "interval",
                   "activity",
                   "cumulativesteps",
                   "methrs")

# Change from Julian time to GMT
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap$time <- sub("#", "", raw_ap$time)
raw_ap[,2] <- as.numeric(as.character(raw_ap[,2]))
raw_ap[,3] <- as.numeric(as.character(raw_ap[,3]))
raw_ap[,4] <- as.numeric(as.character(raw_ap[,4]))
raw_ap[,5] <- as.numeric(as.character(raw_ap[,5]))*2 #event files have half the actual number of steps for some reason
raw_ap[,6] <- as.numeric(as.character(raw_ap[,6]))

t <- dim(raw_ap)[1]
raw_ap <- raw_ap[!(raw_ap[,"time"] == "1899-12-30"), ]
raw_ap <- raw_ap[!(raw_ap[,"time"] == "0"), ]
n <- dim(raw_ap)[1]		

if(is.character(raw_ap$time) == T & t == n) {
  
  raw_ap$time <- as.numeric(raw_ap$time)
  raw_ap$time <- as.POSIXct(as.Date(raw_ap$time,
                                    origin = "1899-12-30"))
  raw_ap$time <- as.POSIXlt(raw_ap$time,
                            tz = "UTC")
  
  # for some reason, converting to UTC actually makes it relevant time zone
  raw_ap$time <- force_tz(raw_ap$time,
                          tz = "America/Chicago")
  raw_ap$time <- strptime(raw_ap$time,
                          format = "%Y-%m-%d %H:%M:%S")
  
}

# check#1: See if ap file is in log +correction factor + dst offset
id <- substr(test, 1, 4)
visit <- substr(test, 6, 6)
on_off <- on_off_log[on_off_log$ID == id & on_off_log$Visit == visit, ]
date_time_visit <- on_off$date_time_on
date_time_file <- raw_ap$time[1]

if (dim(on_off)[1] == 0) {
  
  message("Error: Subject/Visit not in on_off_log")
  
} else {
  
  # correction factor for ap files after 11/01/2018
  if (date_time_visit > as.Date("2018-11-01")) { 
    
    ### CORRECTION FACTOR ###
    raw_ap$time <- raw_ap$time + 3106.8918*24*60*60
    
    ### after testing all files were at least 6 sec off ###
    raw_ap$time <- raw_ap$time + 6 
    
    # daylight savings
    if (all(isFALSE(dst(date_time_file)), 
            isTRUE(dst(date_time_visit)))) {
      
      # n-y: substract 1 hour because it is ahead
      raw_ap$time <- raw_ap$time - 60*60 
      
    } else if (all(isTRUE(dst(date_time_file)), 
                   isFALSE(dst(date_time_visit)))) {
      
      # y-n: add 1 hour because it is behind
      raw_ap$time <- raw_ap$time + 60*60
      
    }
  }
  
  # second by second
  n <- dim(raw_ap)[1]
  time_each_event <- as.vector(difftime(strptime(raw_ap$time[seq_len(n - 1) + 1],
                                                 format="%Y-%m-%d %H:%M:%S"),
                                        strptime(raw_ap$time[seq_len(n - 1)],
                                                 format="%Y-%m-%d %H:%M:%S"),
                                        units = "secs"))
  time_each_event <- c(time_each_event,
                       round(raw_ap[n,"interval"],
                             0))
  time_each_event[is.na(time_each_event) == T] <- 1
  
  # sbs variables
  te <- length(time_each_event)
  events <- rep((1:te),
                time_each_event)
  acts <- rep(raw_ap$activity,
              time_each_event)
  
  l <- length(acts)
  ap_start <- strptime(raw_ap$time[1],
                       format="%Y-%m-%d %H:%M:%S")
  times <- ap_start + (0:(l - 1))
  
  # The met hours per second in the interval.
  raw_ap$interval <- as.numeric(raw_ap$interval)
  raw_ap$methrs <- as.numeric(raw_ap$methrs)
  
  met_hrs <- raw_ap$methrs / raw_ap$interval 	
  met_hrs <- rep(met_hrs,
                 time_each_event)
  
  # To compute mets per second in the interval, multiply methours by 3600 sec/hour and divide by number of seconds.
  mets <- raw_ap$methrs * 3600 / raw_ap$interval
  mets <- rep(mets,
              time_each_event)
  steps <- rep(raw_ap$cumulativesteps,
               time_each_event)
  
  # Make 15-sec epoch variable and METs
  fifteen_sec_times <- ap_start + (15 * rep(0:(floor(l / 15)),
                                            each = 15,
                                            length = l))
  fifteen_sec_mets <- tapply(mets,
                             INDEX = fifteen_sec_times,
                             FUN = mean)
  fifteen_sec_mets <- rep(fifteen_sec_mets,
                          each = 15,
                          length = l)
  
  # Make 1-min epoch variable and METs
  one_min_times <- ap_start + (60 * rep(0:(floor(l / 60)),
                                        each = 60,
                                        length = l))
  one_min_mets <- tapply(mets,
                         INDEX = one_min_times,
                         FUN = mean)
  one_min_mets <- rep(one_min_mets,
                      each = 60,
                      length = l)
  
  date <- substring(format(times), 1, 10)
  
  # data frame
  sbs_ap <- data.frame(time = NA,
                       date = NA,
                       ap_posture = NA,
                       mets = NA,
                       met_hours = NA,
                       steps = NA,
                       num_events = NA,
                       stringsAsFactors = F)
  sbs_ap <- sbs_ap[-1,]
  sbs_ap <- merge(sbs_ap,
                  data.frame(time = times,
                             date = date,
                             ap_posture = acts,
                             mets = mets,
                             fifteen_sec_mets = fifteen_sec_mets,
                             one_min_mets = one_min_mets,
                             met_hours = met_hrs,
                             steps = steps,
                             num_events = events,
                             stringsAsFactors = F),
                  all = T)
  sbs_ap$mets <- signif(sbs_ap$mets,
                        digits = 3)
  
  # on off times
  on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
  off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")
  
  #	label off times
  sbs_ap$off <- 1
  l <- dim(sbs_ap)[1]
  class(sbs_ap$time)
  inds <- (1:l)[(sbs_ap$time >= on) & (sbs_ap$time <= off)]
  sbs_ap$off[inds] <- 0
  
  # check#2: see if off times were actually labeled
  inds_worn <- (1:(dim(sbs_ap)[1]))[sbs_ap$off==0]
  i <- length(inds_worn)
  if(i == 0) {
    
    message("Error: AP and on_off do not match")
    
  } else {
    
    # Clean - avsa specific
    vis_ap <- sbs_ap[sbs_ap$off == 0, ] # remove non-visit time
    vis_ap$time <- as.POSIXct(vis_ap$time, 
                              tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
    vis_ap$ID <- as.numeric(id) #add in ID
    vis_ap$Visit <- as.numeric(visit) #add in visit
    vis_ap <- vis_ap[ , c("ID",
                          "Visit",
                          "time",
                          "ap_posture")]
    
    # write data frame
    write.table(vis_ap,
                file = paste0("./3_data/processed/ap_clean/", id, "V", visit, ".csv"),
                sep = ",",
                row.names = F)
  }
}



# process_activpal2 -------------------------------------------------------

# fpa_ap_raw <- "./3_data/raw/events"
# fpa_ap_clean <- "./3_data/processed/ap_clean"
# log_on_off <- on_off_log
# id_visits <- "1096V2"

cnt_ap <- 1

fls_ap_raw <- 
  list.files(
    path = fpa_ap_raw,
    pattern = ".csv"
  )

if (purrr::is_null(id_visits) == FALSE) {
  
  if (purrr::is_scalar_character(id_visits)) {
    
    ind_fls_raw <- 
      fls_ap_raw %>% 
      str_detect(pattern = id_visits) %>% 
      which()
    fls_ap_raw <- 
      fls_ap_raw[ind_fls_raw]
    
  } else {
    
    # id_visits is not a character or single entry.
    stop(
      'Argument "id_visits" is not scalar.\n',
      'Please make sure id_visits are seperated by "|" with no spaces\n',
      "in between."
    )
    
  }
  
}

# fnm_ap_raw <- fls_ap_raw[1]

# since event file names have mumbo jumbo after first 6 characters
# chr_ids_visits <- 
#   str_sub(
#     fls_ap_raw,
#     start = 1,
#     end = 6
#   )

for (i in seq_along(fls_ap_raw)) {
  
  fnm_ap_raw <- fls_ap_raw[i]
  id_visit <-
    str_sub(
      fnm_ap_raw,
      start = 1,
      end = 6
    )
  id <- 
    str_sub(
      fnm_ap_raw,
      start = 1,
      end = 4
    )
  visit <- 
    str_sub(
      fnm_ap_raw,
      start = 6,
      end = 6
    )
  message(
    "Processing File #", i, " ", id_visit, ":",
    appendLF = FALSE
  )
  
  #### CHECK 1: make sure corresponding event file of id in subject log is present
  # if (id %in% char_ap_ids == FALSE) {
  #   
  #   message("\n")
  #   warning("ID ", id, ":\n",
  #           "    Event file not found or named incorrectly\n",
  #           call. = FALSE)
  #   
  #   next()
  #   
  # }
  # 
  # inds_ap_raw <- which(char_ap_ids %in% id)
  # 
  # path_ap_raw <- paste0(path_ap_event_files,
  #                       flst_ap_raw[inds_ap_raw])
  
  # STEP 1: Clean Raw AP File ----
  message(
    "reading...",
    appendLF = FALSE
  )
  tib_ap_raw <-
    suppressMessages(
      vroom(
        file = paste(
          fpa_ap_raw,
          fnm_ap_raw,
          sep = "/"
        ),
        delim = ",",
        progress = FALSE
      )
    )
  tib_ap_raw <- 
    tib_ap_raw[, c("Time",                                              
                   "DataCount (samples)",                             
                   "Interval (s)",                                  
                   "ActivityCode (0=sedentary, 1=standing, 2=stepping)",
                   "CumulativeStepCount",                      
                   "Activity Score (MET.h)")]
  colnames(tib_ap_raw) <-
    c("time",
      "datacount",
      "interval",
      "activity",
      "cumulativesteps",
      "methrs")
  tib_ap_raw$time <- 
    str_replace(
      tib_ap_raw$time,
      pattern = "#",
      replacement = ""
    )
  tib_ap_raw$time <- 
    str_replace(
      tib_ap_raw$time,
      pattern = "#",
      replacement = ""
    )
  # all(
  #   sub(
  #     pattern = "#",
  #     replacement = "",
  #     tib_ap_raw$time
  #   ) ==
  #     str_replace(
  #       tib_ap_raw$time,
  #       pattern = "#",
  #       replacement = ""
  #     )
  # )
  # tib_ap_raw$time <- sub("#", "", tib_ap_raw$time)
  # tib_ap_raw$time <- sub("#", "", tib_ap_raw$time)
  
  # event files have half the actual number of steps for some reason
  tib_ap_raw$cumulativesteps <- tib_ap_raw$cumulativesteps * 2
  # all(
  #   (tib_ap_raw[, "cumulativesteps"] * 2) == (tib_ap_raw[, 5] * 2)
  # )
  
  # make sure event file is not corrupt
  nrw_ap_raw1 <- nrow(tib_ap_raw)
  tib_ap_raw  <- tib_ap_raw[!(tib_ap_raw$time == "1899-12-30"), ]
  tib_ap_raw  <- tib_ap_raw[!(tib_ap_raw$time == "0"), ]
  nrw_ap_raw2 <- nrow(tib_ap_raw)		
  
  # Change from Julian time to UTC
  if(is.character(tib_ap_raw$time) == TRUE &
     nrw_ap_raw1 == nrw_ap_raw2) {
    
    # convert from Julian date to UTC date time. For some reason, UTC time is
    # actually relevant time zone.
    tib_ap_raw$time <- 
      as.double(tib_ap_raw$time) %>% 
      lubridate::as_date(origin = "1899-12-30") %>% 
      lubridate::as_datetime(tz = "UTC") %>% 
      lubridate::force_tz(tzone = "America/Chicago")      
    
    # attributes(tib_ap_raw$time)
    # Difference with base R is date time is kept as POSIXct instead of
    # going back and forth to POSIXlt.
    # tib_ap_raw$time <- as.numeric(tib_ap_raw$time) %>% 
    # as.Date(origin = "1899-12-30") %>% 
    #   as.POSIXct() %>% 
    #   as.POSIXlt(tz = "UTC") %>% 
    #   force_tz(tzone = "America/Chicago") %>% 
    #   as.POSIXct()
    
  } else {
    
    stop("THIS ISNT SUPPPPPPPOSE TO HAPPPPPPEN")
    
  }
  
  # test2 <- tib_ap_raw[, c(1, 3)]
  # test2$interval_hms <- seconds_to_period(test2$interval)
  # test2$is_dst <- dst(tib_ap_raw$time)
  # which(duplicated(test2$is_dst) == FALSE)
  # test2 <- test2[5749:5753, -2]
  # test2$UTCtime <- with_tz(test2$time,
  #                          tzone = "UTC")
  # with_tz(test2$time,
  #         tzone = "UTC")
  
  
  #### CHECK 2: See if ap file is in on_off_log
  tib_on_off <- 
    log_on_off[log_on_off$id == id & log_on_off$visit == visit, ]
  
  # AP Event file does not automatically adjust for dst.
  # Therefore, only rely on first entry for both.
  dtm_file <- tib_ap_raw$time[1]
  dtm_visit  <- tib_on_off$date_time_on[1]
  tib_ap_raw$is_dst <- dst(tib_ap_raw$time)
  lgl_dst <- unique(tib_ap_raw$is_dst)
  
  if (nrow(tib_on_off) == 0) {
    
    message("\n")
    warning("AP File ", id_visit, ":\n",
            "    No entry in on_off_log.\n",
            call. = FALSE)
    
    next()
    
  }
  
  # AP files after 11/01/2018 have incorrect date due to outdated
  # PALStudio WHICH. IS. STILL. BEING. USED. However, applying correction
  # factor will account for dst for an AP that croses from no-yes/yes-no.
  if (dtm_visit > as.Date("2018-11-01")) {
    
    ### CORRECTION FACTOR ###
    tib_ap_raw$time <- tib_ap_raw$time + 3106.8918*24*60*60
    
    # after testing all files were at least 6 sec off
    tib_ap_raw$time <- tib_ap_raw$time + 6 
    
    # daylight savings
    if (dst(dtm_file) == FALSE &
        dst(dtm_visit) == TRUE) {
      
      # n-y: substract 1 hour because it is ahead
      tib_ap_raw$time <- tib_ap_raw$time - 60*60 
      
    } else if (dst(dtm_file) == TRUE &
               dst(dtm_visit) == FALSE) {
      
      # y-n: add 1 hour because it is behind
      tib_ap_raw$time <- tib_ap_raw$time + 60*60
      
    }
    
  } else if (length(lgl_dst) == 2) {
    
    # For AP files that switch from y-n or n-yes, must add/remove an hour.
    ind_dst <- which(duplicated(tib_ap_raw$is_dst) == FALSE)[2]
    # which(duplicated(tib_ap_raw$is_dst) == FALSE)
    # which(duplicated(tib_ap_raw$is_dst, fromLast = T) == FALSE)
    
    if (lgl_dst[1] == FALSE) {
      
      # n-y: add one hour when dst is TRUE
      tib_ap_raw$time[ind_dst:nrw_ap_raw2] <- 
        tib_ap_raw$time[ind_dst:nrw_ap_raw2] + 60*60
      
    } else if (lgl_dst[1] == TRUE) {
      
      # y-n: subtract one hour when dst is FALSE
      tib_ap_raw$time[ind_dst:nrw_ap_raw2] <- 
        tib_ap_raw$time[ind_dst:nrw_ap_raw2] - 60*60
      
    }
    
  }
  
  # STEP 2: Second by Second ----
  message(
    "sec-by-sec...",
    appendLF = FALSE
  )  
  
  # OLD
  # test <- tibble(
  #   og    = tib_ap_raw$interval,
  #   diff  = times2,
  #   og_round = round(
  #     tib_ap_raw$interval,
  #     digits = 0
  #   ),
  #   diff_round  = round(
  #     int_event_time,
  #     digits = 0
  #   ),
  #   lgl   = (og_round == diff_round),
  #   .rows = nrw_ap_raw2
  # )
  # test2 <- 
  #   test[test$lgl == FALSE, ]
  # og_round <- round(
  #   tib_ap_raw$interval,
  #   digits = 0
  # )
  # difftime(
  #   (tib_ap_raw$time[nrw_ap_raw2] + tib_ap_raw$interval[nrw_ap_raw2]),
  #   tib_ap_raw$time[1],
  #   units = "secs"
  # )
  # sum(tib_ap_raw$interval)
  # sum(times2)
  # sum(int_event_time)
  # sum(og_round)
  # 
  # time_each_event <- as.vector(difftime(strptime(raw_ap$time[seq_len(n - 1) + 1],
  #                                                format="%Y-%m-%d %H:%M:%S"),
  #                                       strptime(raw_ap$time[seq_len(n - 1)],
  #                                                format="%Y-%m-%d %H:%M:%S"),
  #                                       units = "secs"))
  # time_each_event <- c(time_each_event,
  #                      round(raw_ap[n,"interval"],
  #                            0))
  # time_each_event[is.na(time_each_event) == T] <- 1
  # all(
  #   as.vector(
  #     difftime(
  #       tib_ap_raw$time[seq_len(nrw_ap_raw2 - 1) + 1],
  #       tib_ap_raw$time[seq_len(nrw_ap_raw2 - 1)],
  #       units = "secs"
  #     )
  #   ) ==
  #     diff.POSIXt(
  #       tib_ap_raw$time,
  #       lag = 1,
  #       differences = 1
  #     )
  # )
  # int_event_time <- 
  #   c(int_event_time,
  #     tib_ap_raw$interval[nrw_ap_raw2])
  # int_event_time <- 
  #   round(
  #     int_event_time,
  #     digits = 0
  #   )
  # tib_ap_raw <- 
  #   add_row(
  #     tib_ap_raw,
  #     time            = (
  #       tib_ap_raw$time[nrw_ap_raw2] +
  #         tib_ap_raw$interval[nrw_ap_raw2]
  #     ),
  #     interval        = 0,
  #     activity        = tib_ap_raw$activity[nrw_ap_raw2],
  #     cumulativesteps = tib_ap_raw$cumulativesteps[nrw_ap_raw2],
  #     methrs          = 0
  #   )
  # raw_dtm_strpd <-
  #   strptime(
  #     raw_dtm,
  #     format = "%Y-%m-%d %H:%M:%OS"
  #   ) %>%
  #   as.POSIXct(tz = "America/Chicago")
  # int_event_time <- 
  #   as.vector(
  #     diff.POSIXt(
  #       raw_dtm_strpd,
  #       lag = 1,
  #       differences = 1
  #     )
  #   )
  # sbs_int_events <- 
  #   rep(
  #     seq_along(int_event_time),
  #     times = int_event_time
  #   )
  # sbs_int_acts <- 
  #   as.integer(
  #     rep(
  #       tib_ap_raw$activity,
  #       times = int_event_time
  #     )
  #   )
  # sbs_dbl_steps <- 
  #   rep(
  #     tib_ap_raw$cumulativesteps,
  #     times = int_event_time
  #   )
  # dtm_start <- raw_dtm_strpd[1]
  # test <- 
  #   seq.POSIXt(
  #     dtm_start,
  #     by = "secs",
  #     length.out = nrw_ap_sbs
  #   )
  # sbs_dtm_times <- dtm_start + (0:(nrw_ap_sbs - 1))
  # all(
  #   test == sbs_dtm_times
  # )
  # tib_ap_raw$interval <- as.numeric(tib_ap_raw$interval)
  # tib_ap_raw$methrs <- as.numeric(tib_ap_raw$methrs)
  # sbs_dbl_methrs <- tib_ap_raw$methrs / tib_ap_raw$interval 	
  # sbs_dbl_methrs <- rep(sbs_dbl_methrs,
  #                        times = int_event_time)
  # sbs_dbl_mets <- tib_ap_raw$methrs * 3600 / tib_ap_raw$interval
  # sbs_dbl_mets <- rep(sbs_dbl_mets,
  #                      times = int_event_time)
  
  # Create dtm vector where last entry is time[nrow] + interval[nrow] for diff
  # function. Do it this way as previous method of adding in the rounded
  # interval[nrow] to stripped time does not take into account fractional
  # seconds.
  raw_dtm <- 
    c(tib_ap_raw$time,
      tib_ap_raw$time[nrw_ap_raw2] + tib_ap_raw$interval[nrw_ap_raw2])
  
  # To make sec-by-sec, take the difference between each time and the previous
  # time. Do this with the "strptimed" date-time's as this will account for 
  # added fractional seconds. Not doing so will result in a sbs file less 
  # than 2 hours for visits. This will also make time entries exactly equal to
  # on and off times.
  # seconds(tib_ap_raw$time[1])
  raw_dtm_strpd <-
    raw_dtm %>% 
    strptime(format = "%Y-%m-%d %H:%M:%OS") %>%
    as.POSIXct(tz = "America/Chicago")
  int_event_time <- 
    raw_dtm_strpd %>% 
    diff.POSIXt(
      lag = 1,
      differences = 1
    ) %>% 
    as.vector()
  int_event_time[is.na(int_event_time)] <- 1
  int_event_time <- as.integer(int_event_time)
  
  # make sbs variables:
  sbs_int_events <- 
    int_event_time %>% 
    seq_along() %>% 
    rep(times = int_event_time)
  sbs_int_acts <- 
    tib_ap_raw$activity %>% 
    rep(times = int_event_time) %>% 
    as.integer()
  sbs_dbl_steps <-
    tib_ap_raw$cumulativesteps %>% 
    rep(times = int_event_time) %>% 
    as.double()
  nrw_ap_sbs <- length(sbs_int_events)
  sbs_dtm_times <- 
    seq.POSIXt(
      from = raw_dtm_strpd[1],
      by = "secs",
      length.out = nrw_ap_sbs
    )
  sbs_dte_dates <- date(sbs_dtm_times)
  
  # The met hours per second in the interval.
  sbs_dbl_methrs <- 
    (tib_ap_raw$methrs / tib_ap_raw$interval) %>% 
    rep(times = int_event_time)
  
  # To compute mets per second in the interval, multiply methours by 3600 sec/hour
  # and divide by number of seconds.
  sbs_dbl_mets <- 
    (tib_ap_raw$methrs * 3600 / tib_ap_raw$interval) %>% 
    rep(times = int_event_time)
  
  # ap_sbs
  tib_ap_sbs <- 
    tibble(
      time       = sbs_dtm_times,
      date       = sbs_dte_dates,
      ap_posture = sbs_int_acts,
      mets       = sbs_dbl_mets,
      met_hours  = sbs_dbl_methrs,
      steps      = sbs_dbl_steps,
      num_events = sbs_int_events,
      .rows      = nrw_ap_sbs
    )
  tib_ap_sbs$mets <- 
    signif(
      tib_ap_sbs$mets,
      digits = 3
    )
  
  #	STEP 3: Label on off times ----
  message(
    "off-times...",
    appendLF = FALSE
  )
  tib_ap_sbs$off <- 1
  nrw_on_off <- nrow(tib_on_off)
  dbl_sec_applied <- vector(mode = "double",
                            length = nrw_on_off)
  
  #	If on/off times recorded - loop through and label time monitor is not worn.
  for (i in seq_len(nrw_on_off)) {
    
    dtm_on  <- tib_on_off$date_time_on[i]
    dtm_off <- tib_on_off$date_time_off[i]
    
    # Can use integer subsetting as time should never have an NA at this point.
    ind_on <- 
      tib_ap_sbs$time %>% 
      dplyr::between(
        left = dtm_on,
        right = dtm_off
      ) %>% 
      which()
    # test <- 
    #   which(
    #     (tib_ap_sbs$time >= dtm_on) &
    #       (tib_ap_sbs$time <= dtm_off)
    #   )
    # test2 <- 
    #   ind_on <- 
    #   tib_ap_sbs$time %>% 
    #   between(
    #     left = dtm_on,
    #     right = dtm_off
    #   ) %>% 
    #   which()
    # all(
    #   test == test2
    # )
    
    # Tells us if the on interval was actually applied to AP (IMG) file. Substract
    # one because it includes on time.
    dbl_sec_applied[i] <- length(ind_on) - 1
    
    if (length(ind_on) > 0) {
      
      tib_ap_sbs$off[ind_on] <- 0
      
      # If on interval goes past midnight, make all the dates the first date
      # of the on interval.
      tib_ap_sbs$date[ind_on] <- tib_ap_sbs$date[ind_on][1]
      
    }
  }
  
  #### CHECK 3: see if off times were actually labeled.
  dbl_sec_on <- tib_on_off$seconds_on
  
  # In the event an on interval overlapped another on interval (take out extra
  # "zero" seconds).
  int_sec_worn <- sum(tib_ap_sbs$off == 0) - nrw_on_off
  int_sec_applied_all <- as.integer(sum(dbl_sec_applied))
  
  if (int_sec_applied_all <= 0 ||
      int_sec_worn <= 0) {
    
    # none of the AP (IMG) file has any of the log entries.
    message("\n")
    warning("AP File ", id_visit, ":\n",
            "    Event file and on_off_log entries do not match at all.\n",
            "    AP likely not worn at all or corrupt.\n",
            call. = FALSE)
    
    next() 
    
  }  else if (all(dbl_sec_on == dbl_sec_applied) == FALSE) {
    
    # AP (IMG) partially has on off entry and does not have the rest after.
    message("\n")
    ind_mis_part <- 
      which(
        dbl_sec_on != dbl_sec_applied &
          dbl_sec_applied != -1
      )
    ind_mis_full <- 
      which(
        dbl_sec_on != dbl_sec_applied &
          dbl_sec_applied == -1
      )
    chr_entries_log <- 
      apply(
        log_on_off[, 1:4],
        MARGIN = 1,
        FUN = paste,
        collapse = " "
      )
    chr_entries_visit <- 
      apply(
        tib_on_off[, 1:4],
        MARGIN = 1,
        FUN = paste,
        collapse = " "
      )
    ind_log_part <- 
      which(
        chr_entries_log %in%
          chr_entries_visit[ind_mis_part]
      )
    ind_log_full <- 
      which(
        chr_entries_log %in%
          chr_entries_visit[ind_mis_full]
      )
    chr_mis_time_part <- 
      paste(
        format.POSIXct(
          log_on_off$date_time_on[ind_log_part],
          format = "%m/%d/%Y %H:%M:%S"
        ),
        format.POSIXct(
          log_on_off$date_time_off[ind_log_part],
          format = "%m/%d/%Y %H:%M:%S"
        ),
        sep = " - "
      )
    
    chr_mis_time_full <- 
      paste(
        format.POSIXct(
          log_on_off$date_time_on[ind_log_full],
          format = "%m/%d/%Y %H:%M:%S"
        ),
        format.POSIXct(
          log_on_off$date_time_off[ind_log_full],
          format = "%m/%d/%Y %H:%M:%S"
        ),
        sep = " - "
      )
    
    for (i in seq_along(ind_log_part)) {
      
      mis_num    <- ind_log_part[i]
      mis_on_off <- chr_mis_time_part[i]
      
      warning("AP File ", id_visit, ":\n",
              "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
              "    Interval partially found in Event File.\n",
              "    Fix entry or remove file.\n",
              call. = FALSE)
      
    }
    
    for (i in seq_along(ind_log_full)) {
      
      mis_num    <- ind_log_full[i]
      mis_on_off <- chr_mis_time_full[i]
      
      warning("AP File ", id_visit, ":\n",
              "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
              "    Interval not found at all in Event File.\n",
              "    Fix entry or remove file.\n",
              call. = FALSE)
      
    }
    
    next()
    
  } else if (int_sec_worn != int_sec_applied_all) {
    
    # on intervals are in AP (IMG) but one on interval overlaps another interval.
    message("\n")
    warning("AP File ", id_visit, ":\n",
            "    Event file and on_off_log entries partially match.\n",
            "    One or more log entries overlap each other.\n",
            "    Check on_off entries.\n",
            call. = FALSE)
    
    next()
    
  }
  
  # STEP 4 : Clean (AVSA Specific) ----
  message(
    "cleaning...\n",
    appendLF = TRUE
  )
  
  # make file with off time cleaned out. REMEMBER! This still includes
  # the "zero" seconds of each on interval as it is still needed.
  tib_ap_vis <- tib_ap_sbs[tib_ap_sbs$off == 0, ]
  tib_ap_vis$id <- id
  tib_ap_vis$visit <- visit
  tib_ap_vis <- 
    tib_ap_vis[ , c("id",
                    "visit",
                    "time",
                    "ap_posture")]
  
  # write data frame
  fnm_ap_clean <- 
    paste(
      "FLAC",
      id_visit,
      sep = "_"
    ) %>% 
    paste0(".csv")
  vroom::vroom_write(
    tib_ap_vis,
    path = paste(
      fpa_ap_clean,
      fnm_ap_clean,
      sep = "/"
    ),
    delim = ",",
    # bom = TRUE,
    progress = FALSE
  )
  
  cnt_ap <- cnt_ap + 1
  
}

message(
  "--------------------------------Done Processing--------------------------------\n",
  "                              ", cnt_ap - 1, " files processed.\n",
  "\n",
  "File times are in UTC.\n",
  appendLF = TRUE
)



# testing daylight conversion -----------------------------------------------------
on_off_log <- "visit_on_off_log.csv"

# test1
nsavings_test <- "1053V1-AP740068 12Aug10 11-03am for 4h 43m-AOSD-CL08090134-Events.csv" #yes-no
ysavings_test <- "1074V1-AP740064 9Nov10 12-00pm for 3h 29m-AOSD-CL08090134-Events.csv" #no-yes

# test2
nsavings_test <- "1052V1-AP740072 10Aug10 2-59pm for 5h 13m-AOSD-CL08090134-Events.csv" #yes-no
ysavings_test <- "1052V3-AP740067 17Sep10 4-44pm for 4h 49m-AOSD-CL08090134-Events.csv" #yes-yes

# test3
nsavings_test <- "1085V1-AP740074 4Dec10 3-39pm for 3h 54m-AOSD-CL08090134-Events.csv" #no-yes
ysavings_test <- "1115V2-AP740083 2Apr11 6-18pm for 3h 22m-AOSD-CL08090134-Events.csv" #yes-yes

# test4
nsavings_test <- "1100V2-AP740067 3Feb11 10-32am for 3h 39m-AOSD-CL08090134-Events.csv"  #n-y
ysavings_test <- "4121V1-AP740081 21May11 11-36am for 4h 26m-AOSD-CL08090134-Events.csv" #y-n

# test5
nsavings_test <- "7101V1-AP740081 11Feb11 2-28pm for 5h 47m-AOSD-CL08090134-Events.csv"  #n-y
ysavings_test <- "5114V1-AP740074 29Mar11 10-34am for 4h 44m-AOSD-CL08090134-Events.csv" #y-y

# test6
nsavings_test <- "1042V2-AP740063 4Jun10 10-21am for 6h 23m-AOSD-CL08090134-Events.csv"  #y-n
ysavings_test <- "1068V2-AP740072 23Oct10 12-28pm for 4h 40m-AOSD-CL08090134-Events.csv" #y-y

# test7
nsavings_test <- "1087V3-AP740072 12Jan11 3-45pm for 3h 58m-AOSD-CL08090134-Events.csv"  #n-y
ysavings_test <- "1042V3-AP740068 23Jul10 11-28am for 5h 41m-AOSD-CL08090134-Events.csv" #y-n

# test8
nsavings_test <- "1118V3-AP740065 26Apr11 10-45am for 3h 56m-AOSD-CL08090134-Events.csv" #y-y
ysavings_test <- "3065V3-AP740072 6Oct10 10-52am for 4h 4m-AOSD-CL08090134-Events.csv"   #y-y

# read in on off log and clean
{
log <- read.table(file = paste0("./3_data/raw/", on_off_log),
                  header = T,
                  sep = ",",
                  stringsAsFactors = F)

log$date_on <- paste(log$date_on_month,
                     log$date_on_day,
                     log$date_on_year,
                     sep="/")
log$time_on <- paste(log$time_on_hour,
                     log$time_on_minute,
                     log$time_on_seconds,
                     sep=":")
log$date_off <- paste(log$date_off_month,
                      log$date_off_day,
                      log$date_off_year,
                      sep="/")
log$time_off <- paste(log$time_off_hour,
                      log$time_off_minute,
                      log$time_off_seconds,
                      sep=":")
log$date_time_on <- paste(log$date_on,
                          log$time_on,
                          sep=" ")
log$date_time_off <- paste(log$date_off,
                           log$time_off,
                           sep=" ")
log$date_time_on <- strptime(log$date_time_on,
                             "%m/%d/%Y %H:%M:%S")
log$date_time_off <- strptime(log$date_time_off,
                              "%m/%d/%Y %H:%M:%S")
log$date_time_on <- force_tz(log$date_time_on,
                             tz = "America/Chicago")
log$date_time_off <- force_tz(log$date_time_off,
                              tz = "America/Chicago")
}

########## no savings
{
nsav_ap <- read.table(file = paste0("./3_data/raw/events/", nsavings_test),
                     header = T,
                     sep = ",",
                     stringsAsFactors = F)
nsav_ap <- nsav_ap[,(1:6)]
names(nsav_ap) <- c("time",
                   "datacount",
                   "interval",
                   "activity",
                   "cumulativesteps",
                   "methrs")

# Change from Julian time to GMT
nsav_ap$time <- sub("#", "", nsav_ap$time)
nsav_ap$time <- sub("#", "", nsav_ap$time)
nsav_ap[,2] <- as.numeric(as.character(nsav_ap[,2]))
nsav_ap[,3] <- as.numeric(as.character(nsav_ap[,3]))
nsav_ap[,4] <- as.numeric(as.character(nsav_ap[,4]))
nsav_ap[,5] <- as.numeric(as.character(nsav_ap[,5]))*2 #event files have half the actual number of steps for some reason
nsav_ap[,6] <- as.numeric(as.character(nsav_ap[,6]))

t <- dim(nsav_ap)[1]
nsav_ap <- nsav_ap[!(nsav_ap[,"time"] == "1899-12-30"), ]
nsav_ap <- nsav_ap[!(nsav_ap[,"time"] == "0"), ]
n <- dim(nsav_ap)[1]		
  
nsav_ap$time <- as.numeric(nsav_ap$time)
nsav_ap$time <- as.POSIXct(as.Date(nsav_ap$time,
                                   origin = "1899-12-30"))
nsav_ap$time <- as.POSIXlt(nsav_ap$time,
                           tz = "UTC")

# for some reason, converting to UTC actually makes it relevant time zone
nsav_ap$time <- force_tz(nsav_ap$time,
                         tz = "America/Chicago")
nsav_ap$time <- strptime(nsav_ap$time,
                         format = "%Y-%m-%d %H:%M:%S")
nsav_ap$time <- nsav_ap$time + 3106.8918*24*60*60 ### CORRECTION FACTOR ###
}

######### yes savings
{
ysav_ap <- read.table(file = paste0("./3_data/raw/events/", ysavings_test),
                      header = T,
                      sep = ",",
                      stringsAsFactors = F)
ysav_ap <- ysav_ap[,(1:6)]
names(ysav_ap) <- c("time",
                    "datacount",
                    "interval",
                    "activity",
                    "cumulativesteps",
                    "methrs")

# Change from Julian time to GMT
ysav_ap$time <- sub("#", "", ysav_ap$time)
ysav_ap$time <- sub("#", "", ysav_ap$time)
ysav_ap[,2] <- as.numeric(as.character(ysav_ap[,2]))
ysav_ap[,3] <- as.numeric(as.character(ysav_ap[,3]))
ysav_ap[,4] <- as.numeric(as.character(ysav_ap[,4]))
ysav_ap[,5] <- as.numeric(as.character(ysav_ap[,5]))*2 #event files have half the actual number of steps for some reason
ysav_ap[,6] <- as.numeric(as.character(ysav_ap[,6]))

t <- dim(ysav_ap)[1]
ysav_ap <- ysav_ap[!(ysav_ap[,"time"] == "1899-12-30"), ]
ysav_ap <- ysav_ap[!(ysav_ap[,"time"] == "0"), ]
n <- dim(ysav_ap)[1]		
  
ysav_ap$time <- as.numeric(ysav_ap$time)
ysav_ap$time <- as.POSIXct(as.Date(ysav_ap$time,
                                   origin = "1899-12-30"))
ysav_ap$time <- as.POSIXlt(ysav_ap$time,
                           tz = "UTC")

# for some reason, converting to UTC actually makes it relevant time zone
ysav_ap$time <- force_tz(ysav_ap$time,
                         tz = "America/Chicago")
ysav_ap$time <- strptime(ysav_ap$time,
                         format = "%Y-%m-%d %H:%M:%S")
ysav_ap$time <- ysav_ap$time + 3106.8918*24*60*60 ### CORRECTION FACTOR ###
}



# process_irr - test 1 ---------------------------------------------------------------------

list_irr <- list.files("./3_data/raw/IRR",
                       ".csv")
anno_file_list <- list_irr

# sbs function for code times
sbs <- function(i) {
  
  new <- seq.POSIXt(mer_anno$NEWstarttime[i],
                    mer_anno$NEWendtime[i], by = "sec")
  annotation <- rep(mer_anno$annotation[i],
                    length(new))
  data.frame(time = new,
             annotation = annotation)
  
}

# create processed anno files
for (i in seq_along(anno_file_list)) {
  
  file_name <- anno_file_list[1]
  
  message("\nProcessing ", file_name, "...")
  
  # difference
  raw_anno <- read.table(file = paste0("./3_data/raw/IRR/", file_name),
                         header = T,
                         sep = ",")
  
  raw_anno$startTime <- ymd_hms(raw_anno$startTime,
                                tz="UTC")
  raw_anno$endTime <- ymd_hms(raw_anno$endTime,
                              tz="UTC")
  raw_anno$startTime <- with_tz(raw_anno$startTime,
                                tz = "America/Chicago")
  raw_anno$endTime <- with_tz(raw_anno$endTime,
                              tz = "America/Chicago")
  
  # merge times and raw
  id <- as.integer(substr(file_name, 6, 9))
  visit <- as.integer(substr(file_name, 11, 11))
  raw_anno$ID <- id
  raw_anno$Visit = visit
  mer_anno <- merge(raw_anno,
                    corr_times,
                    by = c("ID",
                           "Visit"))
  
  # check#1: See if timestamp was entered
  if (all(is.na(mer_anno$Difference))) {
    
    warning("\n",
            "\n",
            file_name, "annotation file does not have an entry in Timestamps.csv")
    
  } else {
    
    # add diff to times
    mer_anno <- mer_anno %>%
      mutate(NEWstarttime = if_else(!is.na(Difference),
                                    startTime + Difference,
                                    startTime))
    mer_anno <- mer_anno %>%
      mutate(NEWendtime = if_else(!is.na(Difference),
                                  endTime + Difference,
                                  endTime))
    
    # to POSIXlt for padding later 
    mer_anno$NEWstarttime <- strptime(mer_anno$NEWstarttime,
                                      format="%Y-%m-%d %H:%M:%OS")
    mer_anno$NEWendtime <- strptime(mer_anno$NEWendtime,
                                    format="%Y-%m-%d %H:%M:%OS")
    
    # sbs
    n <- nrow(mer_anno)
    l <- lapply(1:n, sbs)
    sbs_anno <- suppressMessages(Reduce(rbind, l) %>% 
                                   pad())
    
    # changing NA's to transition;gap
    levels <- levels(sbs_anno$annotation)
    levels[length(levels) + 1] <- "gap"
    sbs_anno$annotation <- factor(sbs_anno$annotation,
                                  levels = levels)
    sbs_anno$annotation[is.na(sbs_anno$annotation)] <- "gap"
    
    # on off times
    on_off <- on_off_log[on_off_log$ID == id & on_off_log$Visit == visit, ]
    on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
    off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")
    
    #	label off times
    sbs_anno$off <- 1
    n <- dim(sbs_anno)[1]
    index <- (1:n)[(sbs_anno$time >= on) & (sbs_anno$time <= off)]
    sbs_anno$off[index] <- 0
    
    # check#2: see if off times were actually labeled
    inds_worn <- (1:(dim(sbs_anno)[1]))[sbs_anno$off==0]
    i <- length(inds_worn)
    if(i == 0) {
      
      warning("\n",
              "\n",
              file_name, "timestamp or on_off_log entry incorrect")
      
    } else {
      
      # Clean - avsa specific
      vis_anno <- sbs_anno[sbs_anno$off == 0, ] # remove off times
      vis_anno$time <- as.POSIXct(vis_anno$time, 
                                  tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
      vis_anno$ID <- id # add in ID
      vis_anno$Visit <- visit # add in Visit number
      vis_anno$annotation <- as.character(vis_anno$annotation) #change to character for next step
      vis_anno$annotation[vis_anno$annotation == "posture;0006 sitting"] <- "0" 
      vis_anno$annotation[vis_anno$annotation == "posture;0007 standing"] <- "1" 
      vis_anno$annotation[vis_anno$annotation == "posture;0008 movement"] <- "2"
      vis_anno$annotation[vis_anno$annotation == "gap"] <- "3"
      vis_anno$annotation[!(vis_anno$annotation %in% c("0", "1", "2", "3"))] <- "4"
      vis_anno <-vis_anno[, c("ID",
                              "Visit",
                              "time",
                              "annotation")]
      
      # write table, difference compared to process_anno
      write.table(vis_anno,
                  file = paste0("./3_data/processed/irr_clean/",
                                file_name),
                  sep = ",",
                  row.names = F)
      
    }
  }
}



# merge_anno_ap - Test 1 --------------------------------------------------
test <- "1002V3.csv"

print(test)

vis_anno <- read_csv(file = paste0("./3_data/processed/anno_clean/", test),
                     col_names = T)
id_visit <- substr(test, 1, 6)
vis_ap <- read_csv(file = paste0("./3_data/processed/ap_clean/", id_visit, ".csv"),
                   col_names = T)
vis_merged <- inner_join(vis_anno, vis_ap)
write_csv(vis_merged,
          path = paste0("./3_data/analysis/merged_anno_ap/", id_visit, ".csv"))


# merge_anno_ap - Test 2 --------------------------------------------------
# test with 1013v2 since it had problems in the past
test <- "1013V2.csv"

print(test)

vis_anno <- read_csv(file = paste0("./3_data/processed/anno_clean/", test),
                     col_names = T)
l <- nrow(vis_anno)
id_visit <- substr(test, 1, 6)
vis_ap <- read_csv(file = paste0("./3_data/processed/ap_clean/", id_visit, ".csv"),
                   col_names = T)
vis_merged <- inner_join(vis_anno,
                         vis_ap,
                         by = c("ID", "Visit", "time"))
n <- nrow(vis_merged)

# Check #1
if (l == n) {
  
  vis_merged <- vis_merged[, -3]
  write_csv(vis_merged,
            path = paste0("./3_data/analysis/merged_anno_ap/", id_visit, ".csv"))
  
} else {
  
  warning(paste(id_visit, "annotation and AP file do not match in time",
                sep = " "))
  
}



# merg_irr - test 1 -------------------------------------------------------

list_irr <- list.files("./3_data/processed/irr_clean",
                               ".csv")

# list names of visits
index <- str_sub(list_irr,
                 start = 1,
                 end = 11) %>% 
  unique()

for (i in seq_along(index)) {
  
  # list files per visit
  list_visit <- list_irr[str_detect(list_irr,
                                    index[5])]
  
  # get id and visit for table
  id_visit <- str_sub(list_visit[1],
                      start = 1,
                      end = 11)

  for (i in seq_along(list_visit)) {
    
    file_name <- list_visit[i]
    
    # read in one visit file
    irr_img <- vroom(file = paste("./3_data/processed/irr_clean",
                                  file_name,
                                  sep = "/"),
                     delim = ",")
    
    irr_img <- unique(irr_img)
    
    # get coder name and rename annotation column after it
    coder <- sub(".*\\E_",
                 "",
                 file_name)
    coder <- sub("\\..*",
                 "",
                 coder)
    
    colnames(irr_img)[4] <- coder
    
    # put anno files in one list
    if (i == 1) {
      
      list_merge <- list(irr_img)
      
    } else if (i > 1) {
      
      list_merge[[i]] <- irr_img
      
    }
  }

  # merge anno's for one visit into one df
  irr_merged <- bind_cols(list_merge)
  
  # remove id, visit, time
  og <- colnames(irr_merged)[1:3]
  dup <- c(og,
           paste0(og, "1"),
           paste0(og, "2"),
           paste0(og, "3"))
  irr_clean <- irr_merged[, -which(colnames(irr_merged) %in% dup)]
  
  # krippendorff's alpha
  irr_kripp <- kripp.alpha(t(irr_clean),
                           method = "nominal")
  
  irr_gapless <- irr_clean[irr_clean$Chang != 3 & irr_clean$Miller != 3 & irr_clean$Smith != 3,]
  
  irr_kripp_gapless <- kripp.alpha(t(irr_gapless),
                                   method = "nominal")
  
  # make df
  irr_table <- data.frame(id_visit     = id_visit,
                          kripp_full   = irr_kripp$value,
                          krip_gapless = irr_kripp_gapless$value)
  
  # write table
  if (i == 1) {
    
    vroom_write(irr_table,
                path = "./4_results/irr_table.csv",
                delim = ",",
                append = F)
    
  } else if (i > 1) {
    
    vroom_write(irr_table,
                path = "./4_results/irr_table.csv",
                delim = ",",
                append = T)
    
  }
}



# analysis_avsa - Test 1 ---------------------------------------------------
test <- "1002V3.csv"

counter <- 1

# loop starts

print(test)

data_merged <- read_csv(file = paste0("./3_data/analysis/merged_anno_ap/",test),
                        col_names = T)
data_event <- data_merged[data_merged$annotation != 3, ]

# maybe???
write_csv(data_event,
          path = paste0("./3_data/analysis/event/", test, "_event.csv"),
          col_names = T)

# future tests
events.1sec$annotation <- as.factor(events.1sec$annotation)
events.1sec$ap.posture <- as.factor((events.1sec$ap.posture))
anno.levels <- levels(events.1sec$annotation)
l <- length(anno.levels)
ap.levels <- levels(events.1sec$ap.posture)
n <- length(ap.levels)

if (l==1){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels[length(anno.levels) + 1] <- "2"
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (l==2){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels <- sort(anno.levels)
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (n==1){
  ap.levels[length(ap.levels) + 1] <- "1"
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 
if (n==2){
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 

# times: visit, event, transition (all converted to minutes)
time_visit <- nrow(data_merged) %>% #
  as.integer()
time_visit <- time_visit/60

time_event <- data_merged[data_merged$annotation != 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_event <- time_event/60

time_trans <- data_merged[data_merged$annotation == 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_trans <- time_trans/60

# check to see event and transition equal data_merged. dont include in function
time_event + time_trans == nrow(data_merged)/60

# times: posture, agree & misclassification from confusion matrix
time_matr_event <- table(data_event$ap_posture, data_event$annotation) # rows = ap
time_matr_event <- addmargins(time_matr_event)
time_matr_event <- time_matr_event/60 
time_matr_event

time_ap_sit <- time_matr_event[1, 4] # posture times
time_ap_sta <- time_matr_event[2, 4]
time_ap_mov <- time_matr_event[3, 4]

time_anno_sit <- time_matr_event[4, 1]
time_anno_sta <- time_matr_event[4, 2]
time_anno_mov <- time_matr_event[4, 3]

time_agre_ss <- time_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
time_miss_st <- time_matr_event[1, 2] # "anno misclassified ap sitting as standing"
time_miss_sm <- time_matr_event[1, 3] # "anno misclassified ap sitting as movement"

time_miss_ts <- time_matr_event[2, 1]
time_agre_tt <- time_matr_event[2, 2] # "anno agrees with ap standing"
time_miss_tm <- time_matr_event[2, 3]

time_miss_ms <- time_matr_event[3, 1]
time_miss_mt <- time_matr_event[3, 2]
time_agre_mm <- time_matr_event[3, 3]

time_agre_total <- time_agre_ss + time_agre_tt + time_agre_mm 

# check
sum(data_merged$annotation == data_merged$ap_posture) # TRUE = agree, adds all sec they agree
time_agre_total == sum(data_merged$annotation == data_merged$ap_posture)/60

# time table
id <- data_merged$ID[1]
visit <- data_merged$Visit[1]

table_analysis_time <- data.frame(ID             = id,
                                  Visit          = visit,
                                  visit_time     = time_visit,
                                  event_time     = time_event,
                                  trans_time     = time_trans,
                                  sit_ap         = time_ap_sit,
                                  sit_anno       = time_anno_sit,
                                  stand_ap       = time_ap_sta,
                                  stand_anno     = time_anno_sta,
                                  move_ap        = time_ap_mov,
                                  move_anno      = time_anno_mov,
                                  sit_agree      = time_agre_ss,
                                  stand_agree    = time_agre_tt,
                                  move_agree     = time_agre_mm,
                                  sit_mis_stand  = time_miss_st,
                                  sit_mis_move   = time_miss_sm,
                                  stand_mis_sit  = time_miss_ts,
                                  stand_mis_move = time_miss_tm,
                                  move_mis_sit   = time_miss_ms,
                                  move_mis_stand = time_miss_mt)


# percentages: event, transition, total agreement and event agreement
perc_event <- time_event/time_visit*100 #
perc_trans <- time_trans/time_visit*100
perc_agre_total <-  time_agre_total/time_visit*100 #
perc_agre_event <-  time_agre_total/time_event*100 #

# percentages: agree and misclassification by ap posture from confusion matrix
perc_sit_row <- time_matr_event[1, ]/time_ap_sit # dividing by ap posture times, are = to
perc_sta_row <- time_matr_event[2, ]/time_ap_sta
perc_mov_row <- time_matr_event[3, ]/time_ap_mov

perc_matr_event <- rbind(perc_sit_row, perc_sta_row, perc_mov_row)
perc_matr_event <- perc_matr_event*100
perc_matr_event

perc_ap_sit <- time_ap_sit/time_event*100 # posture percentages of event time
perc_ap_sta <- time_ap_sta/time_event*100
perc_ap_mov <- time_ap_mov/time_event*100
perc_ap_sit + perc_ap_sta + perc_ap_mov == 100

perc_anno_sit <- time_anno_sit/time_event*100
perc_anno_sta <- time_anno_sta/time_event*100
perc_anno_mov <- time_anno_mov/time_event*100
perc_anno_sit + perc_anno_sta + perc_anno_mov == 100

perc_agre_ss <- perc_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
perc_miss_st <- perc_matr_event[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"
perc_miss_sm <- perc_matr_event[1, 3] # "anno misclassified ap sitting as movement ##% of ap sit time"

perc_miss_ts <- perc_matr_event[2, 1]
perc_agre_tt <- perc_matr_event[2, 2] # "anno agrees with ap standing ##% of ap standing time"
perc_miss_tm <- perc_matr_event[2, 3]

perc_miss_ms <- perc_matr_event[3, 1]
perc_miss_mt <- perc_matr_event[3, 2]
perc_agre_mm <- perc_matr_event[3, 3]

# percentage table
table_analysis_percentage <- data.frame(ID             = id,
                                        Visit          = visit,
                                        perc_event     = perc_event,
                                        perc_trans     = perc_trans,
                                        sit_ap         = perc_ap_sit,
                                        sit_anno       = perc_anno_sit,
                                        stand_ap       = perc_ap_sta,
                                        stand_anno     = perc_anno_sta,
                                        move_ap        = perc_ap_mov,
                                        move_anno      = perc_anno_mov,
                                        total_agree    = perc_agre_total,
                                        event_agree    = perc_agre_event,
                                        sit_agree      = perc_agre_ss,
                                        stand_agree    = perc_agre_tt,
                                        move_agree     = perc_agre_mm,
                                        sit_mis_stand  = perc_miss_st,
                                        sit_mis_move   = perc_miss_sm,
                                        stand_mis_sit  = perc_miss_ts,
                                        stand_mis_move = perc_miss_tm,
                                        move_mis_sit   = perc_miss_ms,
                                        move_mis_stand = perc_miss_mt)



if (counter == 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = T,
            append = F)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = T,
            append = F)
  
}

if (counter > 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = T,
            append = T)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = T,
            append = T)
  
}

counter <- counter+1



# analysis_avsa - Test 2 --------------------------------------------------
# testing appending to already made analysis tables
test <- "1009V1.csv"

# made counter 2 to add to already created table from test 1
counter <- 2

# loop starts

print(test)

data_merged <- read_csv(file = paste0("./3_data/analysis/merged_anno_ap/",test),
                        col_names = T)
data_event <- data_merged[data_merged$annotation != 3, ]

# maybe???
write_csv(data_event,
          path = paste0("./3_data/analysis/event/", test, "_event.csv"),
          col_names = T)

# future tests
events.1sec$annotation <- as.factor(events.1sec$annotation)
events.1sec$ap.posture <- as.factor((events.1sec$ap.posture))
anno.levels <- levels(events.1sec$annotation)
l <- length(anno.levels)
ap.levels <- levels(events.1sec$ap.posture)
n <- length(ap.levels)

if (l==1){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels[length(anno.levels) + 1] <- "2"
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (l==2){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels <- sort(anno.levels)
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (n==1){
  ap.levels[length(ap.levels) + 1] <- "1"
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 
if (n==2){
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 

# times: visit, event, transition (all converted to minutes)
time_visit <- nrow(data_merged) %>% #
  as.integer()
time_visit <- time_visit/60

time_event <- data_merged[data_merged$annotation != 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_event <- time_event/60

time_trans <- data_merged[data_merged$annotation == 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_trans <- time_trans/60

# check to see event and transition equal data_merged. dont include in function
time_event + time_trans == nrow(data_merged)/60

# times: posture, agree & misclassification from confusion matrix
time_matr_event <- table(data_event$ap_posture, data_event$annotation) # rows = ap
time_matr_event <- addmargins(time_matr_event)
time_matr_event <- time_matr_event/60 
time_matr_event

time_ap_sit <- time_matr_event[1, 4] # posture times
time_ap_sta <- time_matr_event[2, 4]
time_ap_mov <- time_matr_event[3, 4]

time_anno_sit <- time_matr_event[4, 1]
time_anno_sta <- time_matr_event[4, 2]
time_anno_mov <- time_matr_event[4, 3]

time_agre_ss <- time_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
time_miss_st <- time_matr_event[1, 2] # "anno misclassified ap sitting as standing"
time_miss_sm <- time_matr_event[1, 3] # "anno misclassified ap sitting as movement"

time_miss_ts <- time_matr_event[2, 1]
time_agre_tt <- time_matr_event[2, 2] # "anno agrees with ap standing"
time_miss_tm <- time_matr_event[2, 3]

time_miss_ms <- time_matr_event[3, 1]
time_miss_mt <- time_matr_event[3, 2]
time_agre_mm <- time_matr_event[3, 3]

time_agre_total <- time_agre_ss + time_agre_tt + time_agre_mm 

# check
sum(data_merged$annotation == data_merged$ap_posture)/60 # TRUE = agree, adds all sec they agree
time_agre_total == sum(data_merged$annotation == data_merged$ap_posture)/60

# time table
id <- data_merged$ID[1]
visit <- data_merged$Visit[1]

table_analysis_time <- data.frame(ID             = id,
                                  Visit          = visit,
                                  visit_time     = time_visit,
                                  event_time     = time_event,
                                  trans_time     = time_trans,
                                  sit_ap         = time_ap_sit,
                                  sit_anno       = time_anno_sit,
                                  stand_ap       = time_ap_sta,
                                  stand_anno     = time_anno_sta,
                                  move_ap        = time_ap_mov,
                                  move_anno      = time_anno_mov,
                                  sit_agree      = time_agre_ss,
                                  stand_agree    = time_agre_tt,
                                  move_agree     = time_agre_mm,
                                  sit_mis_stand  = time_miss_st,
                                  sit_mis_move   = time_miss_sm,
                                  stand_mis_sit  = time_miss_ts,
                                  stand_mis_move = time_miss_tm,
                                  move_mis_sit   = time_miss_ms,
                                  move_mis_stand = time_miss_mt)


# percentages: event, transition, total agreement and event agreement
perc_event <- time_event/time_visit*100 #
perc_trans <- time_trans/time_visit*100
perc_agre_total <-  time_agre_total/time_visit*100 #
perc_agre_event <-  time_agre_total/time_event*100 #

# percentages: agree and misclassification by ap posture from confusion matrix
perc_sit_row <- time_matr_event[1, ]/time_ap_sit # dividing by ap posture times, are = to
perc_sta_row <- time_matr_event[2, ]/time_ap_sta
perc_mov_row <- time_matr_event[3, ]/time_ap_mov

perc_matr_event <- rbind(perc_sit_row, perc_sta_row, perc_mov_row)
perc_matr_event <- perc_matr_event*100
perc_matr_event

perc_ap_sit <- time_ap_sit/time_event*100 # posture percentages of event time
perc_ap_sta <- time_ap_sta/time_event*100
perc_ap_mov <- time_ap_mov/time_event*100
perc_ap_sit + perc_ap_sta + perc_ap_mov == 100

perc_anno_sit <- time_anno_sit/time_event*100
perc_anno_sta <- time_anno_sta/time_event*100
perc_anno_mov <- time_anno_mov/time_event*100
perc_anno_sit + perc_anno_sta + perc_anno_mov == 100

perc_agre_ss <- perc_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
perc_miss_st <- perc_matr_event[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"
perc_miss_sm <- perc_matr_event[1, 3] # "anno misclassified ap sitting as movement ##% of ap sit time"

perc_miss_ts <- perc_matr_event[2, 1]
perc_agre_tt <- perc_matr_event[2, 2] # "anno agrees with ap standing ##% of ap standing time"
perc_miss_tm <- perc_matr_event[2, 3]

perc_miss_ms <- perc_matr_event[3, 1]
perc_miss_mt <- perc_matr_event[3, 2]
perc_agre_mm <- perc_matr_event[3, 3]

# percentage table
table_analysis_percentage <- data.frame(ID             = id,
                                        Visit          = visit,
                                        perc_event     = perc_event,
                                        perc_trans     = perc_trans,
                                        sit_ap         = perc_ap_sit,
                                        sit_anno       = perc_anno_sit,
                                        stand_ap       = perc_ap_sta,
                                        stand_anno     = perc_anno_sta,
                                        move_ap        = perc_ap_mov,
                                        move_anno      = perc_anno_mov,
                                        total_agree    = perc_agre_total,
                                        event_agree    = perc_agre_event,
                                        sit_agree      = perc_agre_ss,
                                        stand_agree    = perc_agre_tt,
                                        move_agree     = perc_agre_mm,
                                        sit_mis_stand  = perc_miss_st,
                                        sit_mis_move   = perc_miss_sm,
                                        stand_mis_sit  = perc_miss_ts,
                                        stand_mis_move = perc_miss_tm,
                                        move_mis_sit   = perc_miss_ms,
                                        move_mis_stand = perc_miss_mt)



if (counter == 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = T,
            append = F)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = T,
            append = F)
  
}

if (counter > 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = F,
            append = T)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = F,
            append = T)
  
}

counter <- counter+1



# analysis_avsa - Test 3 ----------------------------------------------------
# merged file that does not have one of the postures and fixing that
test <- "1052V2.csv"

# made counter 2 to add to already created table from test 1
counter <- 2

# loop starts

print(test)

data_merged <- read_csv(file = paste0("./3_data/analysis/merged_anno_ap/",test),
                        col_names = T)

data_event <- data_merged[data_merged$annotation != 3, ]

# future tests
data_event$annotation <- as.factor(data_event$annotation)
data_event$ap_posture <- as.factor((data_event$ap_posture))

anno_levels <- levels(data_event$annotation)
ap_levels <- levels(data_event$ap_posture)

if (length(anno_levels) < 3 || length(ap_levels) < 3) {
  
  event_levels <- union(anno_levels, ap_levels) %>% 
    as.integer() %>% 
    sort() %>% 
    paste()
    
  data_event$annotation <- factor(data_event$annotation,
                                  levels = event_levels)
  data_event$ap_posture<- factor(data_event$ap_posture,
                                 levels = event_levels)
  
} 

# times: visit, event, transition (all converted to minutes)
time_visit <- nrow(data_merged) %>% #
  as.integer()
time_visit <- time_visit/60

time_event <- data_merged[data_merged$annotation != 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_event <- time_event/60

time_trans <- data_merged[data_merged$annotation == 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_trans <- time_trans/60

# check to see event and transition equal data_merged. dont include in function
near(time_event + time_trans, nrow(data_merged)/60)

# times: posture, agree & misclassification from confusion matrix
time_matr_event <- table(data_event$ap_posture, data_event$annotation) # rows = ap
time_matr_event <- addmargins(time_matr_event)
time_matr_event <- time_matr_event/60 
time_matr_event

time_ap_sit <- time_matr_event[1, 4] # posture times
time_ap_sta <- time_matr_event[2, 4]
time_ap_mov <- time_matr_event[3, 4]

time_anno_sit <- time_matr_event[4, 1]
time_anno_sta <- time_matr_event[4, 2]
time_anno_mov <- time_matr_event[4, 3]

time_agre_ss <- time_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
time_miss_st <- time_matr_event[1, 2] # "anno misclassified ap sitting as standing"
time_miss_sm <- time_matr_event[1, 3] # "anno misclassified ap sitting as movement"

time_miss_ts <- time_matr_event[2, 1]
time_agre_tt <- time_matr_event[2, 2] # "anno agrees with ap standing"
time_miss_tm <- time_matr_event[2, 3]

time_miss_ms <- time_matr_event[3, 1]
time_miss_mt <- time_matr_event[3, 2]
time_agre_mm <- time_matr_event[3, 3]

time_agre_total <- time_agre_ss + time_agre_tt + time_agre_mm 

# check
sum(data_merged$annotation == data_merged$ap_posture)/60 # TRUE = agree, adds all sec they agree
time_agre_total == sum(data_merged$annotation == data_merged$ap_posture)/60

# time table
id <- data_merged$ID[1]
visit <- data_merged$Visit[1]

table_analysis_time <- data.frame(ID             = id,
                                  Visit          = visit,
                                  visit_time     = time_visit,
                                  event_time     = time_event,
                                  trans_time     = time_trans,
                                  sit_ap         = time_ap_sit,
                                  sit_anno       = time_anno_sit,
                                  stand_ap       = time_ap_sta,
                                  stand_anno     = time_anno_sta,
                                  move_ap        = time_ap_mov,
                                  move_anno      = time_anno_mov,
                                  sit_agree      = time_agre_ss,
                                  stand_agree    = time_agre_tt,
                                  move_agree     = time_agre_mm,
                                  sit_mis_stand  = time_miss_st,
                                  sit_mis_move   = time_miss_sm,
                                  stand_mis_sit  = time_miss_ts,
                                  stand_mis_move = time_miss_tm,
                                  move_mis_sit   = time_miss_ms,
                                  move_mis_stand = time_miss_mt)


# percentages: event, transition, total agreement and event agreement
perc_event <- time_event/time_visit*100 #
perc_trans <- time_trans/time_visit*100
perc_agre_total <-  time_agre_total/time_visit*100 #
perc_agre_event <-  time_agre_total/time_event*100 #

# percentages: agree and misclassification by ap posture from confusion matrix
perc_sit_row <- time_matr_event[1, ]/time_ap_sit # dividing by ap posture times, are = to
perc_sta_row <- time_matr_event[2, ]/time_ap_sta
perc_mov_row <- time_matr_event[3, ]/time_ap_mov

perc_matr_event <- rbind(perc_sit_row, perc_sta_row, perc_mov_row)
perc_matr_event <- perc_matr_event*100
perc_matr_event

perc_ap_sit <- time_ap_sit/time_event*100 # posture percentages of event time
perc_ap_sta <- time_ap_sta/time_event*100
perc_ap_mov <- time_ap_mov/time_event*100
perc_ap_sit + perc_ap_sta + perc_ap_mov == 100

perc_anno_sit <- time_anno_sit/time_event*100
perc_anno_sta <- time_anno_sta/time_event*100
perc_anno_mov <- time_anno_mov/time_event*100
perc_anno_sit + perc_anno_sta + perc_anno_mov == 100

perc_agre_ss <- perc_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
perc_miss_st <- perc_matr_event[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"
perc_miss_sm <- perc_matr_event[1, 3] # "anno misclassified ap sitting as movement ##% of ap sit time"

perc_miss_ts <- perc_matr_event[2, 1]
perc_agre_tt <- perc_matr_event[2, 2] # "anno agrees with ap standing ##% of ap standing time"
perc_miss_tm <- perc_matr_event[2, 3]

perc_miss_ms <- perc_matr_event[3, 1]
perc_miss_mt <- perc_matr_event[3, 2]
perc_agre_mm <- perc_matr_event[3, 3]

# percentage table
table_analysis_percentage <- data.frame(ID             = id,
                                        Visit          = visit,
                                        perc_event     = perc_event,
                                        perc_trans     = perc_trans,
                                        sit_ap         = perc_ap_sit,
                                        sit_anno       = perc_anno_sit,
                                        stand_ap       = perc_ap_sta,
                                        stand_anno     = perc_anno_sta,
                                        move_ap        = perc_ap_mov,
                                        move_anno      = perc_anno_mov,
                                        total_agree    = perc_agre_total,
                                        event_agree    = perc_agre_event,
                                        sit_agree      = perc_agre_ss,
                                        stand_agree    = perc_agre_tt,
                                        move_agree     = perc_agre_mm,
                                        sit_mis_stand  = perc_miss_st,
                                        sit_mis_move   = perc_miss_sm,
                                        stand_mis_sit  = perc_miss_ts,
                                        stand_mis_move = perc_miss_tm,
                                        move_mis_sit   = perc_miss_ms,
                                        move_mis_stand = perc_miss_mt)



if (counter == 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = T,
            append = F)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = T,
            append = F)
  
}

if (counter > 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = F,
            append = T)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = F,
            append = T)
  
}

counter <- counter+1



# analysis_avsa - Test 4 --------------------------------------------------
# merged file where both anno and ap do not have one of the postures and fixing that
test <- "1074V2.csv"

# made counter 2 to add to already created table from test 1
counter <- 2

# loop starts

print(test)

data_merged <- read_csv(file = paste0("./3_data/analysis/merged_anno_ap/",test),
                        col_names = T)

data_event <- data_merged[data_merged$annotation != 3, ]

# future tests
data_event$annotation <- as.factor(data_event$annotation)
data_event$ap_posture <- as.factor((data_event$ap_posture))

anno_levels <- levels(data_event$annotation)
ap_levels <- levels(data_event$ap_posture)

if (length(anno_levels) < 3 || length(ap_levels) < 3) {
  
  event_levels <- union(anno_levels, ap_levels) %>% 
    as.integer() %>% 
    sort() %>% 
    paste()
  
  if (all(event_levels == c("0", "1", "2"))) {
    
    data_event$annotation <- factor(data_event$annotation,
                                    levels = event_levels)
    data_event$ap_posture<- factor(data_event$ap_posture,
                                   levels = event_levels)
    
  } else if (all(event_levels == c("1", "2"))) {
    
    event_levels[length(event_levels) + 1] <- "3"
    data_event$annotation <- factor(data_event$annotation,
                                    levels = event_levels)
    data_event$ap_posture<- factor(data_event$ap_posture,
                                   levels = event_levels)
    
  } 
}

paste
names(anno_levels)
paste0(anno_levels, collapse = "")
switch(EXPR = anno_levels,
       "0" = )

levels(data_event$annotation)
levels(data_event$ap_posture)

anno_levels <- paste(sort(as.integer(union(anno_levels, ap_levels))))
union(anno_levels, ap_levels)
levels <- union(anno_levels, ap_levels)
levels <- order(union(anno_levels, ap_levels))
lev <- factor()

l <- length(anno_levels)
n <- length(ap_levels)

if (l==1){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels[length(anno.levels) + 1] <- "2"
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (l==2){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels <- sort(anno.levels)
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (n==1){
  ap.levels[length(ap.levels) + 1] <- "1"
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 
if (n==2){
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 

# times: visit, event, transition (all converted to minutes)
time_visit <- nrow(data_merged) %>% #
  as.integer()
time_visit <- time_visit/60

time_event <- data_merged[data_merged$annotation != 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_event <- time_event/60

time_trans <- data_merged[data_merged$annotation == 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_trans <- time_trans/60

# check to see event and transition equal data_merged. dont include in function
near(time_event + time_trans, nrow(data_merged)/60)

# times: posture, agree & misclassification from confusion matrix
time_matr_event <- table(data_event$ap_posture, data_event$annotation) # rows = ap
time_matr_event <- addmargins(time_matr_event)
time_matr_event <- time_matr_event/60 
time_matr_event

time_ap_sit <- time_matr_event[1, 4] # posture times
time_ap_sta <- time_matr_event[2, 4]
time_ap_mov <- time_matr_event[3, 4]

time_anno_sit <- time_matr_event[4, 1]
time_anno_sta <- time_matr_event[4, 2]
time_anno_mov <- time_matr_event[4, 3]

time_agre_ss <- time_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
time_miss_st <- time_matr_event[1, 2] # "anno misclassified ap sitting as standing"
time_miss_sm <- time_matr_event[1, 3] # "anno misclassified ap sitting as movement"

time_miss_ts <- time_matr_event[2, 1]
time_agre_tt <- time_matr_event[2, 2] # "anno agrees with ap standing"
time_miss_tm <- time_matr_event[2, 3]

time_miss_ms <- time_matr_event[3, 1]
time_miss_mt <- time_matr_event[3, 2]
time_agre_mm <- time_matr_event[3, 3]

time_agre_total <- time_agre_ss + time_agre_tt + time_agre_mm 

# check
sum(data_merged$annotation == data_merged$ap_posture)/60 # TRUE = agree, adds all sec they agree
time_agre_total == sum(data_merged$annotation == data_merged$ap_posture)/60

# time table
id <- data_merged$ID[1]
visit <- data_merged$Visit[1]

table_analysis_time <- data.frame(ID             = id,
                                  Visit          = visit,
                                  visit_time     = time_visit,
                                  event_time     = time_event,
                                  trans_time     = time_trans,
                                  sit_ap         = time_ap_sit,
                                  sit_anno       = time_anno_sit,
                                  stand_ap       = time_ap_sta,
                                  stand_anno     = time_anno_sta,
                                  move_ap        = time_ap_mov,
                                  move_anno      = time_anno_mov,
                                  sit_agree      = time_agre_ss,
                                  stand_agree    = time_agre_tt,
                                  move_agree     = time_agre_mm,
                                  sit_mis_stand  = time_miss_st,
                                  sit_mis_move   = time_miss_sm,
                                  stand_mis_sit  = time_miss_ts,
                                  stand_mis_move = time_miss_tm,
                                  move_mis_sit   = time_miss_ms,
                                  move_mis_stand = time_miss_mt)


# percentages: event, transition, total agreement and event agreement
perc_event <- time_event/time_visit*100 #
perc_trans <- time_trans/time_visit*100
perc_agre_total <-  time_agre_total/time_visit*100 #
perc_agre_event <-  time_agre_total/time_event*100 #

# percentages: agree and misclassification by ap posture from confusion matrix
perc_sit_row <- time_matr_event[1, ]/time_ap_sit # dividing by ap posture times, are = to
perc_sta_row <- time_matr_event[2, ]/time_ap_sta
perc_mov_row <- time_matr_event[3, ]/time_ap_mov

perc_matr_event <- rbind(perc_sit_row, perc_sta_row, perc_mov_row)
perc_matr_event <- perc_matr_event*100
perc_matr_event

perc_ap_sit <- time_ap_sit/time_event*100 # posture percentages of event time
perc_ap_sta <- time_ap_sta/time_event*100
perc_ap_mov <- time_ap_mov/time_event*100
perc_ap_sit + perc_ap_sta + perc_ap_mov == 100

perc_anno_sit <- time_anno_sit/time_event*100
perc_anno_sta <- time_anno_sta/time_event*100
perc_anno_mov <- time_anno_mov/time_event*100
perc_anno_sit + perc_anno_sta + perc_anno_mov == 100

perc_agre_ss <- perc_matr_event[1, 1] # last two letters: firs is ap, second is anno, t = stand
perc_miss_st <- perc_matr_event[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"
perc_miss_sm <- perc_matr_event[1, 3] # "anno misclassified ap sitting as movement ##% of ap sit time"

perc_miss_ts <- perc_matr_event[2, 1]
perc_agre_tt <- perc_matr_event[2, 2] # "anno agrees with ap standing ##% of ap standing time"
perc_miss_tm <- perc_matr_event[2, 3]

perc_miss_ms <- perc_matr_event[3, 1]
perc_miss_mt <- perc_matr_event[3, 2]
perc_agre_mm <- perc_matr_event[3, 3]

# percentage table
table_analysis_percentage <- data.frame(ID             = id,
                                        Visit          = visit,
                                        perc_event     = perc_event,
                                        perc_trans     = perc_trans,
                                        sit_ap         = perc_ap_sit,
                                        sit_anno       = perc_anno_sit,
                                        stand_ap       = perc_ap_sta,
                                        stand_anno     = perc_anno_sta,
                                        move_ap        = perc_ap_mov,
                                        move_anno      = perc_anno_mov,
                                        total_agree    = perc_agre_total,
                                        event_agree    = perc_agre_event,
                                        sit_agree      = perc_agre_ss,
                                        stand_agree    = perc_agre_tt,
                                        move_agree     = perc_agre_mm,
                                        sit_mis_stand  = perc_miss_st,
                                        sit_mis_move   = perc_miss_sm,
                                        stand_mis_sit  = perc_miss_ts,
                                        stand_mis_move = perc_miss_tm,
                                        move_mis_sit   = perc_miss_ms,
                                        move_mis_stand = perc_miss_mt)



if (counter == 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = T,
            append = F)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = T,
            append = F)
  
}

if (counter > 1) {
  
  write_csv(table_analysis_time,
            path = "./3_data/analysis/table_analysis_time.csv",
            col_names = F,
            append = T)
  
  write_csv(table_analysis_percentage,
            path = "./3_data/analysis/table_analysis_percentage.csv",
            col_names = F,
            append = T)
  
}

counter <- counter+1

### merged count files ###
counts.list.1sec <- list.files("./counts/", "1sec.csv")
counts.1sec <- lapply(counts.list.1sec, read.csv, header=T)
counts.1sec <- do.call(rbind, counts.1sec)



# analysis_avsa - Test 5 --------------------------------------------------
# testing everything works after including gaps

list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv")
merged_list <- list_merged

# tests
file_name <- merged_list[1] # 1002v3 NORMAL  
file_name <- merged_list[16] # 1052v2 NO MOVING - GOOD
file_name <- merged_list[22] # 1074v2 NO SITTING - GOOD
file_name <- merged_list[26] # 1085v3 NO SITTING - GOOD

message("\nPreparing ", file_name, "...")

# read in merged file
data_merged <- suppressMessages(vroom(file = paste0("./3_data/analysis/merged_anno_ap/",
                                   file_name),
                     delim = ","))

# remove gaps to create trans and both gaps/transitions to creat event
data_trans <- data_merged[data_merged$annotation != 3, ]
data_event <- data_trans[data_trans$annotation != 4, ]

# fixpoint#1: if a file does not have a posture
data_trans$annotation <- as.factor(data_trans$annotation)
data_trans$ap_posture <- as.factor((data_trans$ap_posture))

anno_levels <- levels(data_trans$annotation)
ap_levels <- levels(data_trans$ap_posture)

if (length(anno_levels) < 4 || length(ap_levels) < 4) {
  
  event_levels <- union(anno_levels, ap_levels) %>% 
    as.integer() %>% 
    sort() %>% 
    paste()
  
  # if event_levels has all postures
  if (all(c("0", "1", "2", "4") %in% event_levels)) {
    
    data_trans$annotation <- factor(data_trans$annotation,
                                    levels = event_levels)
    data_trans$ap_posture<- factor(data_trans$ap_posture,
                                   levels = event_levels)
    
    # if there is no sitting in both anno and ap
  } else if (all(c("1", "2", "4") %in% event_levels)) {
    
    event_levels[length(event_levels) + 1] <- "0"
    event_levels <- as.integer(event_levels) %>% 
      sort() %>% 
      paste()
    
    data_trans$annotation <- factor(data_trans$annotation,
                                    levels = event_levels)
    data_trans$ap_posture<- factor(data_trans$ap_posture,
                                   levels = event_levels)
    
  } 
}    

# fixpoint for event
data_event$annotation <- as.factor(data_event$annotation)
data_event$ap_posture <- as.factor((data_event$ap_posture))

anno_levels <- levels(data_event$annotation)
ap_levels <- levels(data_event$ap_posture)

if (length(anno_levels) < 3 || length(ap_levels) < 3) {
  
  event_levels <- union(anno_levels, ap_levels) %>% 
    as.integer() %>% 
    sort() %>% 
    paste()
  
  # if event_levels has all postures
  if (all(c("0", "1", "2") %in% event_levels)) {
    
    data_event$annotation <- factor(data_event$annotation,
                                    levels = event_levels)
    data_event$ap_posture<- factor(data_event$ap_posture,
                                   levels = event_levels)
    
    # if there is no sitting in both anno and ap
  } else if (all(c("1", "2") %in% event_levels)) {
    
    event_levels[length(event_levels) + 1] <- "0"
    event_levels <- as.integer(event_levels) %>% 
      sort() %>% 
      paste()
    
    data_event$annotation <- factor(data_event$annotation,
                                    levels = event_levels)
    data_event$ap_posture<- factor(data_event$ap_posture,
                                   levels = event_levels)
    
  } 
}    

# times: visit, event, transition (all converted to minutes)
time_visit <- nrow(data_merged) %>% #
  as.integer()
time_visit <- time_visit/60

time_event <- data_event %>% #
  nrow(.) %>% 
  as.integer()
time_event <- time_event/60

time_trans <- data_merged[data_merged$annotation == 4, ] %>% #
  nrow(.) %>% 
  as.integer()
time_trans <- time_trans/60

time_gap <- data_merged[data_merged$annotation == 3, ] %>% #
  nrow(.) %>% 
  as.integer()
time_gap <- time_gap/60

# check to see event and transition equal data_merged. dont include in function
all.equal(time_event + time_gap + time_trans,
          nrow(data_merged)/60)

# times: posture, agree & misclassification from confusion matrix
time_matr_event <- table(data_event$ap_posture, data_event$annotation) # rows = ap
time_matr_event <- addmargins(time_matr_event)
time_matr_event <- time_matr_event/60 
time_matr_event

time_ap_sit <- time_matr_event[1, 4] # posture times
time_ap_sta <- time_matr_event[2, 4]
time_ap_mov <- time_matr_event[3, 4]

time_anno_sit <- time_matr_event[4, 1]
time_anno_sta <- time_matr_event[4, 2]
time_anno_mov <- time_matr_event[4, 3]

time_agre_ss <- time_matr_event[1, 1] # last two letters: first is ap, second is anno, d = stand, t = trans
time_miss_sd <- time_matr_event[1, 2] # "anno misclassified ap sitting as standing"
time_miss_sm <- time_matr_event[1, 3] # "anno misclassified ap sitting as movement"

time_miss_ds <- time_matr_event[2, 1]
time_agre_dd <- time_matr_event[2, 2] # "anno agrees with ap standing"
time_miss_dm <- time_matr_event[2, 3]

time_miss_ms <- time_matr_event[3, 1]
time_miss_md <- time_matr_event[3, 2]
time_agre_mm <- time_matr_event[3, 3]

time_matr_trans <- (table(data_trans$ap_posture, data_trans$annotation) %>% 
                      addmargins())/60
time_matr_trans
time_miss_st <- time_matr_trans[1, 4] # "transition time when there is ap sitting"
time_miss_dt <- time_matr_trans[2, 4]
time_miss_mt <- time_matr_trans[3, 4]

time_agre_total <- time_agre_ss + time_agre_dd + time_agre_mm 

# check
sum(data_merged$annotation == data_merged$ap_posture)/60 # TRUE = agree, adds all sec they agree
all.equal(time_agre_total,
          sum(data_merged$annotation == data_merged$ap_posture)/60)

# time table
id <- data_merged$ID[1]
visit <- data_merged$Visit[1]

table_analysis_time <- data.frame(ID             = id,
                                  Visit          = visit,
                                  visit_time     = time_visit,
                                  event_time     = time_event,
                                  gap_time       = time_gap,
                                  trans_time     = time_trans,
                                  total_agree    = time_agre_total,
                                  event_agree    = time_agre_total,
                                  sit_ap         = time_ap_sit,
                                  sit_anno       = time_anno_sit,
                                  stand_ap       = time_ap_sta,
                                  stand_anno     = time_anno_sta,
                                  move_ap        = time_ap_mov,
                                  move_anno      = time_anno_mov,
                                  sit_agree      = time_agre_ss,
                                  stand_agree    = time_agre_dd,
                                  move_agree     = time_agre_mm,
                                  sit_trans      = time_miss_st,
                                  stand_trans    = time_miss_dt,
                                  move_trans     = time_miss_mt,
                                  sit_mis_stand  = time_miss_sd,
                                  sit_mis_move   = time_miss_sm,
                                  stand_mis_sit  = time_miss_ds,
                                  stand_mis_move = time_miss_dm,
                                  move_mis_sit   = time_miss_ms,
                                  move_mis_stand = time_miss_md)

# percentages: event, transition, total agreement and event agreement
perc_event <- time_event/time_visit*100 #
perc_trans <- time_trans/time_visit*100
perc_gap   <- time_gap/time_visit*100
perc_agre_total <-  time_agre_total/time_visit*100 #
perc_agre_event <-  time_agre_total/time_event*100 #

# percentages: agree and misclassification by ap posture from confusion matrix
perc_sit_row <- time_matr_event[1, ]/time_ap_sit # dividing by ap posture times, are = to
perc_sta_row <- time_matr_event[2, ]/time_ap_sta
perc_mov_row <- time_matr_event[3, ]/time_ap_mov

perc_matr_event <- rbind(perc_sit_row, perc_sta_row, perc_mov_row)
perc_matr_event <- perc_matr_event*100
perc_matr_event

perc_ap_sit <- time_ap_sit/time_event*100 # posture percentages of event time
perc_ap_sta <- time_ap_sta/time_event*100
perc_ap_mov <- time_ap_mov/time_event*100
perc_ap_sit + perc_ap_sta + perc_ap_mov == 100

perc_anno_sit <- time_anno_sit/time_event*100
perc_anno_sta <- time_anno_sta/time_event*100
perc_anno_mov <- time_anno_mov/time_event*100
all.equal(perc_anno_sit + perc_anno_sta + perc_anno_mov,
          100)

perc_agre_ss <- perc_matr_event[1, 1] # last two letters: firs is ap, second is anno, d = stand, t = trans
perc_miss_sd <- perc_matr_event[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"
perc_miss_sm <- perc_matr_event[1, 3] # "anno misclassified ap sitting as movement ##% of ap sit time"


perc_miss_ds <- perc_matr_event[2, 1]
perc_agre_dd <- perc_matr_event[2, 2] # "anno agrees with ap standing ##% of ap standing time"
perc_miss_dm <- perc_matr_event[2, 3]


perc_miss_ms <- perc_matr_event[3, 1]
perc_miss_md <- perc_matr_event[3, 2]
perc_agre_mm <- perc_matr_event[3, 3]

perc_matr_trans <- (time_matr_trans[, 4]/time_matr_trans[, 5])*100
perc_miss_st <- perc_matr_trans[1] # "##% of TOTAL (non-event) ap sit time classified as transition"
perc_miss_dt <- perc_matr_trans[2]
perc_miss_mt <- perc_matr_trans[3]

# percentage table
table_analysis_percentage <- data.frame(ID             = id,
                                        Visit          = visit,
                                        event_time     = perc_event,
                                        gap_time       = perc_gap,
                                        trans_time     = perc_trans,
                                        total_agree    = perc_agre_total,
                                        event_agree    = perc_agre_event,
                                        sit_ap         = perc_ap_sit,
                                        sit_anno       = perc_anno_sit,
                                        stand_ap       = perc_ap_sta,
                                        stand_anno     = perc_anno_sta,
                                        move_ap        = perc_ap_mov,
                                        move_anno      = perc_anno_mov,
                                        sit_agree      = perc_agre_ss,
                                        stand_agree    = perc_agre_dd,
                                        move_agree     = perc_agre_mm,
                                        sit_trans      = perc_miss_st,
                                        stand_trans    = perc_miss_dt,
                                        move_trans     = perc_miss_mt,
                                        sit_mis_stand  = perc_miss_sd,
                                        sit_mis_move   = perc_miss_sm,
                                        stand_mis_sit  = perc_miss_ds,
                                        stand_mis_move = perc_miss_dm,
                                        move_mis_sit   = perc_miss_ms,
                                        move_mis_stand = perc_miss_md)

if (counter == 1) {
  
  vroom_write(table_analysis_time,
              path = "./3_data/analysis/table_analysis_time.csv",
              delim = ",",
              append = F)
  
  vroom_write(table_analysis_percentage,
              path = "./3_data/analysis/table_analysis_percentage.csv",
              delim = ",",
              append = F)
  
}

if (counter > 1) {
  
  vroom_write(table_analysis_time,
              path = "./3_data/analysis/table_analysis_time.csv",
              delim = ",",
              append = T)
  
  vroom_write(table_analysis_percentage,
              path = "./3_data/analysis/table_analysis_percentage.csv",
              delim = ",",
              append = T)
  
}


# analysis_avsa - Test 6 ------------------------------------------------

list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv")
merged_list <- list_merged
file_name <- merged_list[1] # 1002v3 NORMAL  

# begin loop
file_name <- merged_list[i]

message("\nPreparing ", file_name, "...")

# read in merged file
data_merged <- suppressMessages(vroom(file = paste0("./3_data/analysis/merged_anno_ap/",
                                                    file_name),
                                      delim = ","))

# remove gaps to create trans and both gaps/transitions to create event
data_gapless <- data_merged[data_merged$annotation != 3, ] # gaps = 3
data_event <- data_gapless[data_gapless$annotation != 4, ] # transitons = 4

# fixpoint#1: if a file does not have a posture
data_gapless$annotation <- as.factor(data_gapless$annotation)
data_gapless$ap_posture <- as.factor((data_gapless$ap_posture))

anno_levels <- levels(data_gapless$annotation)
ap_levels <- levels(data_gapless$ap_posture)

if (length(anno_levels) < 4 || length(ap_levels) < 4) {
  
  event_levels <- union(anno_levels, ap_levels) %>% 
    as.integer() %>% 
    sort() %>% 
    paste()
  
  # if event_levels has all postures
  if (all(c("0", "1", "2", "4") %in% event_levels)) {
    
    data_gapless$annotation <- factor(data_gapless$annotation,
                                    levels = event_levels)
    data_gapless$ap_posture<- factor(data_gapless$ap_posture,
                                   levels = event_levels)
    
    # if there is no sitting in both anno and ap
  } else if (all(c("1", "2", "4") %in% event_levels)) {
    
    event_levels[length(event_levels) + 1] <- "0"
    event_levels <- as.integer(event_levels) %>% 
      sort() %>% 
      paste()
    
    data_gapless$annotation <- factor(data_gapless$annotation,
                                    levels = event_levels)
    data_gapless$ap_posture<- factor(data_gapless$ap_posture,
                                   levels = event_levels)
    
  } 
}    

# fixpoint for event
data_event$annotation <- as.factor(data_event$annotation)
data_event$ap_posture <- as.factor((data_event$ap_posture))

anno_levels <- levels(data_event$annotation)
ap_levels <- levels(data_event$ap_posture)

if (length(anno_levels) < 3 || length(ap_levels) < 3) {
  
  event_levels <- union(anno_levels, ap_levels) %>% 
    as.integer() %>% 
    sort() %>% 
    paste()
  
  # if event_levels has all postures
  if (all(c("0", "1", "2") %in% event_levels)) {
    
    data_event$annotation <- factor(data_event$annotation,
                                    levels = event_levels)
    data_event$ap_posture<- factor(data_event$ap_posture,
                                   levels = event_levels)
    
    # if there is no sitting in both anno and ap
  } else if (all(c("1", "2") %in% event_levels)) {
    
    event_levels[length(event_levels) + 1] <- "0"
    event_levels <- as.integer(event_levels) %>% 
      sort() %>% 
      paste()
    
    data_event$annotation <- factor(data_event$annotation,
                                    levels = event_levels)
    data_event$ap_posture<- factor(data_event$ap_posture,
                                   levels = event_levels)
    
  } 
}    

# TIMES: visit, event, transition (all converted to minutes)
time_visit <- (nrow(data_merged) %>%
  as.integer())/60

time_event <- (data_event %>%
  nrow(.) %>% 
  as.integer())/60

time_trans <- (data_merged[data_merged$annotation == 4, ] %>%
  nrow(.) %>% 
  as.integer())/60

time_gap <- (data_merged[data_merged$annotation == 3, ] %>%
  nrow(.) %>% 
  as.integer())/60

# check to see event and transition equal data_merged. dont include in function
all.equal(time_event + time_gap + time_trans,
          nrow(data_merged)/60)

# TIMES: event ap time (for bias), rows = ap
time_matr_event <- (table(data_event$ap_posture,
                          data_event$annotation) %>% 
                      addmargins())/60
time_matr_event

time_ap_sit <- time_matr_event[1, 4] # posture times
time_ap_sta <- time_matr_event[2, 4]
time_ap_mov <- time_matr_event[3, 4]

# TIMES: anno times and (miss)classifications, anno times are same within event and gapless
time_matr_gapless <- (table(data_gapless$ap_posture,
                            data_gapless$annotation) %>% 
                        addmargins())/60
time_matr_gapless

time_anno_sit <- time_matr_gapless[5, 1]
time_anno_sta <- time_matr_gapless[5, 2]
time_anno_mov <- time_matr_gapless[5, 3]

time_agre_ss <- time_matr_gapless[1, 1] # last two letters: first is ap, second is anno, d = stand, t = trans
time_miss_sd <- time_matr_gapless[1, 2] # "anno misclassified ap sitting as standing"
time_miss_sm <- time_matr_gapless[1, 3] # "anno misclassified ap sitting as movement"

time_miss_ds <- time_matr_gapless[2, 1]
time_agre_dd <- time_matr_gapless[2, 2] # "anno agrees with ap standing"
time_miss_dm <- time_matr_gapless[2, 3]

time_miss_ms <- time_matr_gapless[3, 1]
time_miss_md <- time_matr_gapless[3, 2]
time_agre_mm <- time_matr_gapless[3, 3]

time_miss_st <- time_matr_gapless[1, 4] # "transition time when there is ap sitting"
time_miss_dt <- time_matr_gapless[2, 4]
time_miss_mt <- time_matr_gapless[3, 4]

# TIMES: total ap time and total agree time
tot_time_ap_sit <- time_matr_gapless[1, 5]
tot_time_ap_sta <- time_matr_gapless[2, 5]
tot_time_ap_mov <- time_matr_gapless[3, 5]

time_agre_total <- time_agre_ss + time_agre_dd + time_agre_mm 

# check
sum(data_merged$annotation == data_merged$ap_posture)/60 # TRUE = agree, adds all sec they agree
all.equal(time_agre_total,
          sum(data_merged$annotation == data_merged$ap_posture)/60)

# time table
id <- data_merged$ID[1]
visit <- data_merged$Visit[1]

table_analysis_time <- data.frame(ID             = id,
                                  Visit          = visit,
                                  visit_time     = time_visit,
                                  event_time     = time_event,
                                  gap_time       = time_gap,
                                  trans_time     = time_trans,
                                  total_agree    = time_agre_total,
                                  event_agree    = time_agre_total,
                                  sit_ap         = time_ap_sit,
                                  sit_anno       = time_anno_sit,
                                  stand_ap       = time_ap_sta,
                                  stand_anno     = time_anno_sta,
                                  move_ap        = time_ap_mov,
                                  move_anno      = time_anno_mov,
                                  total_sit_ap   = tot_time_ap_sit,
                                  total_stand_ap = tot_time_ap_sta,
                                  total_move_ap  = tot_time_ap_mov,
                                  sit_agree      = time_agre_ss,
                                  stand_agree    = time_agre_dd,
                                  move_agree     = time_agre_mm,
                                  sit_trans      = time_miss_st,
                                  stand_trans    = time_miss_dt,
                                  move_trans     = time_miss_mt,
                                  sit_mis_stand  = time_miss_sd,
                                  sit_mis_move   = time_miss_sm,
                                  stand_mis_sit  = time_miss_ds,
                                  stand_mis_move = time_miss_dm,
                                  move_mis_sit   = time_miss_ms,
                                  move_mis_stand = time_miss_md)

# PERCENTAGES: event, transition, total agreement and event agreement
perc_event <- time_event/time_visit*100 #
perc_trans <- time_trans/time_visit*100
perc_gap   <- time_gap/time_visit*100
perc_agre_total <-  time_agre_total/time_visit*100 #
perc_agre_event <-  time_agre_total/time_event*100 #

# PERCENTAGES: ap and anno of event time
perc_matr_event <- (time_matr_event/time_matr_event[, 4])*100 # dividing by ap posture times
perc_matr_event

perc_ap_sit <- time_ap_sit/time_event*100 # posture percentages of event time
perc_ap_sta <- time_ap_sta/time_event*100
perc_ap_mov <- time_ap_mov/time_event*100
perc_ap_sit + perc_ap_sta + perc_ap_mov == 100

perc_anno_sit <- time_anno_sit/time_event*100
perc_anno_sta <- time_anno_sta/time_event*100
perc_anno_mov <- time_anno_mov/time_event*100
all.equal(perc_anno_sit + perc_anno_sta + perc_anno_mov,
          100)

# PERCENTAGES: (miss)classifications relative to total ap time
perc_matr_gapless <- (time_matr_gapless/time_matr_gapless[, 5])*100

perc_agre_ss <- perc_matr_gapless[1, 1] # last two letters: firs is ap, second is anno, d = stand, t = trans
perc_miss_sd <- perc_matr_gapless[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"
perc_miss_sm <- perc_matr_gapless[1, 3] # "anno misclassified ap sitting as movement ##% of ap sit time"

perc_miss_ds <- perc_matr_gapless[2, 1]
perc_agre_dd <- perc_matr_gapless[2, 2] # "anno agrees with ap standing ##% of ap standing time"
perc_miss_dm <- perc_matr_gapless[2, 3]

perc_miss_ms <- perc_matr_gapless[3, 1]
perc_miss_md <- perc_matr_gapless[3, 2]
perc_agre_mm <- perc_matr_gapless[3, 3]

perc_miss_st <- perc_matr_gapless[1, 4] # "##% of TOTAL (non-event) ap sit time classified as transition"
perc_miss_dt <- perc_matr_gapless[2, 4]
perc_miss_mt <- perc_matr_gapless[3, 4]

# PERCENTAGES: total ap time relative to visit time
tot_perc_ap_sit <- tot_time_ap_sit/time_visit*100 # posture percentages of event time
tot_perc_ap_sta <- tot_time_ap_sta/time_visit*100
tot_perc_ap_mov <- tot_time_ap_mov/time_visit*100
tot_perc_ap_mov + tot_perc_ap_sit + tot_perc_ap_sta + perc_gap

# percentage table
table_analysis_percentage <- data.frame(ID             = id,
                                        Visit          = visit,
                                        event_time     = perc_event,
                                        gap_time       = perc_gap,
                                        trans_time     = perc_trans,
                                        total_agree    = perc_agre_total,
                                        event_agree    = perc_agre_event,
                                        sit_ap         = perc_ap_sit,
                                        sit_anno       = perc_anno_sit,
                                        stand_ap       = perc_ap_sta,
                                        stand_anno     = perc_anno_sta,
                                        move_ap        = perc_ap_mov,
                                        move_anno      = perc_anno_mov,
                                        total_sit_ap   = tot_perc_ap_sit,
                                        total_stand_ap = tot_perc_ap_sta,
                                        total_move_ap  = tot_perc_ap_mov,
                                        sit_agree      = perc_agre_ss,
                                        stand_agree    = perc_agre_dd,
                                        move_agree     = perc_agre_mm,
                                        sit_trans      = perc_miss_st,
                                        stand_trans    = perc_miss_dt,
                                        move_trans     = perc_miss_mt,
                                        sit_mis_stand  = perc_miss_sd,
                                        sit_mis_move   = perc_miss_sm,
                                        stand_mis_sit  = perc_miss_ds,
                                        stand_mis_move = perc_miss_dm,
                                        move_mis_sit   = perc_miss_ms,
                                        move_mis_stand = perc_miss_md)

# write tables
if (counter == 1) {
  
  vroom_write(table_analysis_time,
              path = "./3_data/analysis/table_analysis_time.csv",
              delim = ",",
              append = F)
  
  vroom_write(table_analysis_percentage,
              path = "./3_data/analysis/table_analysis_percentage.csv",
              delim = ",",
              append = F)
  
}

if (counter > 1) {
  
  vroom_write(table_analysis_time,
              path = "./3_data/analysis/table_analysis_time.csv",
              delim = ",",
              append = T)
  
  vroom_write(table_analysis_percentage,
              path = "./3_data/analysis/table_analysis_percentage.csv",
              delim = ",",
              append = T)
  
}
  

# process_avsa_data_OLD ---------------------------------------------------
# OLD
# tib_gapless <- tib_og[tib_og$annotation != 3, ] # gaps = 3
# # fixpoint#1: if a file does not have a posture
# tib_gapless$annotation <- as.factor(tib_gapless$annotation)
# tib_gapless$ap_posture <- as.factor((tib_gapless$ap_posture))
# 
# anno_levels <- levels(tib_gapless$annotation)
# ap_levels <- levels(tib_gapless$ap_posture)
# 
# if (length(anno_levels) < 4 || length(ap_levels) < 4) {
#   
#   event_levels <- union(anno_levels, ap_levels) %>% 
#     as.integer() %>% 
#     sort() %>% 
#     paste()
#   
#   # if event_levels has all postures
#   if (all(c("0", "1", "2", "4") %in% event_levels)) {
#     
#     tib_gapless$annotation <- factor(tib_gapless$annotation,
#                                       levels = event_levels)
#     tib_gapless$ap_posture<- factor(tib_gapless$ap_posture,
#                                      levels = event_levels)
#     
#     # if there is no sitting in both anno and ap
#   } else if (all(c("1", "2", "4") %in% event_levels)) {
#     
#     event_levels[length(event_levels) + 1] <- "0"
#     event_levels <- as.integer(event_levels) %>% 
#       sort() %>% 
#       paste()
#     
#     tib_gapless$annotation <- factor(tib_gapless$annotation,
#                                       levels = event_levels)
#     tib_gapless$ap_posture<- factor(tib_gapless$ap_posture,
#                                      levels = event_levels)
#     
#   } 
# }    
# 
# # fixpoint for event
# tib_event$annotation <- as.factor(tib_event$annotation)
# tib_event$ap_posture <- as.factor((tib_event$ap_posture))
# 
# anno_levels <- levels(tib_event$annotation)
# ap_levels <- levels(tib_event$ap_posture)
# 
# if (length(anno_levels) < 3 || length(ap_levels) < 3) {
#   
#   event_levels <- union(anno_levels, ap_levels) %>% 
#     as.integer() %>% 
#     sort() %>% 
#     paste()
#   
#   # if event_levels has all postures
#   if (all(c("0", "1", "2") %in% event_levels)) {
#     
#     tib_event$annotation <- factor(tib_event$annotation,
#                                     levels = event_levels)
#     tib_event$ap_posture<- factor(tib_event$ap_posture,
#                                    levels = event_levels)
#     
#     # if there is no sitting in both anno and ap
#   } else if (all(c("1", "2") %in% event_levels)) {
#     
#     event_levels[length(event_levels) + 1] <- "0"
#     event_levels <- as.integer(event_levels) %>% 
#       sort() %>% 
#       paste()
#     
#     tib_event$annotation <- factor(tib_event$annotation,
#                                     levels = event_levels)
#     tib_event$ap_posture<- factor(tib_event$ap_posture,
#                                    levels = event_levels)
#     
#   } 
# }    
# TIMES: visit, event, transition (all converted to minutes)
# time_visit <- (nrow(tib_og) %>%
#                  as.integer())/60
# 
# time_event <- (tib_event %>%
#                  nrow(.) %>% 
#                  as.integer())/60
# 
# time_trans <- 
#   (tib_og[tib_og$annotation == 4, ] %>%
#      nrow(.) %>% 
#      as.integer())/60
# 
# time_gap <- (tib_og[tib_og$annotation == 3, ] %>%
#                nrow(.) %>% 
#                as.integer())/60
# # TIMES: event ap time (for bias), rows = ap
# time_matr_event <- (table(tib_event$ap_posture,
#                           tib_event$annotation) %>% 
#                       addmargins())
# time_matr_event
# 
# time_ap_sit <- time_matr_event[1, 4] # posture times
# time_ap_sta <- time_matr_event[2, 4]
# time_ap_mov <- time_matr_event[3, 4]
# # check to see event and transition equal tib_og. dont include in function
# all.equal(time_event + time_gap + time_trans,
#           nrow(tib_og)/60)
# 
# TIMES: anno times and (miss)classifications, anno times are same within event and gapless
# time_matr_gapless <- (table(tib_gapless$ap_posture,
#                             tib_gapless$annotation) %>% 
#                         addmargins())
# time_matr_gapless
# 
# time_anno_sit <- time_matr_gapless[5, 1]
# time_anno_sta <- time_matr_gapless[5, 2]
# time_anno_mov <- time_matr_gapless[5, 3]
# 
# time_agre_ss <- time_matr_gapless[1, 1] # last two letters: first is ap, second is anno, d = stand, t = trans
# time_miss_sd <- time_matr_gapless[1, 2] # "anno misclassified ap sitting as standing"
# time_miss_sm <- time_matr_gapless[1, 3] # "anno misclassified ap sitting as movement"
# 
# time_miss_ds <- time_matr_gapless[2, 1]
# time_agre_dd <- time_matr_gapless[2, 2] # "anno agrees with ap standing"
# time_miss_dm <- time_matr_gapless[2, 3]
# 
# time_miss_ms <- time_matr_gapless[3, 1]
# time_miss_md <- time_matr_gapless[3, 2]
# time_agre_mm <- time_matr_gapless[3, 3]
# 
# time_miss_st <- time_matr_gapless[1, 4] # "transition time when there is ap sitting"
# time_miss_dt <- time_matr_gapless[2, 4]
# time_miss_mt <- time_matr_gapless[3, 4]
# 
# # TIMES: total ap time and total agree time
# tot_time_ap_sit <- time_matr_gapless[1, 5]
# tot_time_ap_sta <- time_matr_gapless[2, 5]
# tot_time_ap_mov <- time_matr_gapless[3, 5]
# 
# time_agre_total <- time_agre_ss + time_agre_dd + time_agre_mm 
# 
# # check
# sum(tib_og$annotation == tib_og$ap_posture)/60 # TRUE = agree, adds all sec they agree
# all.equal(time_agre_total,
#           sum(tib_og$annotation == tib_og$ap_posture)/60)
# table_analysis_time <- 
#   data.frame(
#     ID             = id,
#     Visit          = visit,
#     visit_time     = time_visit,
#     event_time     = time_event,
#     gap_time       = time_gap,
#     trans_time     = time_trans,
#     total_agree    = time_agre_total,
#     event_agree    = time_agre_total,
#     sit_ap         = time_ap_sit,
#     sit_anno       = time_anno_sit,
#     stand_ap       = time_ap_sta,
#     stand_anno     = time_anno_sta,
#     move_ap        = time_ap_mov,
#     move_anno      = time_anno_mov,
#     total_sit_ap   = tot_time_ap_sit,
#     total_stand_ap = tot_time_ap_sta,
#     total_move_ap  = tot_time_ap_mov,
#     sit_agree      = time_agre_ss,
#     stand_agree    = time_agre_dd,
#     move_agree     = time_agre_mm,
#     sit_trans      = time_miss_st,
#     stand_trans    = time_miss_dt,
#     move_trans     = time_miss_mt,
#     sit_mis_stand  = time_miss_sd,
#     sit_mis_move   = time_miss_sm,
#     stand_mis_sit  = time_miss_ds,
#     stand_mis_move = time_miss_dm,
#     move_mis_sit   = time_miss_ms,
#     move_mis_stand = time_miss_md
#   )
# # PERCENTAGES: event, transition, total agreement and event agreement
# perc_event <- time_event/time_visit*100 #
# perc_trans <- time_trans/time_visit*100
# perc_gap   <- time_gap/time_visit*100
# perc_agre_total <-  time_agre_total/time_visit*100 #
# perc_agre_event <-  time_agre_total/time_event*100 #
# 
# # PERCENTAGES: ap and anno of event time
# perc_matr_event <- (time_matr_event/time_matr_event[, 4])*100 # dividing by ap posture times
# perc_matr_event
# 
# perc_ap_sit <- time_ap_sit/time_event*100 # posture percentages of event time
# perc_ap_sta <- time_ap_sta/time_event*100
# perc_ap_mov <- time_ap_mov/time_event*100
# perc_ap_sit + perc_ap_sta + perc_ap_mov == 100
# 
# perc_anno_sit <- time_anno_sit/time_event*100
# perc_anno_sta <- time_anno_sta/time_event*100
# perc_anno_mov <- time_anno_mov/time_event*100
# all.equal(perc_anno_sit + perc_anno_sta + perc_anno_mov,
#           100)
# 
# # PERCENTAGES: (miss)classifications relative to total ap time
# perc_matr_gapless <- (time_matr_gapless/time_matr_gapless[, 5])*100
# 
# perc_agre_ss <- perc_matr_gapless[1, 1] # last two letters: firs is ap, second is anno, d = stand, t = trans
# perc_miss_sd <- perc_matr_gapless[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"
# perc_miss_sm <- perc_matr_gapless[1, 3] # "anno misclassified ap sitting as movement ##% of ap sit time"
# 
# perc_miss_ds <- perc_matr_gapless[2, 1]
# perc_agre_dd <- perc_matr_gapless[2, 2] # "anno agrees with ap standing ##% of ap standing time"
# perc_miss_dm <- perc_matr_gapless[2, 3]
# 
# perc_miss_ms <- perc_matr_gapless[3, 1]
# perc_miss_md <- perc_matr_gapless[3, 2]
# perc_agre_mm <- perc_matr_gapless[3, 3]
# 
# perc_miss_st <- perc_matr_gapless[1, 4] # "##% of TOTAL (non-event) ap sit time classified as transition"
# perc_miss_dt <- perc_matr_gapless[2, 4]
# perc_miss_mt <- perc_matr_gapless[3, 4]
# 
# # PERCENTAGES: total ap time relative to visit time
# tot_perc_ap_sit <- tot_time_ap_sit/time_visit*100 # posture percentages of event time
# tot_perc_ap_sta <- tot_time_ap_sta/time_visit*100
# tot_perc_ap_mov <- tot_time_ap_mov/time_visit*100
# tot_perc_ap_mov + tot_perc_ap_sit + tot_perc_ap_sta + perc_gap
# table_analysis_percentage <- 
#   data.frame(
#     ID             = id,
#     Visit          = visit,
#     event_time     = perc_event,
#     gap_time       = perc_gap,
#     trans_time     = perc_trans,
#     total_agree    = perc_agre_total,
#     event_agree    = perc_agre_event,
#     sit_ap         = perc_ap_sit,
#     sit_anno       = perc_anno_sit,
#     stand_ap       = perc_ap_sta,
#     stand_anno     = perc_anno_sta,
#     move_ap        = perc_ap_mov,
#     move_anno      = perc_anno_mov,
#     total_sit_ap   = tot_perc_ap_sit,
#     total_stand_ap = tot_perc_ap_sta,
#     total_move_ap  = tot_perc_ap_mov,
#     sit_agree      = perc_agre_ss,
#     stand_agree    = perc_agre_dd,
#     move_agree     = perc_agre_mm,
#     sit_trans      = perc_miss_st,
#     stand_trans    = perc_miss_dt,
#     move_trans     = perc_miss_mt,
#     sit_mis_stand  = perc_miss_sd,
#     sit_mis_move   = perc_miss_sm,
#     stand_mis_sit  = perc_miss_ds,
#     stand_mis_move = perc_miss_dm,
#     move_mis_sit   = perc_miss_ms,
#     move_mis_stand = perc_miss_md
#   )
# vroom_write(table_analysis_time,
#             path = "./3_data/analysis/table_analysis_time.csv",
#             delim = ",",
#             append = F)
# 
# vroom_write(table_analysis_percentage,
#             path = "./3_data/analysis/table_analysis_percentage.csv",
#             delim = ",",
#             append = F)
# vroom_write(table_analysis_time,
#             path = "./3_data/analysis/table_analysis_time.csv",
#             delim = ",",
#             append = T)
# 
# vroom_write(table_analysis_percentage,
#             path = "./3_data/analysis/table_analysis_percentage.csv",
#             delim = ",",
#             append = T)



# analysis_sedentary ------------------------------------------------------

list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv")
merged_list <- list_merged
file_name <- merged_list[1] # 1002v3 NORMAL 
file_name <- merged_list[22] # 1074v2 NO SITTING - GOOD
file_name <- merged_list[28] # 1085v3 NO SITTING - GOOD
  
  counter <- 1
  
  for (i in seq_along(merged_list)) {
    
    file_name <- merged_list[i]
    
    message("\nPreparing ", file_name, "...")
    
    # read in merged file
    data_merged <- suppressMessages(vroom(file = paste0("./3_data/analysis/merged_anno_ap/",
                                                        file_name),
                                          delim = ","))
    
    # make stand and move the same, then do the same as analysis_avsa (stand/move = 1)
    data_sedentary <- lapply(data_merged,
                             function(x) replace(x,x %in% 1:2, 1) ) %>% 
      bind_cols()

    data_gapless <- data_sedentary[data_sedentary$annotation != 3, ] # gaps = 3
    data_event <- data_gapless[data_gapless$annotation != 4, ] # transitons = 4
    
    # fixpoint#1: if a file does not have a posture
    data_gapless$annotation <- as.factor(data_gapless$annotation)
    data_gapless$ap_posture <- as.factor((data_gapless$ap_posture))
    
    anno_levels <- levels(data_gapless$annotation)
    ap_levels <- levels(data_gapless$ap_posture)
    
    if (length(anno_levels) < 3 || length(ap_levels) < 3) {
      
      event_levels <- union(anno_levels, ap_levels) %>% 
        as.integer() %>% 
        sort() %>% 
        paste()
      
      # if event_levels has all postures
      if (all(c("0", "1", "4") %in% event_levels)) {
        
        data_gapless$annotation <- factor(data_gapless$annotation,
                                          levels = event_levels)
        data_gapless$ap_posture<- factor(data_gapless$ap_posture,
                                         levels = event_levels)
        
        # if there is no sitting in both anno and ap
      } else if (all(c("1", "4") %in% event_levels)) {
        
        event_levels[length(event_levels) + 1] <- "0"
        event_levels <- as.integer(event_levels) %>% 
          sort() %>% 
          paste()
        
        data_gapless$annotation <- factor(data_gapless$annotation,
                                          levels = event_levels)
        data_gapless$ap_posture<- factor(data_gapless$ap_posture,
                                         levels = event_levels)
        
      } 
    }    
    
    # fixpoint for event
    data_event$annotation <- as.factor(data_event$annotation)
    data_event$ap_posture <- as.factor((data_event$ap_posture))
    
    anno_levels <- levels(data_event$annotation)
    ap_levels <- levels(data_event$ap_posture)
    
    if (length(anno_levels) < 2 || length(ap_levels) < 2) {
      
      event_levels <- union(anno_levels, ap_levels) %>% 
        as.integer() %>% 
        sort() %>% 
        paste()
      
      # if event_levels has all postures
      if (all(c("0", "1") %in% event_levels)) {
        
        data_event$annotation <- factor(data_event$annotation,
                                        levels = event_levels)
        data_event$ap_posture<- factor(data_event$ap_posture,
                                       levels = event_levels)
        
        # if there is no sitting in both anno and ap
      } else if (all(c("1") %in% event_levels)) {
        
        event_levels[length(event_levels) + 1] <- "0"
        event_levels <- as.integer(event_levels) %>% 
          sort() %>% 
          paste()
        
        data_event$annotation <- factor(data_event$annotation,
                                        levels = event_levels)
        data_event$ap_posture<- factor(data_event$ap_posture,
                                       levels = event_levels)
        
      } 
    }    
    
    # TIMES: visit, event, transition (all converted to minutes)
    time_visit <- (nrow(data_merged) %>%
                     as.integer())/60
    
    time_event <- (data_event %>%
                     nrow(.) %>% 
                     as.integer())/60
    
    time_trans <- (data_merged[data_merged$annotation == 4, ] %>%
                     nrow(.) %>% 
                     as.integer())/60
    
    time_gap <- (data_merged[data_merged$annotation == 3, ] %>%
                   nrow(.) %>% 
                   as.integer())/60
    
    # check to see event and transition equal data_merged. dont include in function
    all.equal(time_event + time_gap + time_trans,
              nrow(data_merged)/60)
    
    # TIMES: event ap time (for bias), rows = ap
    time_matr_event <- (table(data_event$ap_posture,
                              data_event$annotation) %>% 
                          addmargins())/60
    time_matr_event
    
    time_ap_sed <- time_matr_event[1, 3] # (non)sedentary times
    time_ap_upr <- time_matr_event[2, 3]
    
    # TIMES: anno times and (miss)classifications, anno times are same within event and gapless
    time_matr_gapless <- (table(data_gapless$ap_posture,
                                data_gapless$annotation) %>% 
                            addmargins())/60
    time_matr_gapless
    
    time_anno_sed <- time_matr_gapless[4, 1]
    time_anno_upr <- time_matr_gapless[4, 2]
    
    time_agre_ss <- time_matr_gapless[1, 1] # last two letters: first is ap, second is anno, u = upright
    time_miss_su <- time_matr_gapless[1, 2] # "anno misclassified ap sitting as upright"
    
    time_miss_us <- time_matr_gapless[2, 1]
    time_agre_uu <- time_matr_gapless[2, 2] # "anno agrees with ap upright"
    
    time_miss_st <- time_matr_gapless[1, 3] # "transition time when there is ap sitting"
    time_miss_ut <- time_matr_gapless[2, 3]
    
    # TIMES: total ap time and total agree time
    tot_time_ap_sed <- time_matr_gapless[1, 4]
    tot_time_ap_upr <- time_matr_gapless[2, 4]
    
    time_agre_total <- time_agre_ss + time_agre_uu
    
    # check
    sum(data_sedentary$annotation == data_sedentary$ap_posture)/60 # TRUE = agree, adds all sec they agree
    all.equal(time_agre_total,
              sum(data_sedentary$annotation == data_sedentary$ap_posture)/60)
    
    # time table
    id <- data_merged$ID[1]
    visit <- data_merged$Visit[1]
    
    table_sedentary_time <- data.frame(ID             = id,
                                      Visit          = visit,
                                      total_agree    = time_agre_total,
                                      event_agree    = time_agre_total,
                                      sed_ap         = time_ap_sed,
                                      sed_anno       = time_anno_sed,
                                      upright_ap     = time_ap_upr,
                                      upright_anno   = time_anno_upr,
                                      total_sed_ap   = tot_time_ap_sed,
                                      total_upr_ap   = tot_time_ap_upr,
                                      sed_agree      = time_agre_ss,
                                      upr_agree      = time_agre_uu,
                                      sed_trans      = time_miss_st,
                                      upr_trans      = time_miss_ut,
                                      sed_mis_upr    = time_miss_su,
                                      upr_mis_sed    = time_miss_us)
    
    # PERCENTAGES: total agreement and event agreement
    perc_agre_total <-  time_agre_total/time_visit*100 #
    perc_agre_event <-  time_agre_total/time_event*100 #
    
    # PERCENTAGES: ap and anno of event time
    perc_matr_event <- (time_matr_event/time_matr_event[, 3])*100 # dividing by ap posture times
    perc_matr_event
    
    perc_ap_sed <- time_ap_sed/time_event*100 # posture percentages of event time
    perc_ap_upr <- time_ap_upr/time_event*100
    perc_ap_sed + perc_ap_upr == 100
    
    perc_anno_sed <- time_anno_sed/time_event*100
    perc_anno_upr <- time_anno_upr/time_event*100
    all.equal(perc_anno_sed + perc_anno_upr,
              100)
    
    # PERCENTAGES: (miss)classifications relative to total ap time
    perc_matr_gapless <- (time_matr_gapless/time_matr_gapless[, 4])*100
    
    perc_agre_ss <- perc_matr_gapless[1, 1] # last two letters: firs is ap, second is anno, d = stand, t = trans
    perc_miss_su <- perc_matr_gapless[1, 2] # "anno misclassified ap sitting as standing ##% of ap sit time"

    perc_miss_us <- perc_matr_gapless[2, 1]
    perc_agre_uu <- perc_matr_gapless[2, 2] # "anno agrees with ap upright ##% of ap upright time"

    perc_miss_st <- perc_matr_gapless[1, 3] # "##% of TOTAL (non-event) ap sit time classified as transition"
    perc_miss_ut <- perc_matr_gapless[2, 3]
    
    # PERCENTAGES: total ap time relative to visit time
    tot_perc_ap_sed <- tot_time_ap_sed/time_visit*100 # posture percentages of event time
    tot_perc_ap_upr <- tot_time_ap_upr/time_visit*100
    
    # percentage table
    table_sedentary_percentage <- data.frame(ID             = id,
                                            Visit          = visit,
                                            total_agree    = perc_agre_total,
                                            event_agree    = perc_agre_event,
                                            sed_ap         = perc_ap_sed,
                                            sed_anno       = perc_anno_sed,
                                            upright_ap     = perc_ap_upr,
                                            upright_anno   = perc_anno_upr,
                                            total_sed_ap   = tot_perc_ap_sed,
                                            total_upr_ap   = tot_perc_ap_upr,
                                            sed_agree      = perc_agre_ss,
                                            upr_agree      = perc_agre_uu,
                                            sed_trans      = perc_miss_st,
                                            upr_trans      = perc_miss_ut,
                                            sed_mis_upr    = perc_miss_su,
                                            upr_mis_sed    = perc_miss_us)
    
    # write tables
    if (counter == 1) {
      
      vroom_write(table_sedentary_time,
                  path = "./3_data/analysis/table_sedentary_time.csv",
                  delim = ",",
                  append = F)
      
      vroom_write(table_sedentary_percentage,
                  path = "./3_data/analysis/table_sedentary_percentage.csv",
                  delim = ",",
                  append = F)
      
    }
    
    if (counter > 1) {
      
      vroom_write(table_sedentary_time,
                  path = "./3_data/analysis/table_sedentary_time.csv",
                  delim = ",",
                  append = T)
      
      vroom_write(table_sedentary_percentage,
                  path = "./3_data/analysis/table_sedentary_percentage.csv",
                  delim = ",",
                  append = T)
      
    }
    
    counter <- counter+1
    
  }





# create summary file tests -----------------------------------------------
create_posture_summary()

data_time <- suppressMessages(vroom(file = "./3_data/analysis/table_analysis_time.csv",
                                    delim = ","))
data_percentage <- suppressMessages(vroom(file = "./3_data/analysis/table_analysis_percentage.csv",
                                          delim = ","))

# changing Na to 0
data_time[is.na(data_time)] <- 0
data_percentage[is.na(data_percentage)] <- 0

# means and sd
colSd <- function(x, na.rm = T) {
  
  if (na.rm) {
    
    n <- colSums(!is.na(x))
    
  } else {
    
    n <- nrow(x)
    
  }
  
  colVar <- colMeans(x*x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
  
  return(sqrt(colVar * n/(n-1)))
  
}

avg_times <- colMeans(data_time)
sd_times <- colSd(data_time)

avg_percs <- colMeans(data_percentage)
sd_percs <- colSd(data_percentage)

# table of time and perc
tbl_sum_vis_time <- bind_rows(avg_times,
                              sd_times) %>% 
  as.data.frame()

tbl_sum_vis_perc <- bind_rows(avg_percs,
                              sd_percs) %>% 
  as.data.frame()

# merge table 
tbl_sum_vis <- bind_rows(tbl_sum_vis_time,
                         tbl_sum_vis_perc) %>% 
  t() %>% 
  as.data.frame()

# clean
colnames(tbl_sum_vis) <- c("mean_min",
                           "sd_min",
                           "mean_perc",
                           "sd_perc")

tbl_sum_vis <- tbl_sum_vis[-(1:2), ]

tbl_sum_vis <- round(tbl_sum_vis, 1)

tbl_sum_vis <- rownames_to_column(tbl_sum_vis,
                                  var = "variable")
comments <- c(NA               , "% of visit time", "% of visit time", "% of visit time",
              "% of visit time", "% of event time", "% of event time", "% of event time",
              "% of event time", "% of event time", "% of event time", "% of event time",
              "% of visit time", "% of visit time", "% of visit time", "% of ap time",
              "% of ap time"   , "% of ap time", "% of ap time", "% of ap time",
              "% of ap time"   , "% of ap time", "% of ap time", "% of ap time",
              "% of ap time"   , "% of ap time", "% of ap time")

tbl_sum_vis$comment <- comments

# write
vroom_write(tbl_sum_vis,
            path = "./4_results/summary_visit.csv",
            delim = ",")
write_rds(tbl_sum_vis,
          path = "./4_results/summary_visit.rds",
          compress = "none")

assign("tbl_sum_vis",
       tbl_sum_vis,
       envir = .GlobalEnv)

# make a table for the posture results:
tbl_sum_pos_time <- data.frame(posture = c("sit","stand","move"))

# raw means:
tbl_sum_pos_time$ap_mean  <- c(mean(data_time$sit_ap),
                               mean(data_time$stand_ap),
                               mean(data_time$move_ap))
tbl_sum_pos_time$ap_sd    <- c(sd(data_time$sit_ap),
                               sd(data_time$stand_ap),
                               sd(data_time$move_ap))
tbl_sum_pos_time$img_mean <- c(mean(data_time$sit_anno),
                               mean(data_time$stand_anno),
                               mean(data_time$move_anno))
tbl_sum_pos_time$img_sd   <- c(sd(data_time$sit_anno),
                               sd(data_time$stand_anno),
                               sd(data_time$move_anno))

# linear mixed effects model: bias~b0 + b_i + e_ij
sitmodel <- lmer(sit_anno - sit_ap ~ 1 + (1|ID),
                 data = data_time)
standmodel <- lmer(stand_anno - stand_ap ~ 1 + (1|ID),
                   data = data_time)
movemodel <- lmer(move_anno - move_ap ~ 1 + (1|ID),
                  data = data_time)

# biases are estimated from model
tbl_sum_pos_time$bias <- c(fixef(sitmodel),
                           fixef(standmodel),
                           fixef(movemodel))

# se is "unexplained variability" in the biases
VarCorr(sitmodel)
as.data.frame(VarCorr(sitmodel))
tbl_sum_pos_time$SE <- c(as.data.frame(VarCorr(sitmodel))[2,5],
                         as.data.frame(VarCorr(standmodel))[2,5],
                         as.data.frame(VarCorr(movemodel))[2,5])

# approximate 95% CIs
tbl_sum_pos_time$upper_95_bias <- tbl_sum_pos_time$lower_95_bias <- NA
confint(sitmodel)
confint(sitmodel, 3)
tbl_sum_pos_time[,8:9] <- rbind(confint(sitmodel,3),
                                confint(standmodel,3),
                                confint(movemodel,3))

# transition time when ap said a posture
tbl_sum_pos_time$img_trans_mean <- c(mean(data_time$sit_trans),
                                     mean(data_time$stand_trans),
                                     mean(data_time$move_trans))

# misclassifications
tbl_sum_pos_time$img_miss_sit_mean   <- c(0,
                                          mean(data_time$stand_mis_sit),
                                          mean(data_time$move_mis_sit))

tbl_sum_pos_time$img_miss_stand_mean <- c(mean(data_time$sit_mis_stand),
                                          0,
                                          mean(data_time$move_mis_stand))

tbl_sum_pos_time$img_miss_move_mean  <- c(mean(data_time$sit_mis_move),
                                          mean(data_time$stand_mis_move),
                                          0)

# % sum table
tbl_sum_pos_perc <- tbl_sum_pos_time

tbl_sum_pos_perc$bias          <- (tbl_sum_pos_time$bias / tbl_sum_pos_time$ap_mean)*100
tbl_sum_pos_perc$SE            <- (tbl_sum_pos_time$SE/tbl_sum_pos_time$ap_mean)*100
tbl_sum_pos_perc$lower_95_bias <- (tbl_sum_pos_time$lower_95_bias/tbl_sum_pos_time$ap_mean)*100
tbl_sum_pos_perc$upper_95_bias <- (tbl_sum_pos_time$upper_95_bias/tbl_sum_pos_time$ap_mean)*100

# round to 1 digit, arbitrarily
tbl_sum_pos_time[,-1] <- round(tbl_sum_pos_time[,-1], 1)
tbl_sum_pos_perc[,-1] <- round(tbl_sum_pos_perc[,-1], 1)

# output summary posture tables

write_rds(tbl_sum_pos_time,
          path = "./4_results/summary_posture_time.rds",
          compress = "none")
write_rds(tbl_sum_pos_perc,
          path = "./4_results/summary_posture_perc.rds",
          compress = "none")
vroom_write(tbl_sum_pos_time,
            path = "./4_results/summary_posture_mins.csv",
            ddelim = ",")
vroom_write(tbl_sum_pos_perc,
            path = "./4_results/summary_posture_perc.csv",
            delim = ",")


# create_misclass_table - Test 1 ------------------------------------------

data_time <- suppressMessages(vroom(file = "./3_data/analysis/table_analysis_time.csv",
                                    delim = ","))

tbl_miss_time <- data.frame(posture = c("sit",
                                          "stand",
                                          "move"))

# total ap times
tbl_miss_time$AP_Total <- c(mean(data_time$total_sit_ap),
                              mean(data_time$total_stand_ap),
                              mean(data_time$total_move_ap))

# IMG correct classification
tbl_miss_time$IMG <- c(mean(data_time$sit_agree),
                         mean(data_time$stand_agree),
                         mean(data_time$move_agree))

# transition misclassification
tbl_miss_time$Transition <- c(mean(data_time$sit_trans),
                                mean(data_time$stand_trans),
                                mean(data_time$move_trans))

# posture misclassifications
tbl_miss_time$Sit   <- c(0,
                           mean(data_time$stand_mis_sit),
                           mean(data_time$move_mis_sit))

tbl_miss_time$Stand <- c(mean(data_time$sit_mis_stand),
                           0,
                           mean(data_time$move_mis_stand))

tbl_miss_time$Move  <- c(mean(data_time$sit_mis_move),
                           mean(data_time$stand_mis_move),
                           0)

# percent table
tbl_miss_perc <- tbl_miss_time

tbl_miss_perc[, 3:7] <- (tbl_miss_perc[, 3:7]/tbl_miss_perc$AP_Total)*100

tbl_miss_perc

# round
tbl_miss_time[, -1] <- round(tbl_miss_time[, -1], 1)
tbl_miss_perc[, -1] <- round(tbl_miss_perc[, -1], 1)

tbl_miss_time
tbl_miss_perc




# figures & tables --------------------------------------------------------

tbl_miss_time <- read_rds(path = "./4_results/posture_miss_time.rds")

# read in only classifications
graph <- tbl_miss_time[ , -which(colnames(tbl_miss_time) %in% c("AP_Total"))]

# rename IMG to correct
colnames(graph)[2] <- "Correct"

# change table into variables that represent x, y, other
graph <- graph %>% 
  melt(id.vars = "Posture")

# clean
colnames(graph)[2] <- "Classification"

graph <- graph[graph$value != 0, ]

graph$Posture <- factor(graph$Posture,
                        levels = c("Sit",
                                   "Stand",
                                   "Move"))
graph

# plot
ggplot(data = graph) +
  geom_bar(mapping = aes(x = Posture,
                         y = value,
                         fill = Classification),
           stat = "identity") +
  geom_text(data = tbl_miss_time,
            mapping = aes(x = Posture,
                          y = AP_Total,
                          label = paste(AP_Total, "mins")),
            vjust = -0.5) +
  geom_text(mapping = aes(x = Posture,
                          y = value,
                          fill = Classification,
                          label = paste(value, "mins")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Total AP estimates correctly classified by IMGs",
       x = "Posture",
       y = "Minutes") +
  theme(plot.title = element_text(lineheight = 1,
                                  hjust = .5),
        text = element_text(size = 15)) +
  ggtitle("Proportion of Total AP estimates correctly classified by IMGs") +
  scale_fill_manual(values = c("#3399FF",
                               "#CC3333",
                               "#9999FF",
                               "#FF9933",
                               "#99CC99"))
scale_fill_brewer(palette = "Set1",
                  direction = -1)

# figure_miss_percent, save as ???
tbl_miss_perc <- read_rds(path = "./4_results/posture_miss_perc.rds")

# read in only classifications
graph <- tbl_miss_perc[ , -which(colnames(tbl_miss_perc) %in% c("AP_Total"))]

# rename IMG to correct
colnames(graph)[2] <- "Correct"

# change table into variables that represent x, y, other
graph <- graph %>% 
  melt(id.vars = "Posture")

# clean
colnames(graph)[2] <- "Classification"

graph <- graph[graph$value != 0, ]

graph$Posture <- factor(graph$Posture,
                        levels = c("Sit",
                                   "Stand",
                                   "Move"))

# label
lbl <- tbl_miss_perc
lbl$label <- lbl$AP_Total/lbl$AP_Total*100



ggplot(data = graph) +
  geom_bar(mapping = aes(x = Posture,
                         y = value,
                         fill = Classification),
           stat = "identity") +
  geom_text(data = lbl,
            mapping = aes(x = Posture,
                          y = label,
                          label = paste(AP_Total, "mins")),
            vjust = -0.5) +
  geom_text(mapping = aes(x = Posture,
                          y = value,
                          fill = Classification,
                          label = paste0(value, "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Total AP estimates correctly classified by IMGs",
       x = "Posture",
       y = "Minutes") +
  theme(plot.title = element_text(lineheight = 1,
                                  hjust = .5),
        text = element_text(size = 15)) +
  ggtitle("Proportion of Total AP estimates correctly classified by IMGs") +
  scale_fill_manual(values = c("#3399FF",
                               "#FF6666",
                               "#9999FF",
                               "#FF9933",
                               "#99CC99"))

ggplot(data = graph) +
  geom_bar(mapping = aes(x = Posture,
                         y = value,
                         fill = Classification),
           stat = "identity",
           position = position_fill()) +
  scale_y_continuous(labels = percent) +
  labs(title = "Proportion of Total AP estimates correctly classified by IMGs",
       x = "Posture",
       y = "Percentage") +
  theme(plot.title = element_text(lineheight = 1,
                                  hjust = .5),
        text = element_text(size = 15)) +
  ggtitle("Proportion of Total AP estimates correctly classified by IMGs")
theme_bw()
scale_fill_brewer(palette = 1)
?scale_fill_brewer()
scale_y_continuous(labels = percent)



# other -------------------------------------------------------------------

# capitalizing
substr(colnames(tbl_bias_time), 1, 1) <- toupper(substr(colnames(tbl_bias_time), 1, 1))
substr(colnames(tbl_bias_time)[2:3], 1, 2) <- toupper(substr(colnames(tbl_bias_time)[2:3], 1, 2))

# aggregrating
time_anno_pos <- data_event %>% 
  group_by(annotation) %>%
  summarise(time = length(annotation))

df.60sec <- df.1sec %>% 
  group_by(ID, by60 = cut(time, "60 sec")) %>%
  summarise(annotation = Modes(annotation), ap.posture = Modes(ap.posture))

Modes <- function(x) {
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
  
}

# testing count function 


events.1sec <- read.csv(paste0("./", event.list.60sec[17]), stringsAsFactors = T)
filename = as.character(substr(event.list.60sec[17], 6, 12))
events.1sec$annotation <- as.factor(events.1sec$annotation)
events.1sec$ap.posture <- as.factor((events.1sec$ap.posture))
anno.levels <- levels(events.1sec$annotation)
l <- length(anno.levels)
ap.levels <- levels(events.1sec$ap.posture)
n <- length(ap.levels)

if (l==1){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels[length(anno.levels) + 1] <- "2"
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (l==2){
  anno.levels[length(anno.levels) + 1] <- "1"
  anno.levels <- sort(anno.levels)
  events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
} 
if (n==1){
  ap.levels[length(ap.levels) + 1] <- "1"
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 
if (n==2){
  ap.levels[length(ap.levels) + 1] <- "2"
  events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
} 

t <- table(events.1sec$ap.posture, events.1sec$annotation)
t
a <- t[1,1]
b <- t[1,2]
c <- t[1,3]
d <- t[2,1] 
e <- t[2,2]
f <- t[2,3]
g <- t[3,1]
h <- t[3,2]
i <- t[3,3]

id <- as.character(events.1sec$ID[1])
count <- c(a,b,c,d,e,f,g,h,i)
activpal   <- rep(0:2, each=3)
annotation <- rep(0:2, times=3)

data <- data.frame(ID=id,
                   Activpal = activpal,
                   Annotation = annotation,
                   Count = count)

write.table(data, 
            file = paste0("./counts/", filename, "60sec.csv"),
            sep = ",",
            row.names = F,
            col.names = T)

# testing event.kappa #

counter <- 1
temp_events <- read.csv(paste0("./", event.list.1sec[12]))
id = as.character(temp_events$ID[1])
temp_events <- temp_events[ ,c(3,4)]
table(temp_events$annotation, temp_events$ap.posture)
kappa2(temp_events)
temp_kappa  <- kappa2(temp_events, weight = "equal")
temp_kappa  <- do.call(rbind, temp_kappa)
p.val       <- temp_kappa[8, ]
temp_kappa  <- temp_kappa[5, ]

if (counter==1)
  kappa.table.1sec <- data.frame(ID = c(id, 2:15),
                                 Visit = c(visit, 2:15),
                                 kappa_1sec = c(temp_kappa, 2:15),
                                 pvalue_1sec = c(p.val, 2:15)
  )
dat
if (counter>1)
  kappa.table.1sec$ID <- append(kappa.table.1sec$ID, id)
kappa.table.1sec$Visit <- append(kappa.table.1sec$Visit, visit)
kappa.table.1sec$kappa_1se <- append(kappa.table.1sec$kappa_1sec, temp_kappa)
kappa.table.1sec$pvalue_1sec <- append(kappa.table.1sec$pvalue_1sec, p.val)

counter <- counter+1

# kappa 



events.1sec <- lapply(event.list.1sec, read.csv, header=T)
events.1sec <- do.call(rbind, events.1sec)
events.1sec <- events.1sec[ ,-c(1,2)]

t <- table(events.1sec$ap.posture, events.1sec$annotation)
t
x<- addmargins(t)
x
45020+8602+28891
3852/21012 + 9496/21012 + 7664/21012
9496/21012
7664/21012
8602/21012
28891/18742
41047/42759
17959/18742

### total kappa ###
events.1sec <- lapply(event.list.1sec, read.csv, header=T)
events.1sec <- do.call(rbind, events.1sec)
events.1sec <- events.1sec[ ,-c(1)]

nrow(subset(events.1sec, ap.posture==0 & annotation==1))
t[1,2]
table(events.1sec$ap.posture, events.1sec$annotation)

t <- table(events.1sec$ap.posture, events.1sec$annotation)



kappa2(events.1sec, weight = "equal")
kap.1sec <- kappa2(events.1sec, weight = "equal")
kap.1sec <- do.call(rbind, kap.1sec)
p.val.1  <- kap.1sec[8, ]
kap.1sec <- kap.1sec[5, ]

events.60sec <- lapply(event.list.60sec, read.csv, header=T)
events.60sec <- do.call(rbind, events.60sec)
events.60sec <- events.60sec[ ,c(5,6)]
kappa2(events.60sec, weight = "equal")
kap.60sec <- kappa2(events.60sec, weight = "equal")
kap.60sec <- do.call(rbind, kap.60sec)
p.val.60  <- kap.60sec[8, ]
kap.60sec <- kap.60sec[5, ]

### kappa per observation ###
event.kappa.1sec(event.list.1sec)
event.kappa.60sec(event.list.60sec)

# maybe for later??
on_off_log$hours_on <- as.vector(difftime(strptime(on_off_log$date_time_off,format="%Y-%m-%d %H:%M:%S"),strptime(on_off_log$date_time_on,format="%Y-%m-%d %H:%M:%S"), units="hours"))


# Use for Noldus??? -------------------------------------------------------

#second by second from process_ap code
time_each_event <- as.vector(difftime(strptime(raw_ap$time[seq_len(n - 1) + 1],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      strptime(raw_ap$time[seq_len(n - 1)],
                                               format="%Y-%m-%d %H:%M:%S"),
                                      units = "secs"))
time_each_event <- c(time_each_event,
                     round(raw_ap[n,"interval"],
                           0))
time_each_event[is.na(time_each_event) == T] <- 1


