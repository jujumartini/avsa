anno_list


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
  attr(raw_ap$time, "tzone")
  
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
raw_ap$interval <- as.numeric(raw_ap$interval)
raw_ap$methrs <- as.numeric(raw_ap$methrs)
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
start_time <- strptime(raw_ap$time[1],
                       format="%Y-%m-%d %H:%M:%S")
times <- start_time + (0:(l - 1))

# The met hours per second in the interval.
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
fifteen_sec_times <- start_time + (15 * rep(0:(floor(l / 15)),
                                            each = 15,
                                            length = l))
fifteen_sec_mets <- tapply(mets,
                           INDEX = fifteen_sec_times,
                           FUN = mean)
fifteen_sec_mets <- rep(fifteen_sec_mets,
                        each = 15,
                        length = l)

# Make 1-min epoch variable and METs
one_min_times <- start_time + (60 * rep(0:(floor(l / 60)),
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

sbs_ap$time <- strptime(sbs_ap$time,"%Y-%m-%d %H:%M:%S")

### Clean ###
sbs_ap$time <- 
  as.POSIXct(sbs_ap$time, 
             tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
sbs_ap <- 
  sbs_ap[!(sbs_ap$off==1), ] #remove non-visit time#
sbs_ap$ID <- 
  as.integer(substr(testap, 6, 9)) #add in ID
sbs_ap$Visit <-
  as.integer(substr(testap, 12, 12)) #add in visit
sbs_ap <- 
  sbs_ap[ , c(11,12,1,3)] #only need ap.posture column
sbs_ap$ap.posture <- 
  as.character(sbs_ap$ap.posture) #change to character for next step
sbs_ap$ap.posture[sbs_ap$ap.posture == "0"] <- "posture;0006 sitting" 
sbs_ap$ap.posture[sbs_ap$ap.posture == "1"] <- "posture;0007 standing" 
sbs_ap$ap.posture[sbs_ap$ap.posture == "2"] <- "posture;0008 movement"
Filename3 = aplist2[event.file]
write.csv(sbs_ap, file = paste0("./data/ap/", Filename3))



# other -------------------------------------------------------------------

# checks
inds_worn <- (1:(dim(sbs_ap)[1]))[sbs_ap$off==0]
i <- length(inds_worn)
if(i == 0) {
  
  sbs_ap$off <- "AP and on.off.log do not match"
  
}

#### testing merge function
test = "FLAC_1042V2_POSTURE_CHANG.CSV"
{
  image.frame <- read.csv(paste0("./data/image", 
                                 "/",
                                 test),
                          header = T,
                          sep = ",",
                          stringsAsFactors = F)
  image.frame <- image.frame[ ,-c(1)]  
  ID = as.character(substr(test, 6, 9))
  Visit = as.character(substr(test, 10, 11))
  
  ap.frame <- read.csv(paste0("./data/ap/", 
                              "FLAC_",
                              ID,
                              "_",
                              Visit,
                              ".csv"),
                       header = T,
                       sep = ",",
                       stringsAsFactors = F)
  ap.frame <- ap.frame[ ,-c(1)]  
  
  merged.frame <- full_join(image.frame, ap.frame)
  merged.frame <- merged.frame[ ,-c(2,3)]
  merged.frame <- merged.frame[complete.cases(merged.frame), ]
  merged.frame$ID <- paste(c(as.character(substr(test, 6, 9)), as.character(substr(test, 10, 11))), collapse = "")
  Filename4 = aplist3
  write.csv(merged.frame, file = paste0("./data/merged/", Filename4))
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




# maybe for later??
on_off_log$hours_on <- as.vector(difftime(strptime(on_off_log$date_time_off,format="%Y-%m-%d %H:%M:%S"),strptime(on_off_log$date_time_on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
