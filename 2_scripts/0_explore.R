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



# other -------------------------------------------------------------------

# checks
inds_worn <- (1:(dim(sbs_ap)[1]))[sbs_ap$off==0]
i <- length(inds_worn)
if(i == 0) {
  
  sbs_ap$off <- "AP and on.off.log do not match"
  
}

sbs_ap$ap.posture <- 
  as.character(sbs_ap$ap.posture) #change to character for next step
sbs_ap$ap.posture[sbs_ap$ap.posture == "0"] <- "posture;0006 sitting" 
sbs_ap$ap.posture[sbs_ap$ap.posture == "1"] <- "posture;0007 standing" 
sbs_ap$ap.posture[sbs_ap$ap.posture == "2"] <- "posture;0008 movement"

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


