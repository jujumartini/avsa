process_anno <- function(anno_file_list, corr_timstamps_path, on_off_log) {
  
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
    
    new <- seq.POSIXt(mer_anno$NEWstarttime[i],
                      mer_anno$NEWendtime[i], by = "sec")
    annotation <- rep(mer_anno$annotation[i],
                      length(new))
    data.frame(time = new,
               annotation = annotation)
    
  }
  
  # create processed anno files
  for (i in seq_along(anno_file_list)) {
    
    print(anno_file_list[i])
    
    raw_anno <- read.table(file = paste0("./3_data/raw/annotation/", anno_file_list[i]),
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
    file_name = anno_file_list[i]
    id <- as.integer(substr(anno_file_list[i], 6, 9))
    visit <- as.integer(substr(anno_file_list[i], 11, 11))
    
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
    
    # take away off times + clean
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
  }
}

img_sbs <- function(anno_file_list, timestamps_path){
  
  # read in timestamps csv
  timestamps <- timestamps_path
  
  for (i in 1:length(anno_file_list)) {
    
    print(annotation.file.list[i])
    
    df1 <- read.csv(file = timestamps)
    
    df2 = read.table(paste0("./raw/annotation", "/",
                            annotation.file.list[i]),
                     header = T,
                     sep = ",")
    Filename = annotation.file.list[i]
    df2$ID <- as.integer(substr(annotation.file.list[i], 6, 9))
    df2$Visit = as.integer(substr(annotation.file.list[i], 11, 11))
    
    ###Change Column Type with Lubridate###
    df2$startTime <- ymd_hms(df2$startTime, tz="UTC")
    df2$endTime <- ymd_hms(df2$endTime, tz="UTC")
    
    df1$StopWatch_YMD_HMS <-
      ymd_hms(df1$StopWatch_YMD_HMS, tz="America/Chicago")
    
    df1$Corr_Picture_YMD_HMS <-
      ymd_hms(df1$Corr_Picture_YMD_HMS, tz="America/Chicago")
    
    ###Create Time Difference column and Calculate###
    df1$Difference <- NA
    df1$Difference <-
      with(df1,
           difftime(StopWatch_YMD_HMS, Corr_Picture_YMD_HMS, units = "secs"))
    
    ###Merge Data Frames###
    df3 <- merge(df2, df1, by = c("ID", "Visit"))
    
    ###Add Difference to Start Time Column###
    df3$NEWstarttime <- NA
    df3 <- df3 %>%
      mutate(NEWstarttime = if_else(!is.na(Difference),
                                    startTime + Difference,
                                    startTime))
    
    ###Add Difference to End Time Column###
    df3$NEWendtime <- NA
    df3 <- df3 %>%
      mutate(NEWendtime = if_else(!is.na(Difference),
                                  endTime + Difference,
                                  endTime))
    
    ### Changing NEW times to Chicago... to not double convert activpal -.-###
    df3$NEWstarttime <- with_tz(df3$NEWstarttime, tz = "America/Chicago")
    df3$NEWendtime <- with_tz(df3$NEWendtime, tz = "America/Chicago")
    df3$NEWstarttime <- strptime(df3$NEWstarttime,format="%Y-%m-%d %H:%M:%OS")
    df3$NEWendtime <- strptime(df3$NEWendtime,format="%Y-%m-%d %H:%M:%OS")
    
    ###write a "check" csv file to see if stopwatch matches NEW start time###
    write.csv(df3, file = paste0("./data/image_check/", Filename))
    
    ###second by second###
    n <- nrow(df3)
    l <- lapply(1:n, function(i) {
      new <- seq.POSIXt(df3$NEWstarttime[i], df3$NEWendtime[i], by = "sec")
      annotation <- rep(df3$annotation[i], length(new))
      data.frame(time = new, annotation = annotation)
    })
    
    df4 <- Reduce(rbind, l)
    
    df5 <- df4 %>% pad
    
    ###changing NA's to transition;gap###
    levels <- levels(df5$annotation)
    levels[length(levels) + 1] <- "transition;gap"
    df5$annotation <- factor(df5$annotation, levels = levels)
    df5$annotation[is.na(df5$annotation)] <- "transition;gap"
    
    ###add in ID and Visit###
    df5$ID <- as.integer(substr(annotation.file.list[i], 6, 9))
    df5$Visit = as.integer(substr(annotation.file.list[i], 11, 11))
    
    ###reorder columns###
    df6 <-
      df5[,c(
        "ID",
        "Visit",
        "time",
        "annotation")]
    
    ###Save the new Dataframe###
    write.csv(df6, file = paste0("./Data/analysis/", Filename))
    
  }
  
}

img_onoff <- function(sec.by.sec.anno.list) {
  
  for (i in 1:length(sec.by.sec.anno.list)) {
    
    print(sec.by.sec.anno.list[i])
    
    on.off.log <- read.csv("./data/Visit_on_off_log.csv")
    on.off.log$id <- as.character(on.off.log$id)
    ID = as.integer(substr(sec.by.sec.anno.list[i], 6, 9))
    Visit = as.character(substr(sec.by.sec.anno.list[i], 10, 11))
    on.off.log <- filter(on.off.log, id == ID)
    on.off.log <- filter(on.off.log, visit == Visit)
    
    on.off.log$date.on <- paste(on.off.log$date.on.month,on.off.log$date.on.day,on.off.log$date.on.year,sep="/")
    on.off.log$time.on <- paste(on.off.log$time.on.hour,on.off.log$time.on.minute,on.off.log$time.on.seconds,sep=":")
    
    on.off.log$date.off <- paste(on.off.log$date.off.month,on.off.log$date.off.day,on.off.log$date.off.year,sep="/")
    on.off.log$time.off <- paste(on.off.log$time.off.hour,on.off.log$time.off.minute,on.off.log$time.off.seconds,sep=":")
    
    on.off.log$date.time.on <- paste(on.off.log$date.on, on.off.log$time.on, sep=" ")
    on.off.log$date.time.off <- paste(on.off.log$date.off, on.off.log$time.off, sep=" ")
    
    on.off.log$date.time.on <- strptime(on.off.log$date.time.on,"%m/%d/%Y %H:%M:%S")
    on.off.log$date.time.off <- strptime(on.off.log$date.time.off,"%m/%d/%Y %H:%M:%S")
    
    on.off.log$date.time.on <- 
      force_tz(on.off.log$date.time.on, tz = "America/Chicago")
    
    on.off.log$date.time.off <- 
      force_tz(on.off.log$date.time.off, tz = "America/Chicago")
    
    on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%d %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
    
    df = read.csv(paste0("./data/image", "/",
                         sec.by.sec.anno.list[i]),
                  header = T,
                  sep = ",",
                  stringsAsFactors = T)
    df$time <- strptime(df$time,"%Y-%m-%d %H:%M:%S")
    
    #	if on/off times recorded - loop through and label time monitor is not worn
    if(dim(on.off.log)[1]>0) {
      
      df$off <- 1	
      
      for (t in (1:dim(on.off.log)[1])) {
        
        on <- strptime(on.off.log$date.time.on[t],"%Y-%m-%d %H:%M:%S")
        class(on)
        off <- strptime(on.off.log$date.time.off[t],"%Y-%m-%d %H:%M:%S")
        n <- dim(df)[1]
        class(df$time)
        inds <- (1:n)[((df$time>=on)&(df$time<=off))]
        
        if (length(inds)>0) {
          
          df$off[inds] <- 0
          
        }
      }
      
      if(dim(on.off.log)[1]==0) {
        
        df$off <- "No.On.Off.Log"	
      }
    }	#end on/off loop
    
    #Clean#
    df$time <- as.POSIXct(df$time, tz = "America/Chicago")
    df <- filter(df, off == 0)
    df <- df[ , 2:5]
    df$annotation <- 
      as.character(df$annotation) #change to character for next step
    df$annotation[df$annotation == "posture;0006 sitting"] <- "0" 
    df$annotation[df$annotation == "posture;0007 standing"] <- "1" 
    df$annotation[df$annotation == "posture;0008 movement"] <- "2"
    df$annotation[df$annotation!="0" & df$annotation!="1" & df$annotation!="2"] <- "3"
    Filename2 = sec.by.sec.anno.list[i]
    write.csv(df, file = paste0("./data/image/", Filename2))
    
  }
}
process_ap <- function(ap.list, directory) {
  
  directory = directory
  
  for (i in 1:length(ap.list)) {
    
    print(ap.list[i])
    
    ###Change from Julian time to GMT###
    
    data <- read.csv(paste0(directory, 
                            ap.list[i]),
                     stringsAsFactors=FALSE)
    data <- data[,(1:6)]
    names(data) <- c("time",
                     "datacount",
                     "interval",
                     "activity",
                     "cumulativesteps",
                     "methrs")
    
    data$time <- sub("#","",data$time)
    data$time <- sub("#","",data$time)
    data[,2] <- as.numeric(as.character(data[,2]))
    data[,3] <- as.numeric(as.character(data[,3]))
    data[,4] <- as.numeric(as.character(data[,4]))
    data[,5] <- as.numeric(as.character(data[,5]))*2 #event files have half the actual number of steps for some reason
    data[,6] <- as.numeric(as.character(data[,6]))
    
    t <- dim(data)[1]
    
    data <- data[!(data[,"time"] == "1899-12-30"),]
    data <- data[!(data[,"time"] == "0"),]
    n <- dim(data)[1]		
    
    if(is.character(data$time)==TRUE&t==n) {
      
      data$time <- as.numeric(data$time)
      data$time <- as.POSIXct(as.Date(data$time,origin="1899-12-30"))
      data$time <- as.POSIXlt(data$time,tz="UTC")
      data$time <- force_tz(data$time, tz = "America/Chicago")
      data$time <- strptime(data$time,format="%Y-%m-%d %H:%M:%S")
      
    }
    
    ### second by second ###
    
    sec.by.sec.data <- data.frame(time=NA, 
                                  date=NA, 
                                  ap.posture=NA)
    sec.by.sec.data <- sec.by.sec.data[-1,]
    
    data$interval <- as.numeric(data$interval)
    
    data$methrs <- as.numeric(data$methrs)
    
    n <- dim(data)[1]
    time.of.each.event <- as.vector(difftime(strptime(data$time[seq_len(n - 1) + 1],format="%Y-%m-%d %H:%M:%S"),strptime(data$time[seq_len(n - 1)],format="%Y-%m-%d %H:%M:%S"), units="secs"))
    start.time <- strptime(data$time[1],format="%Y-%m-%d %H:%M:%S")
    
    time.of.each.event <- c(time.of.each.event, round(data[n,"interval"],0))
    te <- length(time.of.each.event)
    time.of.each.event[is.na(time.of.each.event)==T] <- 1
    events <- rep((1:te),time.of.each.event)
    
    acts <- rep(data$activity,time.of.each.event)
    n <- length(acts)
    times <- start.time+(0:(n-1))
    date <- substring(format(times),1,10)
    
    sec.by.sec.data <- merge(sec.by.sec.data, data.frame(time=times,
                                                         date=date,
                                                         ap.posture=acts, 
                                                         stringsAsFactors=FALSE), 
                             all=TRUE)
    
    ### on/off ###
    
    on.off.log <- read.csv("./Visit_on_off_log.csv")
    on.off.log$id <- as.character(on.off.log$id)
    ID = as.integer(substr(ap.list[i], 1, 4))
    Visit = as.character(substr(ap.list[i], 5, 6))
    on.off.log <- filter(on.off.log, id == ID)
    on.off.log <- filter(on.off.log, visit == Visit)
    
    on.off.log$date.on <- paste(on.off.log$date.on.month,on.off.log$date.on.day,on.off.log$date.on.year,sep="/")
    on.off.log$time.on <- paste(on.off.log$time.on.hour,on.off.log$time.on.minute,on.off.log$time.on.seconds,sep=":")
    
    on.off.log$date.off <- paste(on.off.log$date.off.month,on.off.log$date.off.day,on.off.log$date.off.year,sep="/")
    on.off.log$time.off <- paste(on.off.log$time.off.hour,on.off.log$time.off.minute,on.off.log$time.off.seconds,sep=":")
    
    on.off.log$date.time.on <- paste(on.off.log$date.on, on.off.log$time.on, sep=" ")
    on.off.log$date.time.off <- paste(on.off.log$date.off, on.off.log$time.off, sep=" ")
    
    on.off.log$date.time.on <- strptime(on.off.log$date.time.on,"%m/%d/%Y %H:%M:%S")
    on.off.log$date.time.off <- strptime(on.off.log$date.time.off,"%m/%d/%Y %H:%M:%S")
    
    on.off.log$date.time.on <- 
      force_tz(on.off.log$date.time.on, tz = "America/Chicago")
    
    on.off.log$date.time.off <- 
      force_tz(on.off.log$date.time.off, tz = "America/Chicago")
    
    on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%d %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
    
    sec.by.sec.data$time <- strptime(sec.by.sec.data$time,"%Y-%m-%d %H:%M:%S")
    
    #	if on/off times recorded - loop through and label time monitor is not worn
    if(dim(on.off.log)[1]>0) {
      
      sec.by.sec.data$off <- 1
      
      for (t in (1:dim(on.off.log)[1])) {
        
        on <- strptime(on.off.log$date.time.on[t],"%Y-%m-%d %H:%M:%S")
        class(on)
        off <- strptime(on.off.log$date.time.off[t],"%Y-%m-%d %H:%M:%S")
        n <- dim(sec.by.sec.data)[1]
        class(sec.by.sec.data$time)
        inds <- (1:n)[((sec.by.sec.data$time>=on)&(sec.by.sec.data$time<=off))]
        
        if (length(inds)>0) {
          
          sec.by.sec.data$off[inds] <- 0
        }
        
      }
      if(dim(on.off.log)[1]==0) {
        
        sec.by.sec.data$off <- "No.On.Off.Log"
        
      }
      
    }	#end on/off loop
    
    ### Clean ###
    sec.by.sec.data$time <- 
      as.POSIXct(sec.by.sec.data$time, 
                 tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
    sec.by.sec.data <- 
      sec.by.sec.data[!(sec.by.sec.data$off==1), ] #remove non-visit time#
    sec.by.sec.data$ID <-
      as.integer(substr(ap.list[i], 1, 4)) #add in ID
    sec.by.sec.data$Visit <-
      as.integer(substr(ap.list[i], 6, 6)) #add in visit
    sec.by.sec.data <- 
      sec.by.sec.data[ , c("ID", "Visit", "time", "ap.posture")] #only need ap.posture column
    Filename = as.character(substr(ap.list[i], 1, 6))
    write.csv(sec.by.sec.data, file = paste0("./analysis/ap/", Filename, ".csv"))
    
  }
  
}

process_ap_correction <- function(ap.list, directory) {
  
  directory <- directory
  
  for (i in 1:length(ap.list)) {
    
    print(ap.list[i])
    
    ###Change from Julian time to GMT###
    
    data <- read.csv(paste0(directory, ap.list[i]), stringsAsFactors=FALSE)
    data <- data[,(1:6)]
    names(data) <- c("time","datacount","interval","activity","cumulativesteps","methrs")
    
    data$time <- sub("#","",data$time)
    data$time <- sub("#","",data$time)
    data[,2] <- as.numeric(as.character(data[,2]))
    data[,3] <- as.numeric(as.character(data[,3]))
    data[,4] <- as.numeric(as.character(data[,4]))
    data[,5] <- as.numeric(as.character(data[,5]))*2 #event files have half the actual number of steps for some reason
    data[,6] <- as.numeric(as.character(data[,6]))
    
    t <- dim(data)[1]
    
    data <- data[!(data[,"time"] == "1899-12-30"),]
    data <- data[!(data[,"time"] == "0"),]
    n <- dim(data)[1]		
    
    if(is.character(data$time)==TRUE&t==n) {
      
      data$time <- as.numeric(data$time)
      data$time <- as.POSIXct(as.Date(data$time,origin="1899-12-30"))
      data$time <- as.POSIXlt(data$time,tz="UTC")
      data$time <- force_tz(data$time, tz = "America/Chicago")
      data$time <- strptime(data$time,format="%Y-%m-%d %H:%M:%S")
      data$time <- data$time + 3106.8918*24*60*60 ### CORRECTION FACTOR ###
      
    }
    
    ### second by second ###
    
    sec.by.sec.data <- data.frame(time=NA,
                                  date=NA,
                                  ap.posture=NA)
    sec.by.sec.data <- sec.by.sec.data[-1,]
    
    data$interval <- as.numeric(data$interval)
    
    data$methrs <- as.numeric(data$methrs)
    
    n <- dim(data)[1]
    time.of.each.event <- as.vector(difftime(strptime(data$time[seq_len(n - 1) + 1],format="%Y-%m-%d %H:%M:%S"),strptime(data$time[seq_len(n - 1)],format="%Y-%m-%d %H:%M:%S"), units="secs"))
    start.time <- strptime(data$time[1],format="%Y-%m-%d %H:%M:%S")
    
    time.of.each.event <- c(time.of.each.event, round(data[n,"interval"],0))
    te <- length(time.of.each.event)
    time.of.each.event[is.na(time.of.each.event)==T] <- 1
    events <- rep((1:te),time.of.each.event)
    
    acts <- rep(data$activity,time.of.each.event)
    n <- length(acts)
    times <- start.time+(0:(n-1))
    date <- substring(format(times),1,10)
    
    sec.by.sec.data <- merge(sec.by.sec.data, data.frame(time=times, 
                                                         date=date, 
                                                         ap.posture=acts,
                                                         stringsAsFactors=FALSE),
                             all=TRUE)
    
    ### on/off ###
    
    on.off.log <- read.csv("./Visit_on_off_log.csv")
    on.off.log$id <- as.character(on.off.log$id)
    ID = as.integer(substr(ap.list[i], 1, 4))
    Visit = as.character(substr(ap.list[i], 5, 6))
    on.off.log <- filter(on.off.log, id == ID)
    on.off.log <- filter(on.off.log, visit == Visit)
    
    on.off.log$date.on <- paste(on.off.log$date.on.month,on.off.log$date.on.day,on.off.log$date.on.year,sep="/")
    on.off.log$time.on <- paste(on.off.log$time.on.hour,on.off.log$time.on.minute,on.off.log$time.on.seconds,sep=":")
    
    on.off.log$date.off <- paste(on.off.log$date.off.month,on.off.log$date.off.day,on.off.log$date.off.year,sep="/")
    on.off.log$time.off <- paste(on.off.log$time.off.hour,on.off.log$time.off.minute,on.off.log$time.off.seconds,sep=":")
    
    on.off.log$date.time.on <- paste(on.off.log$date.on, on.off.log$time.on, sep=" ")
    on.off.log$date.time.off <- paste(on.off.log$date.off, on.off.log$time.off, sep=" ")
    
    on.off.log$date.time.on <- strptime(on.off.log$date.time.on,"%m/%d/%Y %H:%M:%S")
    on.off.log$date.time.off <- strptime(on.off.log$date.time.off,"%m/%d/%Y %H:%M:%S")
    
    on.off.log$date.time.on <- 
      force_tz(on.off.log$date.time.on, tz = "America/Chicago")
    
    on.off.log$date.time.off <- 
      force_tz(on.off.log$date.time.off, tz = "America/Chicago")
    
    on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%d %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
    
    sec.by.sec.data$time <- strptime(sec.by.sec.data$time,"%Y-%m-%d %H:%M:%S")
    
    #	if on/off times recorded - loop through and label time monitor is not worn
    if(dim(on.off.log)[1]>0) {
      
      sec.by.sec.data$off <- 1
      
      for (t in (1:dim(on.off.log)[1])) {
        
        on <- strptime(on.off.log$date.time.on[t],"%Y-%m-%d %H:%M:%S")
        class(on)
        off <- strptime(on.off.log$date.time.off[t],"%Y-%m-%d %H:%M:%S")
        n <- dim(sec.by.sec.data)[1]
        class(sec.by.sec.data$time)
        inds <- (1:n)[((sec.by.sec.data$time>=on)&(sec.by.sec.data$time<=off))]
        
        if (length(inds)>0) {
          
          sec.by.sec.data$off[inds] <- 0
          
        }
        
      }
      
      if(dim(on.off.log)[1]==0) {
        
        sec.by.sec.data$off <- "No.On.Off.Log"	
        
      }
      
    }	#end on/off loop
    
    ### Clean ###
    sec.by.sec.data$time <- 
      as.POSIXct(sec.by.sec.data$time, 
                 tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
    sec.by.sec.data <- 
      sec.by.sec.data[!(sec.by.sec.data$off==1), ] #remove non-visit time#
    sec.by.sec.data$ID <- 
      as.integer(substr(ap.list[i], 1, 4)) #add in ID
    sec.by.sec.data$Visit <-
      as.integer(substr(ap.list[i], 6, 6)) #add in visit
    sec.by.sec.data <- 
      sec.by.sec.data[ , c("ID", "Visit", "time", "ap.posture")] #only need ap.posture column
    Filename = as.character(substr(ap.list[i], 1, 6))
    write.csv(sec.by.sec.data, file = paste0("./analysis/ap/", Filename, ".csv"))
  }
  
}

merging.files <- function(file) {

  print(filelist3[file])
  
  image.frame <- read.csv(paste0("./data/image/", 
                                 filelist3[file]),
                          header = T,
                          sep = ",",
                          stringsAsFactors = F)
  image.frame <- image.frame[ ,-1]
  ID = as.character(substr(filelist3[file], 6, 9))
  Visit = as.character(substr(filelist3[file], 10, 11))
  
  ap.frame <- read.csv(paste0("./data/ap/", 
                              "FLAC_",
                              ID,
                              "_",
                              Visit,
                              ".csv"),
                       header = T,
                       sep = ",",
                       stringsAsFactors = F)
  ap.frame <- ap.frame[ ,-1]
  
  merged.frame <- full_join(image.frame, ap.frame)
  merged.frame <- merged.frame[ ,-c(2)]
  merged.frame <- merged.frame[complete.cases(merged.frame), ]
  merged.frame$ID <- paste(c(as.character(substr(filelist3[file], 6, 9)),
                             as.character(substr(filelist3[file], 10, 11)))
                           , collapse = "")
  Filename4 = paste0("FLAC_", ID, "_", Visit)
  write.csv(merged.frame, file = paste0("./data/merged/", Filename4, "_1sec", ".csv"))
  
}

AvsA.results <- function(one.sec.list) {
  
  counter <- 1
  
  for (i in 1:length(one.sec.list)) {
    
    print(one.sec.list[i])
    
    # 60 SEC  WINDOW
    df.1sec <- read.csv(file = paste0("./data/merged/",one.sec.list[i]),
                        stringsAsFactors = F)
    
    df.1sec$time <- as.POSIXct(df.1sec$time)
    visit.time <- as.integer(length(df.1sec$time))-1
    
    # Creating a mode function to aggregate to 60 seconds
    Modes <- function(x) {
      
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
      
    }
    
    # aggregrating
    df.60sec <- df.1sec %>% 
      group_by(ID, by60 = cut(time, "60 sec")) %>%
      summarise(annotation = Modes(annotation), ap.posture = Modes(ap.posture))
    
    # EVENTS & TRANSITION TIME
    df.events.1sec <- filter(df.1sec, annotation!= "3")
    df.events.1sec <- df.events.1sec[ ,-c(1,3)]
    write.csv(df.events.1sec, file = paste0("./data/event/",
                                            as.character(substr(one.sec.list[i], 1, 12)),
                                            "_events1sec.csv"))
    
    event.time.1sec <- as.integer(length(df.events.1sec$ID))-1
    
    df.trans.1sec <- filter(df.1sec, annotation != "0", 
                            annotation != "1",
                            annotation != "2")
    
    transition.time.1sec <- length(df.trans.1sec$ID)
    e<- transition.time.1sec
    
    df.events.60sec <- filter(df.60sec, annotation != "3")
    df.events.60sec <- df.events.60sec[ ,-c(2)]
    write.csv(df.events.60sec, file = paste0("./data/event/",
                                             as.character(substr(one.sec.list[i], 1, 12)),
                                             "_events60sec.csv"))
    
    event.time.60sec <- (as.integer(length(df.events.60sec$ID))-1)
    
    df.trans.60sec <- filter(df.60sec, annotation != "0", 
                             annotation != "1",
                             annotation != "2")
    
    transition.time.60sec <- length(df.trans.60sec$ID)
    f<- transition.time.60sec
    
    # PERCENT AGREEMENT
    
    # total
    agree.1sec <- df.1sec$annotation == df.1sec$ap.posture #TRUE = they agree
    true.1sec <- sum(agree.1sec) #sums all TRUE values, when they agree
    total.percent.agreement.1sec <- true.1sec/nrow(df.1sec)*100 #TRUE / total * 100
    a<- total.percent.agreement.1sec
    
    agree.60sec <- df.60sec$annotation == df.60sec$ap.posture
    true.60sec <- sum(agree.60sec)
    total.percent.agreement.60sec <- true.60sec/nrow(df.60sec)*100
    b<- total.percent.agreement.60sec
    
    # event
    agree.events.1sec <- df.events.1sec$annotation == df.events.1sec$ap.posture #TRUE = they agree
    true.events.1sec <- sum(agree.events.1sec) #sums all TRUE values, when they agree
    events.percent.agreement.1sec <- true.events.1sec/nrow(df.events.1sec)*100 #TRUE / total * 100
    c<- events.percent.agreement.1sec
    
    agree.events.60sec <- df.events.60sec$annotation == df.events.60sec$ap.posture
    true.events.60sec <- sum(agree.events.60sec)
    events.percent.agreement.60sec <- true.events.60sec/nrow(df.events.60sec)*100
    d<- events.percent.agreement.60sec
    
    # POSTURE TIMES
    anno.posture.times.1sec <- df.events.1sec %>% 
      group_by(annotation) %>%
      summarise(time = length(annotation))
    g <- as.integer(anno.posture.times.1sec$time[1])
    i <- as.integer(anno.posture.times.1sec$time[2])
    k <- as.integer(anno.posture.times.1sec$time[3])
    
    ap.posture.times.1sec <- df.events.1sec %>% 
      group_by(ap.posture) %>%
      summarise(time = length(ap.posture))
    h<- as.integer(ap.posture.times.1sec$time[1])
    j<- as.integer(ap.posture.times.1sec$time[2])
    l<- as.integer(ap.posture.times.1sec$time[3])
    
    #1st row = sitting
    #2nd row = standing
    #3rd row = movement
    
    anno.posture.times.60sec <- df.events.60sec %>% 
      group_by(annotation) %>%
      summarise(time = length(annotation))
    m<- as.integer(anno.posture.times.60sec$time[1])
    o<- as.integer(anno.posture.times.60sec$time[2])
    q<- as.integer(anno.posture.times.60sec$time[3])
    
    ap.posture.times.60sec <- df.events.60sec %>% 
      group_by(ap.posture) %>%
      summarise(time = length(ap.posture))
    n<- as.integer(ap.posture.times.60sec$time[1])
    p<- as.integer(ap.posture.times.60sec$time[2])
    r<- as.integer(ap.posture.times.60sec$time[3])
    
    #1st row = sitting
    #2nd row = standing
    #3rd row = movement
    
    # WRITING RESULTS TABLE
    id = as.character(df.1sec$ID[1])
    
    results.table <- data.frame(ID = NA,
                                Visit.Time = NA,
                                Total.Percent.Agreement.1sec = NA,          #a
                                Total.Percent.Agreement.60sec = NA,         #b
                                Events.Percent.Agreement.1sec = NA,         #c
                                Events.Percent.Agreement.60sec = NA,        #d
                                Transition.Time.1sec = NA,                  #e
                                Event.Time.1sec = NA,
                                anno.sitting.1sec = NA,                     #g
                                ap.sitting.1sec = NA,                       #h
                                anno.standing.1sec = NA,                    #i
                                ap.standing.1sec = NA,                      #j
                                anno.movement.1sec = NA,                    #k
                                ap.movement.1sec = NA,                      #l
                                Transition.Time.60sec = NA,                 #f
                                Event.Time.60sec = NA,
                                anno.sitting.60sec = NA,                    #m
                                ap.sitting.60sec = NA,                      #n
                                anno.standing.60sec = NA,                   #o
                                ap.standing.60sec = NA,                     #p
                                anno.movement.60sec = NA,                   #q
                                ap.movement.60sec = NA)                     #r
    
    results.table <- data.frame(ID = id,
                                Visit.Time = visit.time,
                                Total.Percent.Agreement.1sec = a,          
                                Total.Percent.Agreement.60sec = b,         
                                Events.Percent.Agreement.1sec = c,         
                                Events.Percent.Agreement.60sec = d,        
                                Transition.Time.1sec = e,                  
                                Event.Time.1sec = event.time.1sec,
                                anno.sitting.1sec = g,                     
                                ap.sitting.1sec = h,                       
                                anno.standing.1sec = i,                    
                                ap.standing.1sec = j,                      
                                anno.movement.1sec = k,                    
                                ap.movement.1sec = l,  
                                Transition.Time.60sec = f,
                                Event.Time.60sec = event.time.60sec,
                                anno.sitting.60sec = m,                    
                                ap.sitting.60sec = n,                      
                                anno.standing.60sec = o,                   
                                ap.standing.60sec = p,                     
                                anno.movement.60sec = q,                   
                                ap.movement.60sec = r)
    
    if (counter==1) {
      
      write.table(results.table,
                  file=paste(".","results.table.csv",sep="/"),sep=",",
                  row.names=F,
                  col.names=T,
                  append=F)
      
    }
    
    if (counter>1) {
      
      write.table(results.table,
                  file=paste(".","results.table.csv",sep="/"),sep=",",
                  row.names=F,
                  col.names=F,
                  append=T)
      
    }
    
    counter <- counter+1
    
  }
  
}


event.kappa.1sec <- function(event.list) {

    counter <- 1
    
    for (i in 1:length(event.list)) {
      
      print(event.list[i])
      
      temp.1.events <- read.csv(paste0("./", event.list[i]))
      id = as.character(temp.1.events$ID[1])
      temp.1.events <- temp.1.events[ ,c(3,4)]
      temp.1.kappa <-kappa2(temp.1.events, weight = "equal")
      temp.1.kappa <- do.call(rbind, temp.1.kappa)
      p.val.1  <- temp.1.kappa[8, ]
      temp.1.kappa <- temp.1.kappa[5, ]
    
      kappa_table <- data.frame(ID = id,
                                kappa_1sec = temp.1.kappa,
                                pvalue_1sec = p.val.1)
      
      if (counter==1){
        
        write.table(kappa_table,
                    file=paste(".","kappa_table_1.csv",sep="/"),sep=",",
                    row.names=F,
                    col.names=T,
                    append=F)
      
      }
      
      if (counter>1) {
        
        write.table(kappa_table,
                    file=paste(".","kappa_table_1.csv",sep="/"),sep=",",
                    row.names=F,
                    col.names=F,
                    append=T)
        
      }
      
      counter <- counter+1
        
    }
    
  }

event.kappa.60sec <- function(event.list) {
  
  counter <- 1
  
  for (i in 1:length(event.list)) {
    
    print(event.list[i])
    
    temp.60.events <- read.csv(paste0("./", event.list[i]))
    id = as.character(temp.60.events$ID[1])
    temp.60.events <- temp.60.events[ ,c(3,4)]
    temp.60.kappa <-kappa2(temp.60.events, weight = "equal")
    temp.60.kappa <- do.call(rbind, temp.60.kappa)
    p.val.60      <- temp.60.kappa[8, ]
    temp.60.kappa <- temp.60.kappa[5, ]
    
    kappa_table <- data.frame(ID = id,
                              kappa_60sec = temp.60.kappa,
                              pvalue_60sec = p.val.60
    )
    
    if (counter==1) {
      
      write.table(kappa_table,
                  file = paste(".","kappa_table_60.csv",sep="/"),sep=",",
                  row.names=F,
                  col.names=T,
                  append=F)
      
    }
    
    if (counter>1) {
      
      write.table(kappa_table,
                  file = paste(".","kappa_table_60.csv",sep="/"),sep=",",
                  row.names=F,
                  col.names=F,
                  append=T)
      
    }
    
    counter <- counter+1
    
  }
  
}

counts.1sec <- function(file) {
  
  print(event.list.1sec[file])
  
  events <- read.csv(paste0("./", event.list.1sec[file]))
  filename = as.character(substr(event.list.1sec[file], 6, 12))
  events$annotation <- as.factor(events$annotation)
  events$ap.posture <- as.factor((events$ap.posture))
  anno.levels <- levels(events$annotation)
  l <- length(anno.levels)
  ap.levels <- levels(events$ap.posture)
  n <- length(ap.levels)
  
  if (l==1) {
    
    anno.levels[length(anno.levels) + 1] <- "1"
    anno.levels[length(anno.levels) + 1] <- "2"
    events$annotation <- factor(events$annotation, levels = anno.levels)
    
  } 
  
  if (l==2) {
    
    anno.levels[length(anno.levels) + 1] <- "2"
    events$annotation <- factor(events$annotation, levels = anno.levels)
    
  } 
  
  if (n==1) {
    
    ap.levels[length(ap.levels) + 1] <- "1"
    ap.levels[length(ap.levels) + 1] <- "2"
    events$ap.posture <- factor(events$ap.posture, levels = ap.levels)
    
  } 
  
  t <- table(events$ap.posture, events$annotation)
  
  a <- t[1,1]
  b <- t[1,2]
  c <- t[1,3]
  d <- t[2,1] 
  e <- t[2,2]
  f <- t[2,3]
  g <- t[3,1]
  h <- t[3,2]
  i <- t[3,3]
  
  id <- as.character(events$ID[1])
  count <- c(a,b,c,d,e,f,g,h,i)
  activpal   <- rep(0:2, each=3)
  annotation <- rep(0:2, times=3)
  
  data <- data.frame(ID=id,
                     Activpal = activpal,
                     Annotation = annotation,
                     Count = count)
  
  write.table(data, 
              file = paste0("./counts/", filename, "1sec.csv"),
              sep = ",",
              row.names = F,
              col.names = T)
  
}

counts.60sec <- function(file) {
  
  print(event.list.60sec[file])
  
  events <- read.csv(paste0("./", event.list.60sec[file]))
  filename = as.character(substr(event.list.60sec[file], 6, 12))
  events$annotation <- as.factor(events$annotation)
  events$ap.posture <- as.factor((events$ap.posture))
  anno.levels <- levels(events$annotation)
  l <- length(anno.levels)
  ap.levels <- levels(events$ap.posture)
  n <- length(ap.levels)
  
  if (l==1) {
    
    anno.levels[length(anno.levels) + 1] <- "1"
    anno.levels[length(anno.levels) + 1] <- "2"
    events$annotation <- factor(events$annotation, levels = anno.levels)
    
  } 
  if (l==2) {
    anno.levels[length(anno.levels) + 1] <- "1"
    anno.levels <- sort(anno.levels)
    events$annotation <- factor(events$annotation, levels = anno.levels)
    
  } 
  if (n==1) {
    
    ap.levels[length(ap.levels) + 1] <- "1"
    ap.levels[length(ap.levels) + 1] <- "2"
    events$ap.posture <- factor(events$ap.posture, levels = ap.levels)
    
  } 
  
  t <- table(events$ap.posture, events$annotation)
  
  a <- t[1,1]
  b <- t[1,2]
  c <- t[1,3]
  d <- t[2,1] 
  e <- t[2,2]
  f <- t[2,3]
  g <- t[3,1]
  h <- t[3,2]
  i <- t[3,3]
  
  id <- as.character(events$ID[1])
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
  
}
