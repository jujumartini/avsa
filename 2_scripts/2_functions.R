process_anno <- function(anno_file_list, corr_timstamps_path) {
  
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
    
    # merge times and raw
    id <- as.integer(substr(anno_file_list[i], 6, 9))
    visit <- as.integer(substr(anno_file_list[i], 11, 11))
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
                  file = paste0("./3_data/processed/anno_check/",id, "V", visit, ".csv"),
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
  }
}

process_ap <- function(ap_file_list) {
  
  # create clean ap files
  for (i in seq_along(ap_file_list)) {
    
    print(ap_file_list[i])
    
    raw_ap <- read.table(file = paste0("./3_data/raw/events/", ap_file_list[i]),
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
    id <- substr(ap_file_list[i], 1, 4)
    visit <- substr(ap_file_list[i], 6, 6)
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
