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
      
      warning(paste(substr(anno_file_list[i], 6, 11), "annotation file does not have an entry in Timestamps.csv",
                    sep = " "))
      
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
        
        warning(paste(substr(anno_file_list[i], 6, 11), "timestamp or on_off_log entry incorrect",
                      sep = " "))
        
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
      
      warning(paste(substr(ap_file_list[i], 1, 6), "not in on_off_log",
                    sep = " "))
      
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
        
        warning(paste(substr(ap_file_list[i], 1, 6), "ap file and on_off entry do not match",
                      sep = " "))
        
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

merge_anno_ap <- function(list_anno) {

  for (i in seq_along(list_anno)) {
    
    print(list_anno[i])
    
    vis_anno <- read_csv(file = paste0("./3_data/processed/anno_clean/", list_anno[i]),
                         col_names = T)
    l <- nrow(vis_anno)
    id_visit <- substr(list_anno[i], 1, 6)
    vis_ap <- read_csv(file = paste0("./3_data/processed/ap_clean/", id_visit, ".csv"),
                       col_names = T)
    
    vis_merged <- inner_join(vis_anno,
                             vis_ap,
                             by = c("ID", "Visit", "time"))
    n <- nrow(vis_merged)
    
    # Check #1
    if (l == n) {
      
      # don't need time column anymore since they are matched in time
      vis_merged <- vis_merged[, -3]
      write_csv(vis_merged,
                path = paste0("./3_data/analysis/merged_anno_ap/", id_visit, ".csv"))
      
    } else {
      
      warning(paste(id_visit, "annotation and AP file do not match in time",
                    sep = " "))
      
    }
  }
}

analysis_avsa <- function(merged_list) {
  
  counter <- 1
  
  for (i in seq_along(merged_list)) {

    print(merged_list[i])
    
    data_merged <- read_csv(file = paste0("./3_data/analysis/merged_anno_ap/",merged_list[i]),
                            col_names = T)
    data_event <- data_merged[data_merged$annotation != 3, ]
    
    # fixpoint#1: if on file does not have a posture
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
