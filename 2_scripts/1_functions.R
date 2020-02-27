read_on_off_log <- function(path) {
  
  on_off_log <- suppressMessages(vroom(file = path,
                                       delim = ","))
  
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
  
  on_off_log <- on_off_log[, c("ID",
                               "Visit",
                               "date_time_on",
                               "date_time_off")]
  
  assign("log_on_off",
         on_off_log,
         envir = .GlobalEnv)
  
}



read_timestamps <- function(path) {
  
  corr_times <- suppressMessages(vroom(file = path,
                                       delim = ",",
                                       col_select = 1:4))

  # although cameras are in UMASS when imgs are taken, img time still captured relative to midwest time
  corr_times$StopWatch_YMD_HMS    <- suppressWarnings(ymd_hms(corr_times$StopWatch_YMD_HMS, 
                                                              tz="America/chicago"))
  corr_times$Corr_Picture_YMD_HMS <- suppressWarnings(ymd_hms(corr_times$Corr_Picture_YMD_HMS, 
                                                              tz="America/chicago"))
  
  # diff col
  corr_times$Difference <- with(corr_times,
                                difftime(StopWatch_YMD_HMS,
                                         Corr_Picture_YMD_HMS,
                                         units = "secs"))
  
  assign("timestamps",
         corr_times,
         envir = .GlobalEnv)
  
  id_miss <- corr_times$ID[is.na(corr_times$StopWatch_YMD_HMS) |
                             is.na(corr_times$Corr_Picture_YMD_HMS)]
  vis_miss <- corr_times$Visit[is.na(corr_times$StopWatch_YMD_HMS) |
                                     is.na(corr_times$Corr_Picture_YMD_HMS)]
  missing <- bind_cols(ID = id_miss,
                       Visit = vis_miss)
  missing <- paste(missing$ID,
                   missing$Visit,
                   sep = "v")
  
  for (i in seq_along(missing)) { 
    
    warning("\n\n   ", missing[i], ":",
            "\n     One of the timestamps were entered incorrectly!",
            "\n")
    
  }
}



process_anno <- function(anno_file_list,
                         corr_times,
                         on_off_log) {
  
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
    
    file_name <- sub("\\_P.*",
                     "",
                     anno_file_list[i])
    
    message("\nProcessing ", file_name, "...")
    
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
      
      # write a "check" csv file to see if stopwatch matches NEW start time
      write.table(mer_anno,
                  file = paste0("./3_data/processed/anno_check/",id, "V", visit, ".csv"),
                  sep = ",",
                  row.names = F)
      
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
        
        # write table
        write.table(vis_anno,
                    file = paste0("./3_data/processed/anno_clean/", file_name, ".csv"),
                    sep = ",",
                    row.names = F)
      }
    }
  }
}



process_ap <- function(ap_file_list,
                       on_off_log) {
  
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



process_irr <- function(anno_file_list, 
                        corr_times,
                        on_off_log) {
  
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
    
    file_name <- anno_file_list[i]
    
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
}



merge_anno_ap <- function(list_anno) {

  for (i in seq_along(list_anno)) {
    
    print(list_anno[i])
    
    vis_anno <- read_csv(file = paste0("./3_data/processed/anno_clean/", list_anno[i]),
                         col_names = T)
    l <- nrow(vis_anno)
    id_visit <- substr(list_anno[i], 6, 11)
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



merge_irr <- function(list_irr) {
  
  # list names of visits
  index <- str_sub(list_irr,
                   start = 1,
                   end = 11) %>% 
    unique()
  
  for (i in seq_along(index)) {
    
    # list files per visit
    list_visit <- list_irr[str_detect(list_irr,
                                      index[i])]
    
    # get id and visit for table
    id_visit <- str_sub(list_visit[1],
                        start = 1,
                        end = 11)
    
    message("Calculating IRR for ", id_visit, "...\n")
    
    for (i in seq_along(list_visit)) {
      
      file_name <- list_visit[i]
      
      # read in one visit file
      irr_img <- suppressMessages(vroom(file = paste("./3_data/processed/irr_clean",
                                    file_name,
                                    sep = "/"),
                       delim = ","))
      
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
                  path = "./3_data/analysis/irr_table.csv",
                  delim = ",",
                  append = F)
      
    } else if (i > 1) {
      
      vroom_write(irr_table,
                  path = "./3_data/analysis/irr_table.csv",
                  delim = ",",
                  append = T)
      
    }
  }
}


analysis_avsa <- function(merged_list) {
  
  counter <- 1
  
  for (i in seq_along(merged_list)) {

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
    
    counter <- counter+1

  }
}



create_visit_summary <- function() {

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
  
}



create_bias_table <- function() {
  
  data_time <- suppressMessages(vroom(file = "./3_data/analysis/table_analysis_time.csv",
                                      delim = ","))
  
  tbl_bias_time <- data.frame(Posture = c("Sit",
                                          "Stand",
                                          "Move"))
  
  # raw means:
  tbl_bias_time$AP_mean  <- c(mean(data_time$sit_ap),
                              mean(data_time$stand_ap),
                              mean(data_time$move_ap))
  tbl_bias_time$AP_sd    <- c(sd(data_time$sit_ap),
                              sd(data_time$stand_ap),
                              sd(data_time$move_ap))
  tbl_bias_time$IMG_mean <- c(mean(data_time$sit_anno),
                              mean(data_time$stand_anno),
                              mean(data_time$move_anno))
  tbl_bias_time$IMG_sd   <- c(sd(data_time$sit_anno),
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
  tbl_bias_time$Bias <- c(fixef(sitmodel),
                          fixef(standmodel),
                          fixef(movemodel))
  
  # se is "unexplained variability" in the biases
  tbl_bias_time$SE <- c(as.data.frame(VarCorr(sitmodel))[2,5],
                        as.data.frame(VarCorr(standmodel))[2,5],
                        as.data.frame(VarCorr(movemodel))[2,5])
  
  # APproximate 95% CIs
  tbl_bias_time$Upper_95_Bias <- tbl_bias_time$Lower_95_Bias <- NA
  tbl_bias_time[,8:9] <- suppressMessages(rbind(confint(sitmodel,3),
                                                confint(standmodel,3),
                                                confint(movemodel,3)))
  
  # % sum table
  tbl_bias_perc <- tbl_bias_time
  
  tbl_bias_perc$Bias          <- (tbl_bias_time$Bias / tbl_bias_time$AP_mean)*100
  tbl_bias_perc$SE            <- (tbl_bias_time$SE/tbl_bias_time$AP_mean)*100
  tbl_bias_perc$Lower_95_Bias <- (tbl_bias_time$Lower_95_Bias/tbl_bias_time$AP_mean)*100
  tbl_bias_perc$Upper_95_Bias <- (tbl_bias_time$Upper_95_Bias/tbl_bias_time$AP_mean)*100
  
  # round to 1 digit, arbitrarily
  tbl_bias_time[,-1] <- round(tbl_bias_time[,-1], 1)
  tbl_bias_perc[,-1] <- round(tbl_bias_perc[,-1], 1)
  
  # output tables
  write_rds(tbl_bias_time,
            path = "./4_results/posture_bias_time.rds",
            compress = "none")
  write_rds(tbl_bias_perc,
            path = "./4_results/posture_bias_perc.rds",
            compress = "none")
  vroom_write(tbl_bias_time,
              path = "./4_results/posture_bias_time.csv",
              delim = ",")
  vroom_write(tbl_bias_perc,
              path = "./4_results/posture_bias_perc.csv",
              delim = ",")
  
  assign("tbl_bias_time",
         tbl_bias_time,
         envir = .GlobalEnv)
  
  assign("tbl_bias_perc",
         tbl_bias_perc,
         envir = .GlobalEnv)
  
}



create_miss_table <- function() {
  
  data_time <- suppressMessages(vroom(file = "./3_data/analysis/table_analysis_time.csv",
                                      delim = ","))
  
  tbl_miss_time <- data.frame(Posture = c("Sit",
                                          "Stand",
                                          "Move"))
  
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

  # output tables
  write_rds(tbl_miss_time,
            path = "./4_results/posture_miss_time.rds",
            compress = "none")
  write_rds(tbl_miss_perc,
            path = "./4_results/posture_miss_perc.rds",
            compress = "none")
  vroom_write(tbl_miss_time,
              path = "./4_results/posture_miss_time.csv",
              delim = ",")
  vroom_write(tbl_miss_perc,
              path = "./4_results/posture_miss_perc.csv",
              delim = ",")
  
  assign("tbl_miss_time",
         tbl_miss_time,
         envir = .GlobalEnv)
  
  assign("tbl_miss_perc",
         tbl_miss_perc,
         envir = .GlobalEnv)
  
  
}