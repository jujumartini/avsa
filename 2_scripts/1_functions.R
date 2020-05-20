read_on_off_log <- 
  function(path,
           name_log) {
  
  # path = "./3_data/raw"
  # name_log = "visit_on_off_log.csv"
  
  on_off_raw <- 
    suppressMessages(
      vroom(
        file = paste(path,
                     name_log,
                     sep = "/"),
        delim = ","
      )
    )
  
  # Consistency: column names with _ & all columns lowercase
  colnames(on_off_raw) <- 
    str_replace_all(
      colnames(on_off_raw),
      pattern = "\\.",
      replacement = "_"
    ) %>% 
  str_to_lower()
  
  # begin
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
  
  on_off_clean <- 
    on_off_raw[, c("id",
                   "visit",
                   "date_time_on",
                   "date_time_off")]
  
  
  # seconds on for seeing if all on times are applied
  on_off_clean$seconds_on <- 
    as.vector(
      difftime(
        on_off_clean$date_time_off,
        on_off_clean$date_time_on,
        units = "secs"
      )
    )
  
  # vec_clean_ids <- unique(paste0(on_off_clean$id,
  #                                on_off_clean$visit))
  # 
  # if (all(subjects_visits %in% vec_clean_ids) == F) {
  # 
  #   ind_missing <- which(subjects_visits %in% vec_clean_ids == F)
  #   list_missing <- subjects_visits[ind_missing]
  # 
  #   for (missing_on_off_entry in list_missing)
  # 
  #     warning("ID ", missing_on_off_entry, ":\n",
  #             "    Subject log has entry for ID BUT!\n",
  #             "    No corresponding entry found in on off log.\n",
  #             "    Please put entry in ", name_log, " before preceeding.\n",
  #             call. = F)
  # 
  # }

  if (all(on_off_clean$seconds_on > 0) == F) {

    # off entry is before on entry, only for subjects included in subject log
    ind_vis_mis <- which(on_off_clean$seconds_on <= 0)

    vec_mis_ids <- paste0(on_off_clean$id[ind_vis_mis],
                          on_off_clean$visit[ind_vis_mis])

    # list_mis_on_off <- paste(on_off_clean$date_time_on[ind_vis_mis],
    #                          on_off_clean$date_time_off[ind_vis_mis],
    #                          sep = " - ")

    # see where mistake entries (whole row pasted together) lie in raw
    vec_clean_entries <- apply(on_off_clean[, 1:4],
                               MARGIN = 1,
                               FUN = paste,
                               collapse = "")
    vec_mis_entries <- apply(on_off_clean[ind_vis_mis, 1:4],
                             MARGIN = 1,
                             FUN = paste,
                             collapse = "")

    ind_raw_mis <- which(vec_clean_entries %in% vec_mis_entries)

    vec_mis_on_off <-
      paste(
        format.POSIXct(on_off_raw$date_time_on[ind_raw_mis],
                       format = "%m/%d/%Y %H:%M:%S"),
        format.POSIXct(on_off_raw$date_time_off[ind_raw_mis],
                       format = "%m/%d/%Y %H:%M:%S"),
        sep = " - "
      )
    
    for (i in seq_along(vec_mis_ids)) {
      
      mis_num    <- ind_raw_mis[i] + 1
      mis_id     <- vec_mis_ids[i]
      mis_on_off <- vec_mis_on_off[i]
      
      warning("Mistake in Line #", mis_num, ":\n",
              "    ", mis_id, ", ", mis_on_off, ".\n",
              "    Off time takes place before on time.\n",
              "    Please fix entry in ", name_log, " before preceeding.\n",
              call. = F)
      
    }
    
  }
  
  # on_off_clean
  # paste(log_on_off$ID)
  
  # check to see for duplicates
  # on_off_vis[duplicated(on_off_vis$date_time_on), ]
  # duplicated(on_off_vis$date_time_on)
  # duplicated(on_off_vis)
  # 
  # ind_dup_on <- which(duplicated(on_off_vis$date_time_on) | duplicated(on_off_vis$date_time_on,
  #                                                                      fromLast = T))
  # 
  # ind_dup_off <- which(duplicated(on_off_vis$date_time_off) | duplicated(on_off_vis$date_time_off,
  #                                                                      fromLast = T))
  # 
  # 
  # dup_on <- on_off_vis[ind_dup_on, ]
  # dup_off <- on_off_vis[ind_dup_off, ]
  # 
  # dup_on$seconds_on <= 0
  # 
  # 
  # 
  # duplicated(on_off_vis$date_time_on) | duplicated(on_off_vis$date_time_on,
  #                                                  fromLast = T)
  # 
  # on_off_log[on_off_log$seconds_on <= 0, ]
  
  return(on_off_clean)
  
}



read_timestamps <- 
  function(fpa_timestamps,
           fnm_timestamps) {
  
  # fpa_timestamps <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp"
  # fnm_timestamps <- "TimeStamps.csv"
  
  tib_cor_times <- 
    suppressMessages(
      vroom(
        file = paste(
          fpa_timestamps,
          fnm_timestamps,
          sep = "/"
        ),
        delim = ",",
        col_select = 1:4,
        progress = FALSE
      )
    )
  
  # Consistency: column names with _ & all columns lowercase
  colnames(tib_cor_times) <- 
    str_replace_all(
      colnames(tib_cor_times),
      pattern = "\\.",
      replacement = "_"
    ) %>% 
    str_to_lower()

  # although cameras are in UMASS when imgs are taken, img time still captured relative to midwest time
  tib_cor_times$stopwatch_ymd_hms <- 
    suppressWarnings(
      tib_cor_times$stopwatch_ymd_hms %>% 
        lubridate::ymd_hms(tz = "America/chicago")
    )
  tib_cor_times$corr_picture_ymd_hms <- 
    suppressWarnings(
      tib_cor_times$corr_picture_ymd_hms %>% 
        lubridate::ymd_hms(tz = "America/chicago")
    )
  tib_cor_times$difference <- 
    difftime(
      tib_cor_times$stopwatch_ymd_hms,
      tib_cor_times$corr_picture_ymd_hms,
      units = "secs"
    )
  
  # If there are any missing entries.
  id_miss <- tib_cor_times$id[is.na(tib_cor_times$stopwatch_ymd_hms) |
                             is.na(tib_cor_times$corr_picture_ymd_hms)]
  vis_miss <- tib_cor_times$visit[is.na(tib_cor_times$stopwatch_ymd_hms) |
                                     is.na(tib_cor_times$corr_picture_ymd_hms)]
  missing <- bind_cols(id = id_miss,
                       visit = vis_miss)
  missing <- paste(missing$id,
                   missing$visit,
                   sep = "v")
  
  for (i in seq_along(missing)) { 
    
    warning("\n\n   ", missing[i], ":",
            "\n     One of the timestamps were entered incorrectly!",
            "\n")
    
  }
  
  return(tib_cor_times)
  
}



process_annotation <- 
  function(fpa_img_raw,
           fpa_img_clean,
           fls_img_raw,
           tib_cor_time,
           log_on_off) {
  
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
    message("Processing File #", i, ": ", stu_sub_vis,
            appendLF = TRUE)
    
    # STEP 1: raw ----
    message("    Reading...",
            appendLF = FALSE)
    tib_img_raw <- suppressMessages(
      c(fpa_img_raw, fnm_img_raw) %>%
        paste(collapse = "/") %>%
        vroom(delim = ",",
              progress = FALSE)
    )

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
      merge(
        tib_img_raw,
        tib_cor_time,
        by = c("id",
               "visit")
      )
    
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
    message("Sec-By-Sec...",
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
    message("Off-Times...",
            appendLF = FALSE)
    
    tib_img_sbs$off <- 1
    tib_on_off <-
      log_on_off[log_on_off$id == id & log_on_off$visit == visit, ]
    nrw_on_off <- nrow(tib_on_off)
    dbl_sec_applied <-
      vector(
        mode = "double",
        length = nrw_on_off
      )
    
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
    message("Cleaning...\n",
            appendLF = TRUE)
    tib_img_vis <- tib_img_sbs[tib_img_sbs$off == 0,]
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
    tib_img_vis <- 
      tib_img_vis[, c("id",
                      "visit",
                      "time",
                      "date",
                      "annotation")]
    
    # write table
    fnm_img_clean <- 
      paste(
        "FLAC",
        id,
        paste0("V", visit),
        "IMG.csv",
        sep = "_"
      )
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
    "                              ", cnt_img_processed - 1, " files processed.\n",
    "\n",
    "File times are in UTC.\n",
    appendLF = TRUE
  )
  
}



process_activpal2 <- function(fpa_ap_raw,
                              fpa_ap_clean,
                              log_on_off,
                              id_visits = NULL) {
  
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
  
  for (i in seq_along(fls_ap_raw)) {
    
    fnm_ap_raw <- fls_ap_raw[i]
    id_visit <-
      fnm_ap_raw %>% 
      str_sub(start = 1,
              end = 6)
    id <- 
      fnm_ap_raw %>% 
      str_sub(start = 1,
              end = 4)
    visit <- 
      fnm_ap_raw %>% 
      str_sub(start = 6,
              end = 6)
    message("Processing File #", i, ": ", id_visit,
            appendLF = TRUE)
    
    # STEP 1: Clean Raw AP File ----
    message("    Reading...",
            appendLF = FALSE)
    tib_ap_raw <- suppressMessages(
      c(fpa_ap_raw, fnm_ap_raw) %>% 
        paste(collapse = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
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
      tib_ap_raw$time %>% 
      str_replace(pattern = "#",
                  replacement = "")
    tib_ap_raw$time <- 
      tib_ap_raw$time %>% 
      str_replace(pattern = "#",
                  replacement = "")

    # event files have half the actual number of steps for some reason
    tib_ap_raw$cumulativesteps <- tib_ap_raw$cumulativesteps * 2

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
      
    } else {
      
      stop("THIS ISNT SUPPPPPPPOSE TO HAPPPPPPEN")
      
    }
    
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
      warning(
        "AP File ", id_visit, ":\n",
        "    No entry in on_off_log.\n",
        call. = FALSE
      )
      
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
    message("Sec-By-Sec...",
            appendLF = FALSE)
    
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
    raw_dtm_strpd <-
      raw_dtm %>% 
      strptime(format = "%Y-%m-%d %H:%M:%OS") %>%
      as.POSIXct(tz = "America/Chicago")
    int_event_time <- 
      raw_dtm_strpd %>% 
      diff.POSIXt(lag = 1,
                  differences = 1) %>%
      as.vector()
    int_event_time[is.na(int_event_time)] <- 1
    int_event_time <- as.integer(int_event_time)
    
    # Make sbs variables.
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
      tib_ap_sbs$mets %>% 
      signif(digits = 3)
    
    #	STEP 3: Label on off times ----
    message("Off-Times...",
            appendLF = FALSE)
    tib_ap_sbs$off <- 1
    nrw_on_off <- nrow(tib_on_off)
    dbl_sec_applied <- 
      vector(
        mode = "double",
        length = nrw_on_off
      )
    
    #	If on/off times recorded - loop through and label time monitor is not worn.
    for (i in seq_len(nrw_on_off)) {
      
      dtm_on  <- tib_on_off$date_time_on[i]
      dtm_off <- tib_on_off$date_time_off[i]
      
      # Can use integer subsetting as time should never have an NA at this point.
      ind_on <- 
        tib_ap_sbs$time %>% 
        dplyr::between(left = dtm_on,
                       right = dtm_off) %>%
        which()

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
    int_sec_applied_all <- 
      dbl_sec_applied %>% 
      sum() %>% 
      as.integer()
    
    if (int_sec_applied_all <= 0 ||
        int_sec_worn <= 0) {
      
      # none of the AP (IMG) file has any of the log entries.
      message("\n")
      warning(
        "AP File ", id_visit, ":\n",
        "    Event file and on_off_log entries do not match at all.\n",
        "    AP likely not worn at all or corrupt.\n",
        call. = FALSE
      )
      
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

        warning(
          "AP File ", id_visit, ":\n",
          "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
          "    Interval partially found in Event File.\n",
          "    Fix entry or remove file.\n",
          call. = FALSE
        )

      }

      for (i in seq_along(ind_log_full)) {

        mis_num    <- ind_log_full[i]
        mis_on_off <- chr_mis_time_full[i]

        warning(
          "AP File ", id_visit, ":\n",
          "    On off entry #", mis_num + 1, ", ", mis_on_off, ".\n",
          "    Interval not found at all in Event File.\n",
          "    Fix entry or remove file.\n",
          call. = FALSE
        )

      }

      next()
      
    } else if (int_sec_worn != int_sec_applied_all) {
      
      # on intervals are in AP (IMG) but one on interval overlaps another interval.
      message("\n")
      warning(
        "AP File ", id_visit, ":\n",
        "    Event file and on_off_log entries partially match.\n",
        "    One or more log entries overlap each other.\n",
        "    Check on_off entries.\n",
        call. = FALSE
      )
      
      next()
      
    }
    
    # STEP 4 : Clean (AVSA Specific) ----
    message("Cleaning...\n",
            appendLF = TRUE)
    
    # Make file with off time cleaned out. REMEMBER! This still includes
    # the "zero" seconds of each on interval as it is still needed.
    tib_ap_vis <- tib_ap_sbs[tib_ap_sbs$off == 0, ]
    tib_ap_vis$id <- id
    tib_ap_vis$visit <- visit
    tib_ap_vis <- 
      tib_ap_vis[ , c("id",
                      "visit",
                      "time",
                      "date",
                      "ap_posture")]
    
    # write data frame
    fnm_ap_clean <- 
      paste(
        "FLAC",
        id,
        paste0("V", visit),
        "AP.csv",
        sep = "_"
      )
    vroom::vroom_write(
      tib_ap_vis,
      path = paste(fpa_ap_clean,
                   fnm_ap_clean,
                   sep = "/"),
      delim = ",",
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



merge_img_ap <- function(fpa_img_clean,
                         fpa_ap_clean,
                         fpa_merged) {
  
  # fpa_img_clean <- "./3_data/processed/anno_clean"
  # fpa_ap_clean <- "./3_data/processed/ap_clean"
  # fpa_merged <- "./3_data/analysis/merged_anno_ap"
  # fnm_img <- "FLAC_1002_V3_IMG.csv"
  
  fls_img_clean <- 
    list.files(
      path = fpa_img_clean,
      pattern = "FLAC"
    )
  fls_ap_clean <- 
    list.files(
      path = fpa_ap_clean,
      pattern = "FLAC"
    )
  
  for (i in seq_along(fls_img_clean)) {
    
    fnm_img <- fls_img_clean[i]
    
    message("Merging File #", i, ": ", fnm_img, "...",
            appendLF = FALSE)

    study_id_visit <- 
      fnm_img %>% 
      str_replace(pattern = "_[^_]*$",
                  replacement = "")
    lgl_ap_present <- 
      fls_ap_clean %>% 
      str_detect(pattern = study_id_visit)
    
    if (any(lgl_ap_present) == FALSE) {
      
      # No corresponding activPAl file.
      message("\n")
      warning(
        "IMG File ", fnm_img, ":\n",
        "    No corresponding AP file found.",
        .call = FALSE
      )
      
      next()
      
    }
    
    # "^[^_]*_" <- for removing everything before first occurance.
    ind_ap <- 
      fls_ap_clean %>% 
      str_detect(pattern = study_id_visit) %>% 
      which()
    fnm_ap <- fls_ap_clean[ind_ap]
    tib_img <- suppressMessages(
      c(fpa_img_clean,
        fnm_img) %>% 
        paste(collapse = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    )
    tib_ap <- suppressMessages(
      c(fpa_ap_clean,
        fnm_ap) %>% 
        paste(collapse = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    )

    tib_merged <- 
      inner_join(
        tib_img,
        tib_ap,
        by = c("id",
               "visit",
               "time", 
               "date")
      )
    
    nrw_img <- nrow(tib_img)
    nrw_merged <- nrow(tib_merged)
    
    # Check #1
    if (nrw_img != nrw_merged) {
      
      message("\n")
      warning(
        study_id_visit, ":\n",
        "    IMG and AP file do not match in time\n",
        .call = FALSE
      )
      
      next()
       
    }
    
    # Remove "on" time as it is not part of data.
    tib_final <- tib_merged[-1, ]
    
    # Don't need time column anymore since they are matched in time.
    tib_final <- 
      tib_final[, !(colnames(tib_final) %in% c("time",
                                               "date"))]
    
    # Write table.
    fnm_merged <- 
      paste0(
        study_id_visit,
        ".csv"
      )
    vroom_write(
      tib_merged,
      path = paste(fpa_merged, 
                   fnm_merged, 
                   sep = "/"),
      delim = ",")
    
    message("done\n",
            appendLF = )
    
  }
  
  
  
}



# merge_anno_ap <- function(list_anno) {
# 
#   for (i in seq_along(list_anno)) {
#     
#     print(list_anno[i])
#     
#     vis_anno <- read_csv(file = paste0("./3_data/processed/anno_clean/", list_anno[i]),
#                          col_names = T)
#     l <- nrow(vis_anno)
#     id_visit <- substr(list_anno[i], 6, 11)
#     vis_ap <- read_csv(file = paste0("./3_data/processed/ap_clean/", id_visit, ".csv"),
#                        col_names = T)
#     
#     vis_merged <- inner_join(vis_anno,
#                              vis_ap,
#                              by = c("ID", "Visit", "time"))
#     n <- nrow(vis_merged)
#     
#     # Check #1
#     if (l == n) {
#       
#       # don't need time column anymore since they are matched in time
#       vis_merged <- vis_merged[, -3]
#       write_csv(vis_merged,
#                 path = paste0("./3_data/analysis/merged_anno_ap/", id_visit, ".csv"))
#       
#     } else {
#       
#       warning(paste(id_visit, "annotation and AP file do not match in time",
#                     sep = " "))
#       
#     }
#   }
# }



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

analysis_sedentary <- function(merged_list) {
  
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

create_sedentary_tables <- function() {
  
  data_time <- suppressMessages(vroom(file = "./3_data/analysis/table_sedentary_time.csv",
                                      delim = ","))
  
  tbl_bias_time <- data.frame(Posture = c("Sedentary",
                                          "Upright"))
  
  # raw means:
  tbl_bias_time$AP_mean  <- c(mean(data_time$sed_ap),
                              mean(data_time$upright_ap))
  tbl_bias_time$AP_sd    <- c(sd(data_time$sed_ap),
                              sd(data_time$upright_ap))
  tbl_bias_time$IMG_mean <- c(mean(data_time$sed_anno),
                              mean(data_time$upright_anno))
  tbl_bias_time$IMG_sd   <- c(sd(data_time$sed_anno),
                              sd(data_time$upright_anno))
  
  # linear mixed effects model: bias~b0 + b_i + e_ij
  sedmodel <- lmer(sed_anno - sed_ap ~ 1 + (1|ID),
                   data = data_time)
  uprightmodel <- lmer(upright_anno - upright_ap ~ 1 + (1|ID),
                     data = data_time)
  # biases are estimated from model
  tbl_bias_time$Bias <- c(fixef(sedmodel),
                          fixef(uprightmodel))
  
  # se is "unexplained variability" in the biases
  tbl_bias_time$SE <- c(as.data.frame(VarCorr(sedmodel))[2,5],
                        as.data.frame(VarCorr(uprightmodel))[2,5])
  
  # APproximate 95% CIs
  tbl_bias_time$Upper_95_Bias <- tbl_bias_time$Lower_95_Bias <- NA
  tbl_bias_time[,8:9] <- suppressMessages(rbind(confint(sedmodel,3),
                                                confint(uprightmodel,3)))
  
  # % sum table
  tbl_bias_perc <- tbl_bias_time
  
  tbl_bias_perc$Bias          <- (tbl_bias_time$Bias / tbl_bias_time$AP_mean)*100
  tbl_bias_perc$SE            <- (tbl_bias_time$SE/tbl_bias_time$AP_mean)*100
  tbl_bias_perc$Lower_95_Bias <- (tbl_bias_time$Lower_95_Bias/tbl_bias_time$AP_mean)*100
  tbl_bias_perc$Upper_95_Bias <- (tbl_bias_time$Upper_95_Bias/tbl_bias_time$AP_mean)*100
  
  # classification
  tbl_miss_time <- data.frame(Posture = c("Sedentary",
                                          "Upright"))
  
  # total ap times
  tbl_miss_time$AP_Total <- c(mean(data_time$total_sed_ap),
                              mean(data_time$total_upr_ap))
  
  # IMG correct classification
  tbl_miss_time$IMG <- c(mean(data_time$sed_agree),
                         mean(data_time$upr_agree))
  
  # transedion misclassification
  tbl_miss_time$Transition <- c(mean(data_time$sed_trans),
                                mean(data_time$upr_trans))
  
  # sed misclassifications
  tbl_miss_time$Sed   <- c(0,
                           mean(data_time$upr_mis_sed))
  
  tbl_miss_time$Upr <- c(mean(data_time$sed_mis_upr),
                         0)

  # percent table
  tbl_miss_perc <- tbl_miss_time
  
  tbl_miss_perc[, 3:6] <- (tbl_miss_perc[, 3:6]/tbl_miss_perc$AP_Total)*100
  
  # round to 1 digit, arbitrarily
  tbl_bias_time[,-1] <- round(tbl_bias_time[,-1], 1)
  tbl_bias_perc[,-1] <- round(tbl_bias_perc[,-1], 1)
  tbl_miss_time[, -1] <- round(tbl_miss_time[, -1], 1)
  tbl_miss_perc[, -1] <- round(tbl_miss_perc[, -1], 1)
  
  # output tables
  write_rds(tbl_bias_time,
            path = "./4_results/sedentary_bias_time.rds",
            compress = "none")
  write_rds(tbl_bias_perc,
            path = "./4_results/sedentary_bias_perc.rds",
            compress = "none")

  write_rds(tbl_miss_time,
            path = "./4_results/sedentary_miss_time.rds",
            compress = "none")
  
  write_rds(tbl_miss_perc,
            path = "./4_results/sedentary_miss_perc.rds",
            compress = "none")
  
  assign("sed_bias_time",
         tbl_bias_time,
         envir = .GlobalEnv)
  
  assign("sed_bias_perc",
         tbl_bias_perc,
         envir = .GlobalEnv)
  
  assign("sed_miss_time",
         tbl_miss_time,
         envir = .GlobalEnv)
  
  assign("sed_miss_perc",
         tbl_miss_perc,
         envir = .GlobalEnv)
  
}



# need to clean up but what was used for Autumn.
avtivpal_results <- function() {
  
  ##	STEP COUNTS. cumulative steps reported in file so change to total steps/day
  # First need to subset on_off rows
  inds_on_beg <- which(
    tib_ap_vis$time %in%
      tib_on_off$date_time_on
  )
  inds_on_end <- which(
    tib_ap_vis$time %in%
      tib_on_off$date_time_off
  )
  inds_on_beg_end <- sort(c(inds_on_beg, inds_on_end))
  
  # tib_ap_vis[inds_beg_end,]
  dble_steps_beg_end <- tib_ap_vis$steps[inds_on_beg_end]
  
  # subtract "off" steps from "on" steps to get steps when worn
  dble_steps_count <- vector(mode = "double",
                             length = length(dble_steps_beg_end))
  # seq_along(dble_steps_beg_end) %% 2
  
  for (n in seq_along(dble_steps_beg_end)) {
    
    if (n %% 2 == 1) next()
    
    dble_steps_count[n] <-
      dble_steps_beg_end[n] -
      dble_steps_beg_end[n - 1]
    
  }
  
  dble_steps_count <-
    dble_steps_count[seq_along(dble_steps_count) %% 2 == 0]
  
  # make data frame for cases when they take off AP more than once or overnight
  data_steps_daily <- tibble(
    date  = date(tib_on_off$date_time_on),
    steps = dble_steps_count,
    .rows = nrow_on_off
  )
  
  ## INTENSITY MINUTES. light & mvpa only when stepping.
  tib_ap_vis$intensity <- "light"
  tib_ap_vis$intensity[which(tib_ap_vis$ap_posture == 0)] <- "sedentary"
  tib_ap_vis$intensity[which(tib_ap_vis$mets >= 3)] <- "mvpa"
  
  inds_active <- which(
    (tib_ap_vis$mets == 1.40 | tib_ap_vis$mets == 1.25) == FALSE
  )
  
  ## MET HOURS. create data frame that have seconds for each MET value in each day
  ## BUT excluding beginnig of on intervals (equivalent to zeros).
  data_ap_met1 <- tapply(tib_ap_vis$off[-inds_on_beg] == 0,
                         INDEX = list(tib_ap_vis$date[-inds_on_beg],
                                      tib_ap_vis$mets[-inds_on_beg]),
                         FUN = sum)  %>%
    t()                              %>%
    as.data.frame()                  %>%
    rownames_to_column(var = "mets") %>%
    melt(id.vars       = "mets",
         variable.name = "date",
         value.name    = "seconds")
  
  data_ap_met1 <- data_ap_met1[is.na(data_ap_met1$seconds) == FALSE, ]
  data_ap_met1$mets <- as.double(data_ap_met1$mets)
  data_ap_met1 <- data_ap_met1[, c("date",
                                   "mets",
                                   "seconds")]
  
  data_ap_met1$met_hrs <- (data_ap_met1$mets * data_ap_met1$seconds) / 3600
  data_ap_met1$intensity <- "light"
  data_ap_met1$intensity[data_ap_met1$mets == 1.25] <- "sedentary"
  data_ap_met1$intensity[data_ap_met1$mets >= 3.00] <- "mvpa"
  
  # decker_process_ap2 specific: remove stand and sedentary times
  inds_active_met <- which(
    (data_ap_met1$mets == 1.25 | data_ap_met1$mets == 1.40) == FALSE
  )
  data_ap_met2 <- data_ap_met1[inds_active_met, ]
  
  inds_light2 <- which(data_ap_met2$intensity == "light")
  inds_mvpa2 <- which(data_ap_met2$intensity == "mvpa")
  
  # Just in case beginning of on interval is active
  inds_results <- inds_active[inds_active %in% inds_on_beg == FALSE]
  
  # needed for dates that will be fully sedentary.
  fctr_vis_dates <- as.factor(tib_ap_vis$date)
  
  # make .csv file with PA and SB variables per day
  tble_results <- tibble(
    sub   = as.integer(id),
    visit = visit,
    date  = unique(tib_ap_vis$date),
    
    step_count   = tapply(data_steps_daily$steps,
                          INDEX = data_steps_daily$date,
                          FUN = sum),
    hours_worn   = tapply(tib_ap_vis$off[-inds_on_beg] == 0,
                          INDEX = tib_ap_vis$date[-inds_on_beg],
                          FUN = sum) / 3600,
    sed_mins     = tapply(tib_ap_vis$intensity[-inds_on_beg] == "sedentary",
                          INDEX = tib_ap_vis$date[-inds_on_beg],
                          FUN = sum) / 60,
    lit_mins     = tapply(tib_ap_vis$intensity[inds_results] == "light",
                          INDEX = fctr_vis_dates[inds_results],
                          FUN = sum) / 60,
    mvpa_mins    = tapply(tib_ap_vis$intensity[inds_results] == "mvpa",
                          INDEX = fctr_vis_dates[inds_results],
                          FUN = sum) / 60,
    lit_met_hrs  = tapply(data_ap_met2$met_hrs[inds_light2],
                          INDEX = data_ap_met2$date[inds_light2],
                          FUN = sum),
    mvpa_met_hrs = tapply(data_ap_met2$met_hrs[inds_mvpa2],
                          INDEX = data_ap_met2$date[inds_mvpa2],
                          FUN = sum),
    .rows = length(unique(tib_ap_vis$date))
  )
  
  tble_means_sum <- tibble(
    sub         = as.integer(id),
    visit       = visit,
    days_on_off = length(unique(date(tib_on_off$date_time_on))),
    days_file   = length(levels(fctr_vis_dates)),
    
    step_count    = mean(tble_results$step_count,
                         na.rm = TRUE),
    sd_sc         = sd(tble_results$step_count,
                       na.rm = TRUE),
    
    hours_worn    = mean(tble_results$hours_worn,
                         na.rm = TRUE),
    sd_hw         = sd(tble_results$hours_worn,
                       na.rm = TRUE),
    
    sed_mins      = mean(tble_results$sed_mins,
                         na.rm = TRUE),
    sd_sm         = sd(tble_results$sed_mins,
                       na.rm = TRUE),
    
    lit_mins      = mean(tble_results$lit_mins,
                         na.rm = TRUE),
    sd_lm         = sd(tble_results$lit_mins,
                       na.rm = TRUE),
    
    mvpa_mins     = mean(tble_results$mvpa_mins,
                         na.rm = TRUE),
    sd_mm         = sd(tble_results$mvpa_mins,
                       na.rm = TRUE),
    
    lit_met_hrs   = mean(tble_results$lit_met_hrs,
                         na.rm = TRUE),
    sd_lmh        = sd(tble_results$lit_met_hrs,
                       na.rm = TRUE),
    
    mvpa_met_hrs  = mean(tble_results$mvpa_met_hrs,
                         na.rm = TRUE),
    sd_mmh        = sd(tble_results$mvpa_met_hrs,
                       na.rm = TRUE),
    
    sum_lit_hrs   = sum(tble_results$lit_met_hrs,
                        na.rm = TRUE),
    sum_mvpa_hrs  = sum(tble_results$mvpa_met_hrs,
                        na.rm = TRUE),
    .rows = 1
  )
  
  tble_results[is.na(tble_results)] <- 0
  tble_results[, -(1:3)]   <- round(tble_results[, -(1:3)],
                                    digits = 1)
  tble_means_sum[, -(1:2)] <- round(tble_means_sum[, -(1:2)],
                                    digits = 1)
  
  
  if (cnt_ap == 1) {
    
    vroom_write(tble_results,
                path = paste0(path_results,
                              "results_table6.csv"),
                delim = ",",
                append = FALSE,
                progress = FALSE)
    vroom_write(tble_means_sum,
                path = paste0(path_results,
                              "means_sums_table.csv"),
                delim = ",",
                append = FALSE,
                progress = FALSE)
    
    
  } else if (cnt_ap > 1) {
    
    vroom_write(tble_results,
                path = paste0(path_results,
                              "results_table6.csv"),
                delim = ",",
                append = TRUE,
                progress = FALSE)
    vroom_write(tble_means_sum,
                path = paste0(path_results,
                              "means_sums_table.csv"),
                delim = ",",
                append = TRUE,
                progress = FALSE)
    
    
  }
  
}
