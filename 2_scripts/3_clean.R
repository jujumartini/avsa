source("./2_scripts/1_functions.R")

# read in on off log, needed for process functions
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

# image Cleaning ----------------------------------------------------------

list_anno <- toupper(list.files("./3_data/raw/annotation", ".csv"))
corr_timestamps <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv"

process_anno(anno_file_list = list_anno,
             corr_timstamps_path = corr_timestamps)
warnings()

# check files under "check folder to see if Stopwatch matches up with 1 "NEWStartTime" timestamp


# activpal cleaning -------------------------------------------------------

list_ap <- list.files("./3_data/raw/events", ".csv")

process_ap(ap_file_list = list_ap)
warnings()


# merging -----------------------------------------------------------------

list_anno_clean <- list.files("./3_data/processed/anno_clean/")

merge_anno_ap(list_anno = list_anno_clean)
warnings()


