source("./2_scripts/2_functions.R")


# image Cleaning ----------------------------------------------------------

# Second by Second Function

anno_list <- toupper(list.files("./3_data/raw/annotation", ".csv"))
corr_timestamps <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv"
log <- "visit_on_off_log.csv"

process_anno(anno_file_list = anno_list,
             corr_timstamps_path = corr_timestamps,
             on_off_log = log)

warnings()

# check files under "check folder to see if Stopwatch matches up with 1 "NEWStartTime" timestamp

# on and off function #

filelist2 = list.files("./data/image", ".CSV")
for (i in 1:length(filelist2)) {
  image.on.off(i)
}


# activpal cleaning -------------------------------------------------------

### NO CORRECTION ###

aplist = list.files("./raw/analysis", ".csv")
for (i in 1:length(aplist)) { 
  ap.no.correction(i) 
}

### WITH CORRECTION ###

aplist2 = list.files("./raw/analysis_correction", ".csv")
for (i in 1:length(aplist2)){
  ap.with.correction(i)
}


# errors ------------------------------------------------------------------

## image 1013V2 has two 1/16/2018 16:34:49 ##
image.frame <- read.csv(paste0("./data/image", 
                               "/",
                               "FLAC_1013V2_POSTURE_CHANG.CSV"),
                        header = T,
                        sep = ",",
                        stringsAsFactors = T)
# remove row 50 #
image.frame <- image.frame[-c(50), ]
image.frame <- image.frame[ ,-1]
# write #
write.csv(image.frame, file = paste0("./data/image", 
                                     "/",
                                     "FLAC_1013V2_POSTURE_CHANG.CSV"))

## image 1013V3 has two 2018-02-06 17:35:00 ##
image.frame <- read.csv(paste0("./data/image", 
                               "/",
                               "FLAC_1013V3_POSTURE_CHANG.CSV"),
                        header = T,
                        sep = ",",
                        stringsAsFactors = T)
# remove row 1 #
image.frame <- image.frame[-c(1), ]
image.frame <- image.frame[ ,-1]
# write #
write.csv(image.frame, file = paste0("./data/image", 
                                     "/",
                                     "FLAC_1013V3_POSTURE_CHANG.CSV"))


# merging -----------------------------------------------------------------

filelist3 = list.files("./data/image", ".CSV")
for (i in 1:length(filelist3)) {
  merging.files(i)
}

warnings()


# analysis ----------------------------------------------------------------


