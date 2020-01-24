source("./2_scripts/1_functions.R")

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