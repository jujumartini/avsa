source("./2_scripts/1_functions.R")



# prelimary ---------------------------------------------------------------

# read in on off log, needed for process functions
read_on_off_log(path = "./3_data/raw/visit_on_off_log.csv")



# image Cleaning ----------------------------------------------------------

list_anno <- toupper(list.files("./3_data/raw/annotation", ".csv"))

read_timestamps(path = "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv")

process_anno(anno_file_list = list_anno,
             corr_times = timestamps,
             on_off_log = log_on_off)
warnings()

# check files under "check folder to see if Stopwatch matches up with 1 "NEWStartTime" timestamp



# activpal cleaning -------------------------------------------------------

list_ap <- list.files("./3_data/raw/events", ".csv")

process_ap(ap_file_list = list_ap,
           on_off_log = log_on_off)
warnings()



# merging -----------------------------------------------------------------

list_anno_clean <- list.files("./3_data/processed/anno_clean/")

merge_anno_ap(list_anno = list_anno_clean)
warnings()



# create analysis tables --------------------------------------------------

list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv")

analysis_avsa(merged_list = list_merged)
warnings()