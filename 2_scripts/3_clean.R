source("./2_scripts/1_functions.R")

# prelimary ---------------------------------------------------------------

# read in on off log, needed for process functions
on_off_log <- 
  read_on_off_log(
    path = "./3_data/raw",
    name_log = "visit_on_off_log.csv"
  )



# image Cleaning ----------------------------------------------------------

list_anno <- 
  list.files(path = "./3_data/raw/annotation",
             pattern = ".csv") %>% 
  str_to_upper()
timestamps <- 
  read_timestamps(
    fpa_timestamps = "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp",
    fnm_timestamps = "TimeStamps.csv"
  )
process_annotation(
  fpa_img_raw = "./3_data/raw/annotation",
  fpa_img_clean = "./3_data/processed/anno_clean",
  fls_img_raw = list_anno,
  tib_cor_time = timestamps,
  log_on_off = on_off_log
)



# activpal cleaning -------------------------------------------------------

process_activpal2(
  fpa_ap_raw = "./3_data/raw/events",
  fpa_ap_clean = "./3_data/processed/ap_clean",
  log_on_off = on_off_log,
  id_visits = NULL
)



# IRR cleaning ------------------------------------------------------------

list_irr <- list.files("./3_data/raw/IRR",
                       ".csv")

read_timestamps(path = "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv")

process_irr(anno_file_list = list_irr,
            corr_times = timestamps,
            on_off_log = log_on_off)



# merging -----------------------------------------------------------------

irr_list <- list.files("./3_data/processed/irr_clean",
                       ".csv")

merge_irr(list_irr = irr_list)

merge_img_ap(
  fpa_img_clean = "./3_data/processed/anno_clean",
  fpa_ap_clean = "./3_data/processed/ap_clean",
  fpa_merged = "./3_data/analysis/merged_anno_ap"
)



# create analysis tables --------------------------------------------------

list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv")

analysis_avsa(merged_list = list_merged)
warnings()

analysis_sedentary(merged_list = list_merged)
