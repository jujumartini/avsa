source("./2_scripts/1_functions.R")

# summary visit ------------------------------------------------------------

# check to see which columns have NAs
data_time <- read_csv(file = "./3_data/analysis/table_analysis_time.csv")
data_percentage <- read_csv(file = "./3_data/analysis/table_analysis_percentage.csv")

# seeing which columns have NAs
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
}

nacols(data_time)
nacols(data_percentage)

# create visit summary table
create_visit_summary()



# bias --------------------------------------------------------------------

create_bias_table()


# misclassification -------------------------------------------------------

create_miss_table()



# Figures: Plots ----------------------------------------------------------

tbl_bias_time <- read_rds(path = "./4_results/summary_posture_time.rds")
tbl_bias_perc <- read_rds(path = "./4_results/summary_posture_perc.rds")

# figure_bias_minutes, save as 800 by 500 pixels
{
  
  plot(1:3,
       1:3,
       xlab = "",
       ylab = "Bias (95% CI) in minutes",
       xlim = c(.5,3.5),
       ylim = range(tbl_bias_time[,c("Bias",
                                     "Lower_95_Bias",
                                     "Upper_95_Bias" )]),
       type = "n",
       axes = F,
       font.lab = 2)
  
  title(xlab = "Posture",
        font.lab = 2,
        line = 2.5)
  title(sub = paste0("* Mean event min from AP for sitting = ",
                     tbl_bias_time$AP_mean[1],
                     "', standing = ",
                     tbl_bias_time$AP_mean[2],
                     "', and movement = ",
                     tbl_bias_time$AP_mean[3],
                     "' *"), 
        font.sub = 1,
        adj = 0.5)
  
  axis(2)
  axis(1,
       at = 1:3,
       labels = tbl_bias_time$Posture)
  abline(h = 0,
         lty = 3)
  points(1:3,
         tbl_bias_time$Bias,
         pch = 16,
         cex = 1.5)
  
  for (i in 1:3) {
    
    lines(c(i,i),
          c(tbl_bias_time$Lower_95_Bias[i],tbl_bias_time$Upper_95_Bias[i]),
          lty = 2)
    text(i,
         tbl_bias_time$Bias[i],
         paste0(tbl_bias_time$Bias[i],"min"),
         pos = 4,
         cex = 1.25)
    
  }
}

# figure_bias_percent, save as 800 by 500 pixels
{
  plot(1:3,
       1:3,
       xlab = "",
       ylab = "Bias (95% CI) in minutes",
       xlim = c(.5,3.5),
       ylim = range(tbl_bias_perc[,c("Bias",
                                     "Lower_95_Bias",
                                     "Upper_95_Bias" )]),
       type = "n",
       axes = F,
       font.lab = 2)
  
  title(xlab = "Posture",
        font.lab = 2,
        line = 2.5)
  title(sub = paste0("* Mean event min from AP for sitting = ",
                     tbl_bias_perc$AP_mean[1],
                     "', standing = ",
                     tbl_bias_perc$AP_mean[2],
                     "', and movement = ",
                     tbl_bias_perc$AP_mean[3],
                     "' *"), 
        font.sub = 1,
        adj = 0.5)
  
  axis(2)
  axis(1,
       at = 1:3,
       labels = tbl_bias_perc$Posture)
  abline(h = 0,
         lty = 3)
  points(1:3,
         tbl_bias_perc$Bias,
         pch = 16,
         cex = 1.5)
  
  for (i in 1:3) {
    
    lines(c(i,i),
          c(tbl_bias_perc$Lower_95_Bias[i],tbl_bias_perc$Upper_95_Bias[i]),
          lty = 2)
    text(i,
         tbl_bias_perc$Bias[i],
         paste0(tbl_bias_perc$Bias[i],"%"),
         pos = 4,
         cex = 1.25)
    
  }
}

# Figures: Tables ---------------------------------------------------------

tbl_sum_vis <- read_rds(path = "./4_results/summary_visit.rds")
tbl_bias_time <- read_rds(path = "./4_results/posture_bias_time.rds")
tbl_bias_perc <- read_rds(path = "./4_results/posture_bias_perc.rds")
tbl_miss_time <- read_rds(path = "./4_results/posture_miss_time.rds")
tbl_miss_perc <- read_rds(path = "./4_results/posture_miss_perc.rds")

# table_bias_minutes, save as 650 by 175 pixels
{
  # merge mean and sd columns, /u00b1 is plus/minus symbol
  tbl <- tbl_bias_time
  tbl$AP <- paste(tbl$AP_mean,
                  tbl$AP_sd,
                  sep = " \u00b1 ")
  
  tbl$IMG <- paste(tbl$IMG_mean,
                   tbl$IMG_sd,
                   sep = " \u00b1 ")
  
  # clean
  tbl <- tbl[, c("Posture",
                 "AP",
                 "IMG",
                 "Bias",
                 "SE",
                 "Lower_95_Bias",
                 "Upper_95_Bias")]
  
  colnames(tbl) <- c("Posture",
                     "AP min \u00b1 SD",
                     "IMG min \u00b1 SD",
                     "Bias",
                     "SE",
                     "Lower 95% CI",
                     "Higher 95% CI")
  
  # format
  formattable(tbl,
              list(Posture = formatter("span",
                                       style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# table_bias_percent, save as 650 by 175 pixels
{
  # merge mean and sd columns, /u00b1 is plus/minus symbol
  tbl <- tbl_bias_perc
  tbl$AP <- paste(tbl$AP_mean,
                  tbl$AP_sd,
                  sep = " \u00b1 ")
  
  tbl$IMG <- paste(tbl$IMG_mean,
                   tbl$IMG_sd,
                   sep = " \u00b1 ")
  
  # add percent symbol
  tbl$Bias <- paste0(tbl$Bias,
                     c("%", "%", "%"))
  
  tbl$SE <- paste0(tbl$SE,
                   c("%", "%", "%"))
  
  tbl$Lower_95_Bias <- paste0(tbl$Lower_95_Bias,
                              c("%", "%", "%"))
  
  tbl$Upper_95_Bias <- paste0(tbl$Upper_95_Bias,
                              c("%", "%", "%"))
  
  # clean
  tbl <- tbl[, c("Posture",
                 "AP",
                 "IMG",
                 "Bias",
                 "SE",
                 "Lower_95_Bias",
                 "Upper_95_Bias")]
  
  colnames(tbl) <- c("Posture",
                     "AP min \u00b1 SD",
                     "IMG min \u00b1 SD",
                     "Bias",
                     "SE",
                     "Lower 95% CI",
                     "Higher 95% CI")

  # format
  formattable(tbl,
              list("Posture" = formatter("span",
                                         style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# table_miss_minutes, save as 650 by 175 pixels



# table_miss_percent, save as 650 by 175 pixels
formattable(tbl_miss_time,
            list(Posture = formatter("span",
                                     style = x ~ style(font.weight = "bold"))),
            align = c("l", "c", "c", "c", "c", "c", "c"))


# percent
tbl_bias_perc <- tbl_sum_pos_perc[, -c(4, 5, 10, 11, 12, 13)]
tbl_miss_perc <- tbl_sum_pos_perc[, c(1, 4, 5, 10, 11, 12, 13)]

# capitalizing
substr(colnames(tbl_bias_perc), 1, 1) <- toupper(substr(colnames(tbl_bias_perc), 1, 1))
substr(colnames(tbl_bias_perc)[2:3], 1, 2) <- toupper(substr(colnames(tbl_bias_perc)[2:3], 1, 2))

tbl_bias_perc$Posture <- as.character(tbl_bias_perc$Posture)
substr(tbl_bias_perc$Posture, 1, 1) <- toupper(substr(tbl_bias_perc$Posture, 1, 1))

substr(colnames(tbl_miss_perc), 1, 1) <- toupper(substr(colnames(tbl_miss_perc), 1, 1))
substr(colnames(tbl_miss_perc)[2:7], 1, 3) <- toupper(substr(colnames(tbl_miss_perc)[2:7], 1, 3))

tbl_miss_perc$Posture <- as.character(tbl_miss_perc$Posture)
substr(tbl_miss_perc$Posture, 1, 1) <- toupper(substr(tbl_miss_perc$Posture, 1, 1))

# format
formattable(tbl_bias_perc,
            list(Posture = formatter("span",
                                     style = x ~ style(font.weight = "bold"))),
            align = c("l", "c", "c", "c", "c", "c", "c"))

formattable(tbl_miss_perc,
            list(Posture = formatter("span",
                                     style = x ~ style(font.weight = "bold"))),
            align = c("l", "c", "c", "c", "c", "c", "c"))

# misclassification
list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv",
                          full.names = T)

merged_all <- suppressMessages(lapply(list_merged,
                     vroom,
                     delim = ","))
merged_all <- bind_rows(merged_all)
merged_all <- merged_all[merged_all$annotation != 3, ]
sitting_all <- merged_all[merged_all$ap_posture == 0,]
total_posture <- merged_all %>% 
  group_by(ID, Visit, ap_posture, annotation) %>% 
  summarise(total_ap = sum(ap_posture),
            total_img = sum(annotation))
total_posture <- total_posture/60

  
time_matr_all <- table(merged_all$ap_posture,
                       merged_all$annotation)
time_matr_all <- addmargins(time_matr_all)
time_matr_all <- time_matr_all/60
time_matr_all
