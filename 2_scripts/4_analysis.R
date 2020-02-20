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



# summary posture --------------------------------

create_posture_summary()



# Plots -------------------------------------------------------------------

tbl_sum_pos_time <- read_rds(path = "./4_results/summary_posture_time.rds")
tbl_sum_pos_perc <- read_rds(path = "./4_results/summary_posture_perc.rds")

# posture min bias
plot(1:3,
     1:3,
     xlab = "",
     ylab = "Bias (95% CI) in minutes",
     xlim = c(.5,3.5),
     ylim = range(tbl_sum_pos_time[,c("bias",
                                      "lower_95_bias",
                                      "upper_95_bias" )]),
     type = "n",
     axes = F,
     font.lab = 2)

title(xlab = "Posture",
      font.lab = 2,
      line = 2.5)
title(sub = paste0("* Mean posture min from AP for sitting = ",
                   tbl_sum_pos_time$ap_mean[1],
                   "', standing = ",
                   tbl_sum_pos_time$ap_mean[2],
                   "', and movement = ",
                   tbl_sum_pos_time$ap_mean[3],
                   "' *"), 
      font.sub = 1,
      adj = 0.5)

axis(2)
axis(1,
     at = 1:3,
     labels = tbl_sum_pos_time$posture)
abline(h = 0,
       lty = 3)
points(1:3,
       tbl_sum_pos_time$bias,
       pch = 16,
       cex = 1.5)

for (i in 1:3) {
  
  lines(c(i,i),
        c(tbl_sum_pos_time$lower_95_bias[i],tbl_sum_pos_time$upper_95_bias[i]),
        lty = 2)
  text(i,
       tbl_sum_pos_time$bias[i],
       paste(tbl_sum_pos_time$bias[i],"min"),
       pos = 4,
       cex = 1.25)
  
}

# posture perc bias
plot(1:3,
     1:3,
     xlab = "",
     ylab = "Bias (95% CI) as %",
     xlim = c(.5,3.5),
     ylim = range(tbl_sum_pos_perc[, c("bias",
                                       "lower_95_bias",
                                       "upper_95_bias" )]),
     type = "n",
     axes = F,
     font.lab = 2)

title(xlab = "Posture",
      font.lab = 2,
      line = 2.5)
title(sub = paste0("* Mean posture min from AP for sitting = ",
                   tbl_sum_pos_time$ap_mean[1],
                   "', standing = ",
                   tbl_sum_pos_time$ap_mean[2],
                   "', and movement = ",
                   tbl_sum_pos_time$ap_mean[3],
                   "' *"), 
      font.sub = 1,
      adj = 0.5)

axis(2)
axis(1,
     at = 1:3,
     labels = tbl_sum_pos_perc$posture)
abline(h = 0,
       lty = 3)
points(1:3,
       tbl_sum_pos_perc$bias,
       pch = 16,
       cex = 1.5)

for (i in 1:3) {
  
  lines(c(i,i),
        c(tbl_sum_pos_perc$lower_95_bias[i], tbl_sum_pos_perc$upper_95_bias[i]),
        lty = 2)
  text(i,
       tbl_sum_pos_perc$bias[i],
       paste0(tbl_sum_pos_perc$bias[i],"%"),
       pos = 4,
       cex = 1.25)
  
}


