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



# IRR ---------------------------------------------------------------------

irr_table <- vroom(file = "./3_data/analysis/irr_table.csv",
                   delim = ",",
                   col_names = c("id_visit",
                                 "kripp_full",
                                 "kripp_gapless"))
mean(irr_table$kripp_full)
min(irr_table$kripp_full)
max(irr_table$kripp_full)

mean(irr_table$kripp_gapless)
min(irr_table$kripp_gapless)
max(irr_table$kripp_gapless)



# bias --------------------------------------------------------------------

create_bias_table()



# misclassification -------------------------------------------------------

create_miss_table()



# Figures: Plots ----------------------------------------------------------

# pos_figure_bias_minutes, save as 800 by 500 pixels
tbl_bias_time <- read_rds(path = "./4_results/summary_posture_time.rds")
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

# pos_figure_bias_percent, save as 800 by 500 pixels
tbl_bias_perc <- read_rds(path = "./4_results/summary_posture_perc.rds")
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

# pos_figure_miss_minutes, save as 850 by 600 (reshape2)
tbl_miss_time <- read_rds(path = "./4_results/posture_miss_time.rds")
{
  
  # read in only classifications
  graph <- tbl_miss_time[ , -which(colnames(tbl_miss_time) %in% c("AP_Total"))]
  
  # rename IMG to correct
  colnames(graph)[2] <- "Correct"
  
  # change table into variables that represent x, y, other
  graph <- graph %>% 
    melt(id.vars = "Posture")
  
  # clean
  colnames(graph)[2] <- "Classification"
  
  graph <- graph[graph$value != 0, ]
  
  graph$Posture <- factor(graph$Posture,
                          levels = c("Sit",
                                     "Stand",
                                     "Move"))
  
  # plot
  ggplot(data = graph) +
    geom_bar(mapping = aes(x = Posture,
                           y = value,
                           fill = Classification),
             stat = "identity") +
    geom_text(data = tbl_miss_time,
              mapping = aes(x = Posture,
                            y = AP_Total,
                            label = paste(AP_Total, "mins")),
              vjust = -0.5) +
    geom_text(mapping = aes(x = Posture,
                            y = value,
                            fill = Classification,
                            label = paste(value, "mins")),
              position = position_stack(vjust = 0.5)) +
    theme(plot.title = element_text(lineheight = 1,
                                    hjust = .5),
          text = element_text(size = 15)) +
    labs(title = "Total AP estimates classified by IMGs",
         x = "Posture",
         y = "Minutes") +
    scale_fill_manual(values = c("#3399FF",
                                 "#FF6666",
                                 "#9999FF",
                                 "#FF9933",
                                 "#99CC99"))
  
}

# pos_figure_miss_percent, save as 800 by 600 (reshape2)
tbl_miss_perc <- read_rds(path = "./4_results/posture_miss_perc.rds")
{
  
  # read in only classifications
  graph <- tbl_miss_perc[ , -which(colnames(tbl_miss_perc) %in% c("AP_Total"))]
  
  # rename IMG to correct
  colnames(graph)[2] <- "Correct"
  
  # change table into variables that represent x, y, other
  graph <- graph %>% 
    melt(id.vars = "Posture")
  
  # clean
  colnames(graph)[2] <- "Classification"
  
  graph <- graph[graph$value != 0, ]
  
  graph$Posture <- factor(graph$Posture,
                          levels = c("Sit",
                                     "Stand",
                                     "Move"))
  
  # label
  lbl <- tbl_miss_perc
  lbl$label <- lbl$AP_Total/lbl$AP_Total*100
  
  # plot
  ggplot(data = graph) +
    geom_bar(mapping = aes(x = Posture,
                           y = value,
                           fill = Classification),
             stat = "identity") +
    geom_text(data = lbl,
              mapping = aes(x = Posture,
                            y = label,
                            label = paste(AP_Total, "mins")),
              vjust = -0.5) +
    geom_text(mapping = aes(x = Posture,
                            y = value,
                            fill = Classification,
                            label = paste0(value, "%")),
              position = position_stack(vjust = 0.5)) +
    theme(plot.title = element_text(lineheight = 1,
                                    hjust = .5),
          text = element_text(size = 15)) +
    labs(title = "Proportion of Total AP estimates classified by IMGs",
         x = "Posture",
         y = "% of Total AP Estimates") +
    scale_fill_manual(values = c("#3399FF",
                                 "#FF6666",
                                 "#9999FF",
                                 "#FF9933",
                                 "#99CC99"))
  
}



# Figures: Tables ---------------------------------------------------------

# pos_table_bias_minutes, save as 750 by 175 pixels
tbl_bias_time <- read_rds(path = "./4_results/posture_bias_time.rds")
{
  
  tbl <- tbl_bias_time
  
  # merge mean and sd columns, /u00b1 is plus/minus symbol
  tbl$AP <- paste(tbl$AP_mean,
                  tbl$AP_sd,
                  sep = " \u00b1 ")
  
  tbl$IMG <- paste(tbl$IMG_mean,
                   tbl$IMG_sd,
                   sep = " \u00b1 ")
  
  # clean
  tbl <- tbl[, c("Posture",
                 "IMG",
                 "AP",
                 "Bias",
                 "SE",
                 "Lower_95_Bias",
                 "Upper_95_Bias")]
  
  colnames(tbl) <- c("Posture",
                     "Image min \u00b1 SD",
                     "activPAL min \u00b1 SD",
                     "Bias",
                     "Standard Error",
                     "Lower 95% CI",
                     "Higher 95% CI")
  
  # format
  formattable(tbl,
              list(Posture = formatter("span",
                                       style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# pos_table_bias_percent, save as 650 by 175 pixels
tbl_bias_perc <- read_rds(path = "./4_results/posture_bias_perc.rds")
{
  
  tbl <- tbl_bias_perc
  
  # merge mean and sd columns, /u00b1 is plus/minus symbol
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
                     "% Bias",
                     "% SE",
                     "Lower 95% CI %",
                     "Higher 95% CI %")

  # format
  formattable(tbl,
              list("Posture" = formatter("span",
                                         style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# pos_table_miss_minutes, save as 600 by 175 pixels
tbl_miss_time <- read_rds(path = "./4_results/posture_miss_time.rds")
{
  
  tbl <- tbl_miss_time
  
  # clean
  colnames(tbl) <- c("Posture",
                     "Total AP min",
                     "Correct",
                     "Transition",
                     "Sit",
                     "Stand",
                     "Move")
  
  # format
  formattable(tbl,
              list(Posture = formatter("span",
                                       style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# pos_table_miss_percent, save as 600 by 175 pixels
tbl_miss_perc <- read_rds(path = "./4_results/posture_miss_perc.rds")
{
  
  tbl <- tbl_miss_perc
  
  # add percent symbol
  tbl$IMG <- paste0(tbl$IMG,
                    c("%", "%", "%"))
  
  tbl$Transition <- paste0(tbl$Transition,
                           c("%", "%", "%"))
  
  tbl$Sit <- paste0(tbl$Sit,
                    c("%", "%", "%"))
  
  tbl$Stand <- paste0(tbl$Stand,
                      c("%", "%", "%"))
  
  tbl$Move <- paste0(tbl$Move,
                     c("%", "%", "%"))
  
  # clean
  colnames(tbl) <- c("Posture",
                     "Total AP min",
                     "% Correct",
                     "% Transition",
                     "% Sit",
                     "% Stand",
                     "% Move")
  
  # format
  formattable(tbl,
              list("Posture" = formatter("span",
                                         style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}



# sedentary vs upright ----------------------------------------------------

create_sedentary_tables()

# sed_table_bias_minutes, save as 650 by 145 pixels
tbl_bias_time <- read_rds(path = "./4_results/sedentary_bias_time.rds")
{
  
  tbl <- tbl_bias_time
  
  # merge mean and sd columns, /u00b1 is plus/minus symbol
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

# sed_figure_miss_percent, save as 800 by 600 (reshape2)
tbl_miss_perc <- read_rds(path = "./4_results/sedentary_miss_perc.rds")
{
  
  # read in only classifications
  graph <- tbl_miss_perc[ , -which(colnames(tbl_miss_perc) %in% c("AP_Total"))]
  
  # rename IMG to correct
  colnames(graph)[2] <- "Correct"
  
  # change table into variables that represent x, y, other
  graph <- graph %>% 
    melt(id.vars = "Posture")
  
  # clean
  colnames(graph)[2] <- "Classification"
  
  graph <- graph[graph$value != 0, ]
  
  graph$Posture <- factor(graph$Posture,
                          levels = c("Sedentary",
                                     "Upright"))
  
  # label
  lbl <- tbl_miss_perc
  lbl$label <- lbl$AP_Total/lbl$AP_Total*100
  
  # plot
  ggplot(data = graph) +
    geom_bar(mapping = aes(x = Posture,
                           y = value,
                           fill = Classification),
             stat = "identity") +
    geom_text(data = lbl,
              mapping = aes(x = Posture,
                            y = label,
                            label = paste(AP_Total, "mins")),
              vjust = -0.5) +
    geom_text(mapping = aes(x = Posture,
                            y = value,
                            fill = Classification,
                            label = paste0(value, "%")),
              position = position_stack(vjust = 0.5)) +
    theme(plot.title = element_text(lineheight = 1,
                                    hjust = .5),
          text = element_text(size = 15)) +
    labs(title = "Proportion of Total AP estimates classified by IMGs",
         x = "Posture",
         y = "% of Total AP Estimates") +
    scale_fill_manual(values = c("#3399FF",
                                 "#FF6666",
                                 "#9999FF",
                                 "#FF9933",
                                 "#99CC99"))
  
}
