# means and sd ------------------------------------------------------------

data_time <- read_csv(file = "./3_data/analysis/table_analysis_time.csv")
data_percentage <- read_csv(file = "./3_data/analysis/table_analysis_percentage.csv")

# changing NaN to 0
data_percentage$move_agree[is.nan(data_percentage$move_agree)] <- 0
data_percentage$move_mis_sit[is.nan(data_percentage$move_mis_sit)] <- 0
data_percentage$move_mis_stand[is.nan(data_percentage$move_mis_stand)] <- 0

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

# get the length of all possible names to make other vectors same length
n <- length(union(names(avg_times), names(avg_percs)))

length(avg_times) <- n
length(sd_times) <- n
length(avg_percs) <- n
length(sd_percs) <- n

# reorder times and perc vectors into correct position where you want the names in one to fill the spots of the other
names(avg_times)
avg_times <- avg_times[c("ID",
                         "Visit",
                         "visit_time",
                         "event_time",
                         "",
                         "trans_time",
                         "",
                         "",
                         "",
                         "sit_ap",
                         "sit_anno",
                         "stand_ap",
                         "stand_anno",
                         "move_ap",
                         "move_anno",
                         "sit_agree",
                         "stand_agree",
                         "move_agree",
                         "sit_mis_stand", 
                         "sit_mis_move",
                         "stand_mis_sit",
                         "stand_mis_move",
                         "move_mis_sit",
                         "move_mis_stand")]

sd_times <- sd_times[c("ID",
                       "Visit",
                       "visit_time",
                       "event_time",
                       "",
                       "trans_time",
                       "",
                       "",
                       "",
                       "sit_ap",
                       "sit_anno",
                       "stand_ap",
                       "stand_anno",
                       "move_ap",
                       "move_anno",
                       "sit_agree",
                       "stand_agree",
                       "move_agree",
                       "sit_mis_stand", 
                       "sit_mis_move",
                       "stand_mis_sit",
                       "stand_mis_move",
                       "move_mis_sit",
                       "move_mis_stand")]

names(avg_percs)
avg_percs <- avg_percs[c("ID",
                         "Visit",
                         "",
                         "",
                         "perc_event",
                         "",
                         "perc_trans",
                         "total_agree",
                         "event_agree",
                         "sit_ap",
                         "sit_anno",
                         "stand_ap",
                         "stand_anno",
                         "move_ap",
                         "move_anno",
                         "sit_agree",
                         "stand_agree",
                         "move_agree",
                         "sit_mis_stand", 
                         "sit_mis_move",
                         "stand_mis_sit",
                         "stand_mis_move",
                         "move_mis_sit",
                         "move_mis_stand")]

sd_percs <- sd_percs[c("ID",
                       "Visit",
                       "",
                       "",
                       "perc_event",
                       "",
                       "perc_trans",
                       "total_agree",
                       "event_agree",
                       "sit_ap",
                       "sit_anno",
                       "stand_ap",
                       "stand_anno",
                       "move_ap",
                       "move_anno",
                       "sit_agree",
                       "stand_agree",
                       "move_agree",
                       "sit_mis_stand", 
                       "sit_mis_move",
                       "stand_mis_sit",
                       "stand_mis_move",
                       "move_mis_sit",
                       "move_mis_stand")]

# combine the name vectors and reassign them
bind_names <- coalesce(names(avg_times), names(avg_percs))

names(avg_times) <- bind_names
names(sd_times) <- bind_names
names(avg_percs) <- bind_names
names(sd_percs) <- bind_names

descriptive_table <- rbind(avg_times,
                           sd_times,
                           avg_percs,
                           sd_percs)
descriptive_table <- as.data.frame(descriptive_table)
write.csv(descriptive_table,
          file = "./4_results/visit_descriptives.csv")
write_csv(descriptive_table,
          path = "./4_results/visit_descriptives.csv",
          col_names = T)
write
# linear mixed effects model - bias, SE, CI, means, sd --------------------------------

# fit models: bias~b0 + b_i + e_ij
sitmodel <- lmer(sit_anno - sit_ap ~ 1 + (1|ID),data = data_time)
standmodel <- lmer(stand_anno - stand_ap ~ 1 + (1|ID),data = data_time)
movemodel <- lmer(move_anno - move_ap ~ 1 + (1|ID),data = data_time)


# make a table for the results:
summary_table <- data.frame(posture = c("sit","stand","move"))

# biases are estimated from model
summary_table$bias <- c(fixef(sitmodel),
                        fixef(standmodel),
                        fixef(movemodel))

# sd is "unexplained variability" in the biases
VarCorr(sitmodel)
as.data.frame(VarCorr(sitmodel))
summary_table$sd <- c(as.data.frame(VarCorr(sitmodel))[2,5],
                      as.data.frame(VarCorr(standmodel))[2,5],
                      as.data.frame(VarCorr(movemodel))[2,5])

# approximate 95% CIs
summary_table$upper_95_bias <- summary_table$lower_95_bias <- NA
confint(sitmodel)
confint(sitmodel, 3)
summary_table[,4:5] <- rbind(confint(sitmodel,3),
                             confint(standmodel,3),
                             confint(movemodel,3))

# raw means:
summary_table$ap_min_mean <- c(mean(data_time$sit_ap),
                               mean(data_time$stand_ap),
                               mean(data_time$move_ap))
summary_table$ap_min_sd <- c(sd(data_time$sit_ap),
                             sd(data_time$stand_ap),
                             sd(data_time$move_ap))
summary_table$img_min_mean <- c(mean(data_time$sit_anno),
                                mean(data_time$stand_anno),
                                mean(data_time$move_anno))
summary_table$img_min_sd <- c(sd(data_time$sit_anno),
                              sd(data_time$stand_anno),
                              sd(data_time$move_anno))

# % sum table
perc_sum_table <- summary_table

perc_sum_table$bias <- 100*summary_table$bias / summary_table$ap_min_mean
perc_sum_table$sd <- 100*summary_table$sd/summary_table$ap_min_mean
perc_sum_table$lower_95_bias  <- 100*summary_table$lower_95_bias/summary_table$ap_min_mean
perc_sum_table$upper_95_bias <- 100*summary_table$upper_95_bias/summary_table$ap_min_mean

# round to 1 digit, arbitrarily
summary_table[,-1] <- round(summary_table[,-1], 1)
perc_sum_table[,-1] <- round(perc_sum_table[,-1], 1)

write_csv(summary_table,
          path = "./4_results/summary_table_mins.csv",
          col_names = T)
write_csv(perc_sum_table,
          path = "./4_results/summary_table_perc.csv",
          col_names = T)


# make a plot - on min scale
help(par)

plot(1:3,
     1:3,
     xlab = "",
     ylab = "Bias (95% CI) in minutes",
     xlim = c(.5,3.5),
	   ylim = range(summary_table[,c(2,4,5)]),
	   type = "n",
     axes = F,
     font.lab = 2)

title(xlab = "Posture",
      font.lab = 2,
      line = 2.5)
title(sub = "* Mean posture min from AP for sitting = 40.7', standing = 35.1', and movement = 22.5' *", 
      font.sub = 1,
      adj = 0.5)

axis(2)
axis(1,
     at = 1:3,
     labels = summary_table$posture)
abline(h = 0,
       lty = 3)
points(1:3,
       summary_table$bias,
       pch = 16,
       cex = 1.5)

for (i in 1:3) {
  
  lines(c(i,i),
        c(summary_table$lower_95_bias[i],summary_table$upper_95_bias[i]),
        lty = 2)
  text(i,
       summary_table$bias[i],
       paste(summary_table$bias[i],"min"),
       pos = 4,
       cex = 1.25)
  
}


# make a plot - on % scale
plot(1:3,
     1:3,
     xlab = "",
     ylab = "Bias (95% CI) as %",
     xlim = c(.5,3.5),
     ylim = range(perc_sum_table[, c(2,4,5)]),
     type = "n",
     axes = F,
     font.lab = 2)

title(xlab = "Posture",
      font.lab = 2,
      line = 2.5)
title(sub = "* Mean posture min from AP for sitting = 40.7', standing = 35.1', and movement = 22.5' *", 
      font.sub = 1,
      adj = 0.5)

axis(2)
axis(1,
     at = 1:3,
     labels = perc_sum_table$posture)
abline(h = 0,
       lty = 3)
points(1:3,
       perc_sum_table$bias,
       pch = 16,
       cex = 1.5)

for (i in 1:3) {
  
  lines(c(i,i),
        c(perc_sum_table$lower_95_bias[i], perc_sum_table$upper_95_bias[i]),
        lty = 2)
  text(i,
       perc_sum_table$bias[i],
       paste0(perc_sum_table$bias[i],"%"),
       pos = 4,
       cex = 1.25)
  
}


