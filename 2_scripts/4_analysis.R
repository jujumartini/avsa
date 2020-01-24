source("./2_scripts/1_functions.R")


# create analysis tables --------------------------------------------------

list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv")

analysis_avsa(merged_list = list_merged)


# linear mixed effects model - bias and CI --------------------------------



data <- read.csv("R:/PAHRL/Student Access/0_Students/MARTINEZ/AvsA Paper/R Data/posture_mins_table.csv")


# fit models: bias~b0 + b_i + e_ij
sitmodel <- lmer(img_sitting - ap_sitting ~ 1 + (1|ID),data=data)
standmodel <- lmer(img_standing - ap_standing ~ 1 + (1|ID),data=data)
movemodel <- lmer(img_move - ap_move ~ 1 + (1|ID),data=data)


# make a table for the results:

# rows are the postures
summary.table <- data.frame(posture=c("sit","stand","move"))

# biases are estimated from model
summary.table$bias <- c(fixef(sitmodel),fixef(standmodel),fixef(movemodel))

# sd is "unexplained variability" in the biases
summary.table$sd <- c(as.data.frame(VarCorr(sitmodel))[2,5],
							as.data.frame(VarCorr(standmodel))[2,5],
							as.data.frame(VarCorr(movemodel))[2,5])

# approximate 95% CIs
summary.table$upper.95.bias <- summary.table$lower.95.bias <- NA
summary.table[,4:5] <- rbind(confint(sitmodel,3),confint(standmodel,3),confint(movemodel,3))


# raw means:
summary.table$ap.min.mean <- c(mean(data$ap_sitting),mean(data$ap_standing),mean(data$ap_move))
summary.table$ap.min.sd <- c(sd(data$ap_sitting),sd(data$ap_standing),sd(data$ap_move))
summary.table$img.min.mean <- c(mean(data$img_sitting),mean(data$img_standing),mean(data$img_move))
summary.table$img.min.sd <- c(sd(data$img_sitting),sd(data$img_standing),sd(data$img_move))

# % sum table

perc.sum.table <- summary.table

perc.sum.table$bias <- 100*summary.table$bias/summary.table$ap.min.mean
perc.sum.table$sd <- 100*summary.table$sd/summary.table$ap.min.mean
perc.sum.table$lower.95.bias  <- 100*summary.table$lower.95.bias/summary.table$ap.min.mean
perc.sum.table$upper.95.bias <- 100*summary.table$upper.95.bias/summary.table$ap.min.mean


# round to 1 digit, arbitrarily
summary.table[,-1] <- round(summary.table[,-1],1)
help(par)
# make a plot - on min scale
plot(1:3,1:3,
     xlab="",
     ylab="Bias (95% CI) in minutes (IMGs-AP)",
     xlim=c(.5,3.5),
	   ylim=range(summary.table[,c(2,4,5)]),
	   type="n",axes=F,
     font.lab=2)

axis(2)
axis(1,at=1:3,labels=summary.table$posture)
abline(h=0,lty=3)
points(1:3,summary.table$bias,pch=16,cex=1.5)
for (i in 1:3)
{
	lines(c(i,i),c(summary.table$lower.95.bias[i],summary.table$upper.95.bias[i]),lty=2)
	text(i,summary.table$bias[i],paste(summary.table$bias[i],"min"),pos=4,cex=1.25)
}
title(xlab = "Posture", font.lab=2, line = 2.5)
title(sub = "* Mean posture min from AP for sitting=54.8', standing=26.9', and movement=24.0' *", 
      font.sub=1,
      adj=0)


# make a plot - on % scale
plot(1:3,1:3,xlab="Posture",ylab="Bias (95% CI) as %",xlim=c(.5,3.5),
	ylim=range(perc.sum.table[,c(2,4,5)]),
	type="n",axes=F)
axis(2)
axis(1,at=1:3,labels=perc.sum.table$posture)
abline(h=0,lty=3)
points(1:3,perc.sum.table$bias,pch=16,cex=1.5)
for (i in 1:3)
	lines(c(i,i),c(perc.sum.table$lower.95.bias[i],perc.sum.table$upper.95.bias[i]),lty=2)


