source("./2_scripts/1_functions.R")


# create analysis tables --------------------------------------------------

list_merged <- list.files("./3_data/analysis/merged_anno_ap/", "csv")

analysis_avsa(merged_list = list_merged)


# old stuff ---------------------------------------------------------------



# visit time #
average.visit.time <- mean(results$Visit.Time)
average.visit.time/60 #a

# total and event percent agreement #
average.totpa.1sec <- mean(results$Total.Percent.Agreement.1sec) #f
average.totpa.60sec <- mean(results$Total.Percent.Agreement.60sec) #g
average.evepa.1sec <- mean(results$Events.Percent.Agreement.1sec) #h
average.evepa.60sec <- mean(results$Events.Percent.Agreement.60sec) #i

# transition time and event time #
average.trans.1sec <- mean(results$Transition.Time.1sec) #b
average.trans.1sec/60
average.trans.60sec <- mean(results$Transition.Time.60sec) #c

sum(results$Event.Time.1sec)
average.event.1sec <- mean(results$Event.Time.1sec) #d
average.event.1sec/60
average.event.60sec <- mean(results$Event.Time.60sec) #e

event.list.1sec <- list.files(".", "1sec.csv")
event.list.1sec
event.list.60sec<- list.files(".", "60sec.csv")
event.list.60sec

### For Cho ###

for (i in 1:length(event.list.1sec)) {
  counts.1sec(i)
}  

for (i in 1:length(event.list.60sec)) {
  counts.60sec(i)
}  

### testing count function ###
{
  print(event.list.60sec[17])
  
  events.1sec <- read.csv(paste0("./", event.list.60sec[17]), stringsAsFactors = T)
  filename = as.character(substr(event.list.60sec[17], 6, 12))
  events.1sec$annotation <- as.factor(events.1sec$annotation)
  events.1sec$ap.posture <- as.factor((events.1sec$ap.posture))
  anno.levels <- levels(events.1sec$annotation)
  l <- length(anno.levels)
  ap.levels <- levels(events.1sec$ap.posture)
  n <- length(ap.levels)
  
  if (l==1){
    anno.levels[length(anno.levels) + 1] <- "1"
    anno.levels[length(anno.levels) + 1] <- "2"
    events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
  } 
  if (l==2){
    anno.levels[length(anno.levels) + 1] <- "1"
    anno.levels <- sort(anno.levels)
    events.1sec$annotation <- factor(events.1sec$annotation, levels = anno.levels)
  } 
  if (n==1){
    ap.levels[length(ap.levels) + 1] <- "1"
    ap.levels[length(ap.levels) + 1] <- "2"
    events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
  } 
  if (n==2){
    ap.levels[length(ap.levels) + 1] <- "2"
    events.1sec$ap.posture <- factor(events.1sec$ap.posture, levels = ap.levels)
  } 
  
  t <- table(events.1sec$ap.posture, events.1sec$annotation)
  t
  a <- t[1,1]
  b <- t[1,2]
  c <- t[1,3]
  d <- t[2,1] 
  e <- t[2,2]
  f <- t[2,3]
  g <- t[3,1]
  h <- t[3,2]
  i <- t[3,3]
  
  id <- as.character(events.1sec$ID[1])
  count <- c(a,b,c,d,e,f,g,h,i)
  activpal   <- rep(0:2, each=3)
  annotation <- rep(0:2, times=3)
  
  data <- data.frame(ID=id,
                     Activpal = activpal,
                     Annotation = annotation,
                     Count = count)
  
  write.table(data, 
              file = paste0("./counts/", filename, "60sec.csv"),
              sep = ",",
              row.names = F,
              col.names = T)
}

### merged count files ###
counts.list.1sec <- list.files("./counts/", "1sec.csv")
counts.list.60sec <- list.files("./counts/", "60sec.csv")

setwd("R:/PAHRL/Student Access/0_Students/MARTINEZ/AvsA Paper/R Data/data/event/counts/")
counts.1sec <- lapply(counts.list.1sec, read.csv, header=T)
counts.1sec <- do.call(rbind, counts.1sec)
write.table(counts.1sec, 
            file = paste0("./", "countsbyvisit_1sec.csv"),
            sep = ",",
            row.names = F,
            col.names = T)
counts.60sec <- lapply(counts.list.60sec, read.csv, header=T)
counts.60sec <- do.call(rbind, counts.60sec)
write.table(counts.60sec, 
            file = paste0("./", "countsbyvisit_60sec.csv"),
            sep = ",",
            row.names = F,
            col.names = T)

# counts
counts <- read.csv('R:/PAHRL/Student Access/0_Students/MARTINEZ/AvsA Paper/R Data/data/event/counts/countsbyvisit_1sec.csv')
counts <- counts[counts$ID!="1042V2", ]
counts <- counts[counts$ID!="1042V3", ]
counts <- counts[counts$ID!="1048V2", ]
counts <- counts[counts$ID!="1048V3", ]

standing <- counts[counts$Activpal==1, ]
output <- standing[standing$Annotation==2, ]
write.csv(standing, "R:/PAHRL/Student Access/0_Students/MARTINEZ/AvsA Paper/R Data/data/event/counts/standing1sec.csv")
if (standing$ID)
  mean(output$Count)


moving <- counts[counts$Activpal==2, ]
moving <- moving[moving$Activpal!=moving$Annotation, ]




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


