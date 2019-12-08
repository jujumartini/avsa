
# img_sbs - Test 1 --------------------------------------------------------

master_timestamps <- "//ufiles.ad.uwm.edu/uwm/pahrl/FLAC/OxfordImageBrowser-win32-x64/Downloaded Annotation Files/MasterTimeStamp/TimeStamps.csv"

test = "FLAC_1002V3_POSTURE_CHANG.CSV"

df1 <- read.csv(file = master_timestamps)

df2 <- read.table(file = paste0("./3_data/raw/annotation/", test),
                  header = T,
                  sep = ",")
Filename = test
df2$ID <- as.integer(substr(test, 6, 9))
df2$Visit = as.integer(substr(test, 11, 11))

###Change Column Type with Lubridate###
df2$startTime <- ymd_hms(df2$startTime, tz="UTC")
df2$endTime <- ymd_hms(df2$endTime, tz="UTC")
attr(df2$startTime, "tzone")
df1$StopWatch_YMD_HMS <-
  ymd_hms(df1$StopWatch_YMD_HMS, tz="America/Chicago")
attr(df1$StopWatch_YMD_HMS, "tzone")
df1$Corr_Picture_YMD_HMS <-
  ymd_hms(df1$Corr_Picture_YMD_HMS, tz="America/Chicago")

###Create Time Difference column and Calculate###
df1$Difference <- NA
df1$Difference <-
  with(df1,
       difftime(StopWatch_YMD_HMS, Corr_Picture_YMD_HMS, units = "secs"))

###Merge Data Frames###
df3 <- merge(df2, df1, by = c("ID", "Visit"))

###Add Difference to Start Time Column###
df3$NEWstarttime <- NA
df3 <- df3 %>%
  mutate(NEWstarttime = if_else(!is.na(Difference),
                                startTime + Difference,
                                startTime))

###Add Difference to End Time Column###
df3$NEWendtime <- NA
df3 <- df3 %>%
  mutate(NEWendtime = if_else(!is.na(Difference),
                              endTime + Difference,
                              endTime))

###Making sure New start/stop are in correct POSIxt format###
attr(df3$NEWstarttime, "tzone")
df3$NEWstarttime <- with_tz(df3$NEWstarttime, tz = "America/Chicago")
df3$NEWendtime <- with_tz(df3$NEWendtime, tz = "America/Chicago")
df3$NEWstarttime <- strptime(df3$NEWstarttime,format="%Y-%m-%d %H:%M:%OS")
df3$NEWendtime <- strptime(df3$NEWendtime,format="%Y-%m-%d %H:%M:%OS")

###Forcing correct timezone on stopwatch to then compare to NEWStartTime###
df3$StopWatch_YMD_HMS <- 
  force_tz(df3$StopWatch_YMD_HMS, tz = "America/Chicago")
attr(df3$StopWatch_YMD_HMS, "tzone")
df3$StopWatch_YMD_HMS <- with_tz(df3$StopWatch_YMD_HMS, tz = "GMT")
df3$StopWatch_YMD_HMS <- with_tz(df3$StopWatch_YMD_HMS, tz = "America/Chicago")

###total wear time###
df3$weartime <- NA
df3$weartime <- abs(difftime(df3$NEWstarttime[1], df3$NEWendtime[nrow(df3)], units = "secs"))

###write a "check" csv file to see if stopwatch matches NEW start time###
write.csv(df3, file = paste0("./data/image_check/", Filename))

###second by second###
n <- nrow(df3)
l <- lapply(1:n, function(i) {
  new <- seq.POSIXt(df3$NEWstarttime[i], df3$NEWendtime[i], by = "sec")
  annotation <- rep(df3$annotation[i], length(new))
  data.frame(time = new, annotation = annotation)
})

df4 <- Reduce(rbind, l)

df5 <- df4 %>% pad

###changing NA's to transition;gap###
levels <- levels(df5$annotation)
levels[length(levels) + 1] <- "transition;gap"
df5$annotation <- factor(df5$annotation, levels = levels)
df5$annotation[is.na(df5$annotation)] <- "transition;gap"

###add in ID and Visit###
df5$ID <- as.integer(substr(filelist[CSV], 6, 9))
df5$Visit = as.integer(substr(filelist[CSV], 11, 11))

###reorder columns###
df6 <-
  df5[,c(
    "ID",
    "Visit",
    "time",
    "annotation"
  )]

###Save the new Dataframe###
write.csv(df6, file = paste0("./data/image/", Filename))


###testing on off function###
test = "FLAC_1009V1_POSTURE_CHANG.CSV"
{
  
  on.off.log <- read.csv("./data/master_ACC_diary.csv")
  on.off.log$id <- as.character(on.off.log$id)
  ID = as.integer(substr(test, 6, 9))
  Visit = as.character(substr(test, 10, 11))
  on.off.log <- filter(on.off.log, id == ID)
  on.off.log <- filter(on.off.log, visit == Visit)
  
  on.off.log$date.on <- paste(on.off.log$date.on.month,on.off.log$date.on.day,on.off.log$date.on.year,sep="/")
  on.off.log$time.on <- paste(on.off.log$time.on.hour,on.off.log$time.on.minute,on.off.log$time.on.seconds,sep=":")
  
  on.off.log$date.off <- paste(on.off.log$date.off.month,on.off.log$date.off.day,on.off.log$date.off.year,sep="/")
  on.off.log$time.off <- paste(on.off.log$time.off.hour,on.off.log$time.off.minute,on.off.log$time.off.seconds,sep=":")
  
  on.off.log$date.time.on <- paste(on.off.log$date.on, on.off.log$time.on, sep=" ")
  on.off.log$date.time.off <- paste(on.off.log$date.off, on.off.log$time.off, sep=" ")
  
  on.off.log$date.time.on <- strptime(on.off.log$date.time.on,"%m/%d/%Y %H:%M:%S")
  on.off.log$date.time.off <- strptime(on.off.log$date.time.off,"%m/%d/%Y %H:%M:%S")
  
  on.off.log$date.time.on <- 
    force_tz(on.off.log$date.time.on, tz = "America/Chicago")
  
  on.off.log$date.time.off <- 
    force_tz(on.off.log$date.time.off, tz = "America/Chicago")
  
  on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%d %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
  
  df = read.csv(paste0("./data/image", "/",
                       test),
                header = T,
                sep = ",",
                stringsAsFactors = T)
  df$time <- strptime(df$time,"%Y-%m-%d %H:%M:%S")
  
  #	if on/off times recorded - loop through and label time monitor is not worn
  if(dim(on.off.log)[1]>0)
  {
    df$off <- 1	
    for (t in (1:dim(on.off.log)[1]))
    {
      on <- strptime(on.off.log$date.time.on[t],"%Y-%m-%d %H:%M:%S")
      class(on)
      off <- strptime(on.off.log$date.time.off[t],"%Y-%m-%d %H:%M:%S")
      n <- dim(df)[1]
      class(df$time)
      inds <- (1:n)[((df$time>=on)&(df$time<=off))]
      if (length(inds)>0)
        df$off[inds] <- 0
    }
    if(dim(on.off.log)[1]==0)
      df$off <- "No.On.Off.Log"	
  }	#end on/off loop
  
  #Clean#
  df$time <- as.POSIXct(df$time, tz = "America/Chicago")
  df <- filter(df, off == 0)
  df <- df[ , 2:5]
  df$annotation <- 
    as.character(df$annotation) #change to character for next step
  df$annotation[df$annotation == "posture;0006 sitting"] <- "0" 
  df$annotation[df$annotation == "posture;0007 standing"] <- "1" 
  df$annotation[df$annotation == "posture;0008 movement"] <- "2"
  df$annotation[df$annotation!="0" & df$annotation!="1" & df$annotation!="2"] <- "3"
  
  Filename2 = filelist2[CSV]
  write.csv(df, file = paste0("./data/image/", Filename2))
}

### testing ap function ###
testap= "raw/analysis/1002V3-AP740063 16Feb18 9-35am for 4h 24m-AOSD-CL08090134-Events.csv"
{
  
  directory <- "R:/PAHRL/Student Access/0_Students/MARTINEZ/AvsA Paper/R Data/raw/analysis_correction/"
  directory <- "R:/PAHRL/Student Access/0_Students/MARTINEZ/1_Publication Work/AvsA/Data/"
  ###Change from Julian time to GMT###
  
  data <- read.csv(paste0(directory, testap), stringsAsFactors=FALSE)
  data <- data[,(1:6)]
  names(data) <- c("time","datacount","interval","activity","cumulativesteps","methrs")
  
  data$time <- sub("#","",data$time)
  data$time <- sub("#","",data$time)
  data[,2] <- as.numeric(as.character(data[,2]))
  data[,3] <- as.numeric(as.character(data[,3]))
  data[,4] <- as.numeric(as.character(data[,4]))
  data[,5] <- as.numeric(as.character(data[,5]))*2 #event files have half the actual number of steps for some reason
  data[,6] <- as.numeric(as.character(data[,6]))
  
  t <- dim(data)[1]
  
  data <- data[!(data[,"time"] == "1899-12-30"),]
  data <- data[!(data[,"time"] == "0"),]
  n <- dim(data)[1]		
  
  if(is.character(data$time)==TRUE&t==n)
  {
    data$time <- as.numeric(data$time)
    data$time <- as.POSIXct(as.Date(data$time,origin="1899-12-30"))
    data$time <- as.POSIXlt(data$time,tz="UTC")
    attr(data$time, "tzone")
    data$time <- force_tz(data$time, tz = "America/Chicago")
    data$time <- strptime(data$time,format="%Y-%m-%d %H:%M:%S")
    data$time <- data$time + 3106.8918*24*60*60 ### CORRECTION FACTOR ###
  }
  
  ### second by second ###
  
  sec.by.sec.data <- data.frame(time=NA, date=NA, ap.posture=NA, mets=NA, met.hours=NA, steps=NA)
  sec.by.sec.data <- sec.by.sec.data[-1,]
  
  data$interval <- as.numeric(data$interval)
  
  data$methrs <- as.numeric(data$methrs)
  
  n <- dim(data)[1]
  time.of.each.event <- as.vector(difftime(strptime(data$time[seq_len(n - 1) + 1],format="%Y-%m-%d %H:%M:%S"),strptime(data$time[seq_len(n - 1)],format="%Y-%m-%d %H:%M:%S"), units="secs"))
  start.time <- strptime(data$time[1],format="%Y-%m-%d %H:%M:%S")
  
  time.of.each.event <- c(time.of.each.event, round(data[n,"interval"],0))
  te <- length(time.of.each.event)
  time.of.each.event[is.na(time.of.each.event)==T] <- 1
  events <- rep((1:te),time.of.each.event)
  
  acts <- rep(data$activity,time.of.each.event)
  n <- length(acts)
  # The met hours per second in the interval.
  met.hours <- data$methrs/data$interval 	
  met.hours <- rep(met.hours,time.of.each.event)
  # To compute mets per second in the interval, multiply methours by 3600 sec/hour and divide by number of seconds.
  mets <- data$methrs * 3600/data$interval
  mets <- rep(mets,time.of.each.event)
  steps <- rep(data$cumulativesteps,time.of.each.event)
  # Make 15-sec epoch variable and METs
  times <- start.time+(0:(n-1))
  fifteen.sec.times <- start.time + (15*rep(0:(floor(n/15)),each=15,length=n))
  fifteen.sec.mets <- tapply(mets, fifteen.sec.times, mean)
  fifteen.sec.mets <- rep(fifteen.sec.mets, each=15, length=n)
  
  # Make 1-min epoch variable and METs
  times <- start.time+(0:(n-1))
  one.min.times <- start.time + (60*rep(0:(floor(n/60)),each=60,length=n))
  one.min.mets <- tapply(mets, one.min.times, mean)
  one.min.mets <- rep(one.min.mets, each=60, length=n)
  
  date <- substring(format(times),1,10)
  
  sec.by.sec.data <- merge(sec.by.sec.data, data.frame(time=times, date=date, ap.posture=acts, mets=mets, fifteen.sec.mets=fifteen.sec.mets, one.min.mets=one.min.mets, met.hours=met.hours, steps=steps, num.events=events, stringsAsFactors=FALSE), all=TRUE)
  
  sec.by.sec.data$mets <- signif(sec.by.sec.data$mets,3)
  
  
  ### on/off ###
  
  on.off.log <- read.csv("./data/master_ACC_diary.csv")
  on.off.log$id <- as.character(on.off.log$id)
  ID = as.integer(substr(testap, 6, 9))
  Visit = as.character(substr(testap, 11, 12))
  on.off.log <- filter(on.off.log, id == ID)
  on.off.log <- filter(on.off.log, visit == Visit)
  
  on.off.log$date.on <- paste(on.off.log$date.on.month,on.off.log$date.on.day,on.off.log$date.on.year,sep="/")
  on.off.log$time.on <- paste(on.off.log$time.on.hour,on.off.log$time.on.minute,on.off.log$time.on.seconds,sep=":")
  
  on.off.log$date.off <- paste(on.off.log$date.off.month,on.off.log$date.off.day,on.off.log$date.off.year,sep="/")
  on.off.log$time.off <- paste(on.off.log$time.off.hour,on.off.log$time.off.minute,on.off.log$time.off.seconds,sep=":")
  
  on.off.log$date.time.on <- paste(on.off.log$date.on, on.off.log$time.on, sep=" ")
  on.off.log$date.time.off <- paste(on.off.log$date.off, on.off.log$time.off, sep=" ")
  
  on.off.log$date.time.on <- strptime(on.off.log$date.time.on,"%m/%d/%Y %H:%M:%S")
  on.off.log$date.time.off <- strptime(on.off.log$date.time.off,"%m/%d/%Y %H:%M:%S")
  
  on.off.log$date.time.on <- 
    force_tz(on.off.log$date.time.on, tz = "America/Chicago")
  
  
  on.off.log$date.time.off <- 
    force_tz(on.off.log$date.time.off, tz = "America/Chicago")
  
  
  on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%d %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%d %H:%M:%S"), units="hours"))
  
  sec.by.sec.data$time <- strptime(sec.by.sec.data$time,"%Y-%m-%d %H:%M:%S")
  
  #	if on/off times recorded - loop through and label time monitor is not worn
  if(dim(on.off.log)[1]>0)
  {
    sec.by.sec.data$off <- 1	
    for (t in (1:dim(on.off.log)[1]))
    {
      on <- strptime(on.off.log$date.time.on[t],"%Y-%m-%d %H:%M:%S")
      class(on)
      off <- strptime(on.off.log$date.time.off[t],"%Y-%m-%d %H:%M:%S")
      n <- dim(sec.by.sec.data)[1]
      class(sec.by.sec.data$time)
      inds <- (1:n)[((sec.by.sec.data$time>=on)&(sec.by.sec.data$time<=off))]
      if (length(inds)>0)
        sec.by.sec.data$off[inds] <- 0
    }
    if(dim(on.off.log)[1]==0)
      sec.by.sec.data$off <- "No.On.Off.Log"	
  }	#end on/off loop
  
  
  ### Clean ###
  sec.by.sec.data$time <- 
    as.POSIXct(sec.by.sec.data$time, 
               tz = "America/Chicago") #change time to POSIXct class instead of POSIXlt
  sec.by.sec.data <- 
    sec.by.sec.data[!(sec.by.sec.data$off==1), ] #remove non-visit time#
  sec.by.sec.data$ID <- 
    as.integer(substr(testap, 6, 9)) #add in ID
  sec.by.sec.data$Visit <-
    as.integer(substr(testap, 12, 12)) #add in visit
  sec.by.sec.data <- 
    sec.by.sec.data[ , c(11,12,1,3)] #only need ap.posture column
  sec.by.sec.data$ap.posture <- 
    as.character(sec.by.sec.data$ap.posture) #change to character for next step
  sec.by.sec.data$ap.posture[sec.by.sec.data$ap.posture == "0"] <- "posture;0006 sitting" 
  sec.by.sec.data$ap.posture[sec.by.sec.data$ap.posture == "1"] <- "posture;0007 standing" 
  sec.by.sec.data$ap.posture[sec.by.sec.data$ap.posture == "2"] <- "posture;0008 movement"
  Filename3 = aplist2[event.file]
  write.csv(sec.by.sec.data, file = paste0("./data/ap/", Filename3))
  
}

### testing merge function ###
test = "FLAC_1042V2_POSTURE_CHANG.CSV"
{
  image.frame <- read.csv(paste0("./data/image", 
                                 "/",
                                 test),
                          header = T,
                          sep = ",",
                          stringsAsFactors = F)
  image.frame <- image.frame[ ,-c(1)]  
  ID = as.character(substr(test, 6, 9))
  Visit = as.character(substr(test, 10, 11))
  
  ap.frame <- read.csv(paste0("./data/ap/", 
                              "FLAC_",
                              ID,
                              "_",
                              Visit,
                              ".csv"),
                       header = T,
                       sep = ",",
                       stringsAsFactors = F)
  ap.frame <- ap.frame[ ,-c(1)]  
  
  merged.frame <- full_join(image.frame, ap.frame)
  merged.frame <- merged.frame[ ,-c(2,3)]
  merged.frame <- merged.frame[complete.cases(merged.frame), ]
  merged.frame$ID <- paste(c(as.character(substr(test, 6, 9)), as.character(substr(test, 10, 11))), collapse = "")
  Filename4 = aplist3
  write.csv(merged.frame, file = paste0("./data/merged/", Filename4))
}







# testing count function 


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

# testing event.kappa #

counter <- 1
temp_events <- read.csv(paste0("./", event.list.1sec[12]))
id = as.character(temp_events$ID[1])
temp_events <- temp_events[ ,c(3,4)]
table(temp_events$annotation, temp_events$ap.posture)
kappa2(temp_events)
temp_kappa  <- kappa2(temp_events, weight = "equal")
temp_kappa  <- do.call(rbind, temp_kappa)
p.val       <- temp_kappa[8, ]
temp_kappa  <- temp_kappa[5, ]

if (counter==1)
  kappa.table.1sec <- data.frame(ID = c(id, 2:15),
                                 Visit = c(visit, 2:15),
                                 kappa_1sec = c(temp_kappa, 2:15),
                                 pvalue_1sec = c(p.val, 2:15)
  )
dat
if (counter>1)
  kappa.table.1sec$ID <- append(kappa.table.1sec$ID, id)
kappa.table.1sec$Visit <- append(kappa.table.1sec$Visit, visit)
kappa.table.1sec$kappa_1se <- append(kappa.table.1sec$kappa_1sec, temp_kappa)
kappa.table.1sec$pvalue_1sec <- append(kappa.table.1sec$pvalue_1sec, p.val)

counter <- counter+1


