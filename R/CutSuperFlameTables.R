


# ##############################################################
# Cut SuperFLAMe data based on Flame on/off times
# Input is 1) dataframe of merged files
# 2) direcotry where meta table is saved
# Output is a cut datatable 
# ##############################################################

library(data.table)

CutSuperFlame<-function(alldata, meta){
  
  # Convert on and off times to date_time
  Flame_On <- as.POSIXct(paste(meta$Date[1], meta$Flame_on, sep=" "), format="%Y-%m-%d %H:%M:%S", tz=as.character(meta$GPS_Timezone[1]))
  Flame_Off <- as.POSIXct(paste(meta$Date[1], meta$Flame_off, sep=" "), format="%Y-%m-%d %H:%M:%S",  tz=as.character(meta$GPS_Timezone[1]))
  
  #Make a single vector of on/off times
  orderedtimes<-as.POSIXct(c(t(data.frame(Flame_On, Flame_Off))), tz=as.character(meta$GPS_Timezone[1]))
  
  #See if any times are past midnight, if yes add one day
  aftermidnight<-which(c(NA, diff(orderedtimes))<0)
  if (length(aftermidnight)==1){
    orderedtimes[aftermidnight:length(orderedtimes)]<-
      as.POSIXct(c(t(data.frame(Flame_On, Flame_Off)+86400)), 
      tz=as.character(meta$GPS_Timezone[1]))[aftermidnight:length(orderedtimes)]
      
      warning("Data span more than one day")
  }
  
  #cut data based on flame on/off intervals
  i=2
  cutdata<-data.frame() 
  for (i in seq(2,length(orderedtimes), by=2)){
    interval=as.POSIXct(orderedtimes[(i-1):i], tz=as.character(meta$GPS_Timezone[1]))
    attr(interval, "tzone") <- "UTC"
    cutdata<-rbind(cutdata, subset(alldata,alldata$date_time>=interval[1] & alldata$date_time<=interval[2]))
  }
  
  if (nrow(cutdata)==0){
    stop("There are no data within Flame On/Off intervals")}
  
  return(cutdata)
  
}