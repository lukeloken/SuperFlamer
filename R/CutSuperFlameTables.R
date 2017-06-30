


# ##############################################################
# Cut SuperFLAMe data based on Flame on/off times
# Input is 1) dataframe of merged files
# 2) direcotry where meta table is saved
# Output is a cut datatable 
# ##############################################################

library(data.table)

CutSuperFlame<-function(alldata, dir){
  
  #Find meta table
  rawdir<-paste(dir, 'RawData', sep="/")
  metafile<-list.files(rawdir)[grep('FlameMeta', list.files(rawdir))]
  
  if (length(metafile) != 1) {
    stop("'dir/RawData' does not contain one FlameMeta file (e.g., 'FlameMetaDate.csv')")}
  
  #read meta table and omit rows that are empty
  meta<-fread(paste(rawdir, metafile, sep="/"), sep=",", skip=0, header=T)
  meta<-subset(meta, is.na(as.POSIXct(Flame_on, format="%H:%M:%S"))==FALSE)
  
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
  }
  
  #cut data based on flame on/off intervals
  i=2
  cutdata<-data.frame() 
  for (i in seq(2,length(orderedtimes), by=2)){
    interval=as.POSIXct(orderedtimes[(i-1):i], tz=as.character(meta$GPS_Timezone[1]))
    cutdata<-rbind(cutdata, subset(alldata,alldata$Date_Time>=interval[1] & alldata$Date_Time<=interval[2]))
  }
  
  return(cutdata)
  
}