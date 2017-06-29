
# ##############################################################
# Read SuperFLAMe data, construct a single file
# Input is folder directory
# Output is a merged datatable
# ##############################################################

dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'
dir2<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota2'

library(data.table)

ReadSuperFlame<-function(dir){
  files<-list.files(dir)
  if (length(files) == 0) {
    stop("No files in directory")}
  
  #Merge data files that contain these strings in the file name
  patterns<-'GPS|BoxMetrics|EXO|GGA|Nitrate'
  loadfiles<-files[grep(patterns, files)]
  
  #Read files and omit RECORD column
  import.list <- lapply(paste(dir, loadfiles, sep="/"), fread, sep=",", skip=1)
  import.list2 <- lapply(import.list, function(x)x[,-c('RECORD')])

  #Merge datatables using TIMESTAMP
  my.df <- Reduce(function(x, y) merge(x, y, all=FALSE,by=c("TIMESTAMP"),all.x=TRUE, all.y=TRUE),import.list2,accumulate=F) 
  
  head(my.df)
  head(import.list[1])
  head(import.list[2])
}

#Test run

ReadSuperFlame(dir)

ReadSuperFlame(dir2)

# Old Code below from original Flame scripts

read_instruments=function(meta){
require(stringr)
LaserPath <- list.files(path = paste(getwd(), "/laser", sep=""))
#Bind all files in above path using read.table - Need to specify skip, header, etc. 
if (length(LaserPath)>0){
LaserFull<-do.call("rbind", lapply(paste("laser/", LaserPath, sep=""), read.table, sep="," ,skip=1,  header = TRUE, fill=TRUE)) 
LaserFull$Time_raw=as.POSIXct(LaserFull$Time, tz=as.character(meta$LGR_Timezone[1]), format="%m/%d/%y %H:%M:%S")
LaserFull=LaserFull[!duplicated(LaserFull[,"Time_raw"]),]
#This day the LGR was an hour off of UTC
LaserFull$Time<-(LaserFull$Time_raw+meta$LGR_Time_Offset[1])
if (meta$LGR_Unit[1]=="Ankur"){
LaserFull$X.CO2_DRY._ppm<-LaserFull$X.CO2._ppm
LaserFull$X.CH4_DRY._ppm<-LaserFull$X.CH4._ppm
}
laservars <- c("Time", "X.CO2_DRY._ppm", "X.CH4_DRY._ppm")
LaserFull <- LaserFull[laservars]
}
####################################################################################
#read in all of the SUNA converted raw data files using 'suna_datetime' script
NitroPath <- list.files(path = paste(getwd(), "/nitro", sep=""))
#Bind all files in above path using read.table - Need to specify skip, header, etc. 
if (length(NitroPath)>0){
NitroFull<-do.call("rbind", lapply(paste("nitro/", NitroPath, sep=""), read.table, sep=",", header = TRUE, fill=TRUE)) 
NitroFull<-suna_datetime(NitroFull)
nitrovars <- c("Date_Time", "NITRATE_UM",  "NITRATE_MG",	"ABS_254",	"ABS_350", "T_INT",  "T_SPEC",	"T_LAMP")
NitroFull <- NitroFull[nitrovars]
# NitroFull<-subset(NitroFull, NITRATE_UM>0)
}
####################################################################################
#read in all of the YSI instrument data files
YsiPath=list.files(path = paste(getwd(), "/ysi", sep=""))
if (length(YsiPath)>0){
YsiFull<-do.call("rbind", lapply(paste("ysi/",YsiPath,  sep=""), read.table, sep="," ,skip=25,  header = TRUE)) 
YsiFull$Date_Time <- as.POSIXct(paste(YsiFull$Date..MM.DD.YYYY., YsiFull$Time..HH.MM.SS.), format="%m/%d/%Y %H:%M:%S", tz=as.character(meta$YSI_Timezone[1]))
YsiFull$Date_Time<-(YsiFull$Date_Time+meta$YSI_Time_Offset[1])

# For variables that contain mu's (SPC, chla, BGA) convert them to u's
SPC_coln<-grep("SpCond.", colnames(YsiFull), ignore.case=T)
colnames(YsiFull)[SPC_coln]<-"SpCond.uS.cm"

ChlA_coln1<-grep("Chlorophyll.", colnames(YsiFull), ignore.case=T)
ChlA_coln2<-grep("g.L", colnames(YsiFull), ignore.case=T)
colnames(YsiFull)[intersect(ChlA_coln2, ChlA_coln1)]<-"Chlorophyll.ug.L"

BGA_coln1<-grep("BGA.PC.", colnames(YsiFull), ignore.case=T)
BGA_coln2<-grep("g.L", colnames(YsiFull), ignore.case=T)
colnames(YsiFull)[intersect(BGA_coln1, BGA_coln2)]<-"BGA.PC.ug.L"

# Only keep these variables
ysivars <- c("Date_Time", "Temp..C", "SpCond.uS.cm", "Chlorophyll.RFU", "Chlorophyll.ug.L", "BGA.PC.RFU", "BGA.PC.ug.L", "Turbidity.FNU", "fDOM.RFU", "fDOM.QSU", "ODO...sat", "ODO.mg.L", "pH", "Press.psi.a")
YsiFull <- YsiFull[ysivars]
}
######################################################################################
#read in all of the GPS files
GpsPath=list.files(path = paste(getwd(), "/gps", sep=""))

if (meta$GPS_Unit[1]=="RiverEco1" | meta$GPS_Unit[1]=="RiverEco2"){
GpsAll<-do.call("rbind", lapply(paste("gps/", GpsPath, sep=""), read.table, sep="," , header = TRUE))
GpsAll$ltime=as.POSIXct(GpsAll$ltime, tz=as.character(meta$GPS_Timezone[1]))
GpsAll$ltime<-(GpsAll$ltime+meta$GPS_Time_Offset[1])
GpsAll$ltime=as.POSIXct(GpsAll$ltime, tz=as.character(meta$GPS_Timezone[1]))
gpsvars <- c("ltime", "Latitude", "Longitude" )
GpsAll <- GpsAll[gpsvars]
}

if (meta$GPS_Unit[1]=="LTERGarmin" | meta$GPS_Unit[1]=="USGS"){
  
  GpsAll<-do.call("rbind", lapply(paste("gps/", GpsPath, sep=""), read.table, sep="," , header = TRUE, skip=42))
  date<-str_sub(GpsAll$time,1,10)
  time<-str_sub(GpsAll$time,-9,-2)
  GpsAll$ltime<-as.POSIXct(paste(date,time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz=as.character(meta$GPS_Timezone[1]))
  GpsAll$Latitude<-GpsAll$lat
  GpsAll$Longitude<-GpsAll$lon
  gpsvars <- c("ltime", "Latitude", "Longitude","Depth" )
  GpsAll <- GpsAll[gpsvars]
}  

GpsAll$ltime<-(GpsAll$ltime+meta$GPS_Time_Offset[1])

######################################################################################
#read in all of the Picarro files
#Load Picarro directory and list all files

PicarroPath<-list.files(path = paste(getwd(), "/picarro", sep=""))
if (length(PicarroPath)>0){
  PicarroFull<-do.call("rbind", lapply(paste("picarro/", PicarroPath, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE)) 
  
  #Set Picarro Time and remove duplicates
  PicarroFull$DateTime_UTC<-as.POSIXct(paste(PicarroFull$DATE, floor((PicarroFull$TIME*24)), PicarroFull$min, PicarroFull$sec, sep=" "), format="%Y-%m-%d %H %M %S", tz="UTC")
  PicarroFull<-PicarroFull[order(PicarroFull$DateTime_UTC),]
  
  PicarroFull=PicarroFull[!duplicated(PicarroFull[,"DateTime_UTC"]),]
  
  PicarroFull$DateTime_UTC<-(PicarroFull$DateTime_UTC+Picarro_Time_Offset[1])
  
  #Select columns
  Picarrovars <- c(68, 29, 30, 35:49, 58:67)
  PicarroFull <- PicarroFull[,Picarrovars]
  
}

#merge all of the instrument datasets together
#################################################################################
if (length(LaserPath)>0){
  merge.data1=merge(GpsAll, LaserFull,   by.y="Time", by.x=c("ltime"),all=TRUE)
}
if (length(LaserPath)==0) {
  merge.data1=GpsAll
}

if (length(YsiPath)>0){
  merge.data2=merge(merge.data1, YsiFull, by.y="Date_Time", by.x="ltime",all=TRUE)
}
if (length(YsiPath)==0) {
  merge.data2=merge.data1
}

if (length(NitroPath)>0){
merge.data3=merge(merge.data2, NitroFull, by.y="Date_Time", by.x="ltime",all=TRUE)
}
if (length(NitroPath)==0) {
  merge.data3=merge.data2
}

if (length(PicarroPath)>0){
  merge.data4<-merge(merge.data3, PicarroFull, by.y="DateTime_UTC", by.x="ltime",all=TRUE)
}
if (length(PicarroPath)==0) {
  merge.data4<-merge.data3
}


colnames(merge.data4)<-str_replace_all(colnames(merge.data4), "[.]", "")
colnames(merge.data4)<-str_replace_all(colnames(merge.data4), "[_]", "")
colnames(merge.data4)<-str_replace_all(colnames(merge.data4), "[?]", "u")
colnames(merge.data4)<-str_replace_all(colnames(merge.data4), "Chlorophyll", "ChlA")
colnames(merge.data4)<-str_replace_all(colnames(merge.data4), "Turbidity", "Turb")
colnames(merge.data4)<-str_replace_all(colnames(merge.data4), "DRY", "D")
colnames(merge.data4)<-str_replace_all(colnames(merge.data4), "SpCond", "SPC")


##write out to a single .csv file###############################################
return(merge.data4)
}
########################END###########################################################