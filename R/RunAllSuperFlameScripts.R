


# Set data directory
# 'RawData' folder needs to be within this folder and must contain:
# 1) Data files (.dat) from superflame computer (e.g., Site_Date_GPS.dat)
# 2) meta tables (.csv) from access (e.g., FlameMetaDate.csv)
dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'

# Load functions
source('R/MergeSuperFlameTables.R')
source('R/CutSuperFlameTables.R')
source('R/TrimSuperFlameTables.R')
source('R/ConvertLatLongToDecimalDegree.R')

# ###############
# Find meta table
# ###############

rawdir<-paste(dir, 'RawData', sep="/")
metafile<-list.files(rawdir)[grep('FlameMeta', list.files(rawdir))]

if (length(metafile) != 1) {
  stop("'dir/RawData' does not contain one FlameMeta file (e.g., 'FlameMetaDate.csv')")}

#read meta table and omit rows that are empty
meta<-fread(paste(rawdir, metafile, sep="/"), sep=",", skip=0, header=T)
meta<-subset(meta, is.na(as.POSIXct(Flame_on, format="%H:%M:%S"))==FALSE)

if (nrow(meta) ==0) {
  stop("Metatable has zero flame on/off times")}

# ##############################
# Merge, cut, and trim datafiles
# ##############################

#Get and merge all raw data
alldata<-ReadSuperFlame(dir)

#Cut data based on flame on/off times
cutdata<-CutSuperFlame(alldata, meta)

#Trim data to reduce number of columns
trimdata<-TrimSuperFlame(cutdata)



# ###########
# Output Data
# ###########

# Create subfolders to put ProcessedData and Maps
folders<-list.files(dir)
if(length(folders[folders=="ProcessedData"])==0){
  dir.create(paste(dir, "/ProcessedData", sep=""))}
if(length(folders[folders=="Maps"])==0){
  dir.create(paste(dir, "/Maps", sep=""))}

# Determine date and site and confirm they match between meta, directory, and datafiles

# direcotry names
strings<-unlist(strsplit(dir, '_|/'))

# Date
DataDate<-as.Date(cutdata$date_time[1])
Date<-as.Date(meta$Date[1])
DirDate <- try( as.Date( strings[length(strings)-1], format= "%Y-%m-%d") )
if( class(DirDate) == "try-error" || is.na(DirDate)){
  stop("Directory not in standard format (~/Data/YYYY-MM-DD_SiteName)")}

if (identical(Date, DataDate, DirDate)==FALSE){
  warning("Directory, Meta, and/or Data dates do not match")}

# Site
DirSite<-tail(strings, n=1)
Site<-meta$Site[1]

if (identical(Site, DirSite)==FALSE){
  warning("Directory and Meta SiteNames do not match")}

# Save tables
write.table(alldata, file = as.character(paste(dir, '/ProcessedData/', Date, "_", Site, "_01_Merged.csv", sep="")), col.names=TRUE,row.names=F, sep=",")
write.table(cutdata, file = as.character(paste(dir, '/ProcessedData/', Date, "_", Site, "_02_Cut.csv", sep="")), col.names=TRUE,row.names=F, sep=",")
write.table(trimdata, file = as.character(paste(dir, '/ProcessedData/', Date, "_", Site, "_03_Trimmed.csv", sep="")), col.names=TRUE,row.names=F, sep=",")


# Make a spatial points dataframe and save as a shapefile
geodata<-MakeSuperFlameSpatialPoints(trimdata)
writeOGR(geodata, dsn=paste(dir, "/ProcessedData", sep="") ,layer=as.character(paste(Date, "_", Site, "_04_Shapefile", sep="")), driver="ESRI Shapefile",  verbose=F, overwrite=T)

# Visualize Data
PlotSuperFlame(geodata, dir)

# Print Warning Messages
warnings()
