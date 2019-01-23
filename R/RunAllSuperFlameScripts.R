


# Set data directory
# 'RawData' folder needs to be within this folder and must contain:
# 1) Data files (.dat) from superflame computer (e.g., Site_Date_GPS.dat)
# 2) meta tables (.csv) from access (e.g., FlameMetaDate.csv)
# dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'

RunSuperFlame<-function(dir, ...){
  
  # Load functions
  source('R/MergeSuperFlameTables.R')
  source('R/CutSuperFlameTables.R')
  source('R/TrimSuperFlameTables.R')
  source('R/CalculateKhPlummer.R')
  source('R/CalculateKh.R')
  source('R/ConvertGasesSuperFlame.R')
  source('R/CalculateGasSaturation.R')
  source('R/TauCorrectSuperFlame.R')
  source('R/RollingMAD.R')
  source('R/CleanSuperFlame.R')
  source('R/ConvertLatLongToDecimalDegree.R')
  source('R/MakeSuperFlameSpatialPoints.R')
  source('R/VisualizeSpatialData.R')
  source('R/ExtractSampleData.R')
  
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
  tz<-meta$GPS_Timezone[1]
  FLAME_Unit<-meta$FLAME_Unit[1]
  Elevation<-meta$Elevation[1]
  
  if (nrow(meta) ==0) {
    stop("Metatable has zero flame on/off times")}
  
  if (is.finite(meta$Elevation[1])==FALSE) {
    stop("Metatable missing elevation. Needed for CO2 and CH4 unit conversion. Go back to the access file, enter the elevation, and reexport the meta csv to the appropriate data folder.")}
  
  # ##############################
  # Merge, cut, and trim datafiles
  # ##############################
  
  #Get and merge all raw data
  alldata<-ReadSuperFlame(dir)
  
  #Cut data based on flame on/off times
  cutdata<-CutSuperFlame(alldata, meta)
  
  #Trim data to reduce number of columns
  trimdata<-TrimSuperFlame(cutdata)
  
  #Load tau table and apply corrections
  taufile<-list.files('Data')[grep(FLAME_Unit, list.files('Data'))]
  if (length(taufile) != 1) {
    stop("'Data' does not contain Tau file for FLAME_Unit (e.g., 'Manual_Hydros_Taus_2017.01.csv') - Check FLAME_Unit on metafile")}
  tautable<-fread(paste('Data',taufile, sep="/"), sep=",", skip=0, header=T)
  
  correctdata<-TauCorrectSuperFlame(trimdata, tautable, ...)
  
  # Convert CO2 and CH4 to uM and percent Saturation units 
  convertdata<-ConvertGasesSuperFlame(trimdata, Elevation)
  
  #Clean data using SensorQC
  rulefile<-list.files('Data')[grep('SensorQCRules', list.files('Data'))]
  if (length(rulefile) != 1) {
    stop("'Data' does not contain Tau file for FLAME_Unit (e.g., 'Manual_Hydros_Taus_2017.01.csv') - Check FLAME_Unit on metafile")}
  ruletable<-fread(paste('Data',rulefile, sep="/"), sep=",", skip=0, header=T)
  
  cleandata<-CleanSuperFlame(convertdata, ruletable, ...)
  
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
  write.table(correctdata, file = as.character(paste(dir, '/ProcessedData/', Date, "_", Site, "_04_TauCorrected.csv", sep="")), col.names=TRUE,row.names=F, sep=",")
  write.table(cleandata, file = as.character(paste(dir, '/ProcessedData/', Date, "_", Site, "_05_Cleaned.csv", sep="")), col.names=TRUE,row.names=F, sep=",")
  
  
  # Make a spatial points dataframe and save as a shapefile
  geodata<-MakeSuperFlameSpatialPoints(trimdata)
  writeOGR(geodata, dsn=paste(dir, "/ProcessedData", sep="") ,layer=as.character(paste(Date, "_", Site, "_06_ShapefileRaw", sep="")), driver="ESRI Shapefile",  verbose=F, overwrite=T)
  
  geodataclean<-MakeSuperFlameSpatialPoints(cleandata)
  writeOGR(geodataclean, dsn=paste(dir, "/ProcessedData", sep="") ,layer=as.character(paste(Date, "_", Site, "_07_ShapefileCleaned", sep="")), driver="ESRI Shapefile",  verbose=F, overwrite=T)
  
  
  # Visualize Data
  PlotSuperFlame(geodataclean, dir, Date, Site)
  
  # Extract Sample Data
  samplefile<-list.files(rawdir)[grep('FlameSamples', list.files(rawdir))]
  if (length(samplefile) != 1) {
    warning("'dir/RawData' does not contain one FlameSample file (e.g., 'FlameSamplesDate.csv')")
    }  else {
    sample<-fread(paste(rawdir, samplefile, sep="/"), sep=",", skip=0, header=T)
    sample<-subset(sample, !is.na(as.POSIXct(sample$`Sample Time`, format="%H:%M:%S")))
    sampledata<-ExtractSample(cleandata, sample, dir, Date, tz)
    
    write.table(sampledata, file = as.character(paste(dir, '/ProcessedData/', Date, "_", Site, "_08_Samples.csv", sep="")), col.names=TRUE,row.names=F, sep=",")
  }

  
}
