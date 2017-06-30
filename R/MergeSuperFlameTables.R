
# ##############################################################
# Read SuperFLAMe data, construct a single file
# Input is folder directory
# Output is a merged datatable of all SuperFlame Data
# ##############################################################

library(data.table)

ReadSuperFlame<-function(dir){
  rawdir<-paste(dir, 'RawData', sep="/")
  files<-list.files(rawdir)
  if (length(files) ==0) {
    stop("No files in 'dir/RawData' subdirectory")}
  
  # Merge data files that contain these strings in the file name
  # Load files in this order
  patterns<-c('GPS', 'BoxMetrics', 'EXO', 'GGA', 'SUNA')
  loadfiles<-c(files[grep(patterns[1], files)], 
                files[grep(patterns[2], files)], 
                files[grep(patterns[3], files)], 
                files[grep(patterns[4], files)], 
                files[grep(patterns[5], files)])
  
  if (length(loadfiles) == 0) {
    stop("'dir/RawData' does not contain correct files (e.g., '..._GPS.dat')")}
  
  # Get header names
  header.list<-lapply(paste(rawdir, loadfiles, sep="/"), fread, sep=",", skip=1, nrows=0)
  # Get data
  import.list <- lapply(paste(rawdir, loadfiles, sep="/"), fread, sep=",", skip=4, header=F, stringsAsFactors=F, na.strings=c('NA', 'NaN', '', 'null', '"NAN"'))
  
  # Put header names on correct data frame
  for (df in 1:length(import.list)){
    names(import.list[[df]])<- names(header.list[[df]])
  }

  #Omit 'Records' column
  import.list <- lapply(import.list, function(x)x[,-c('RECORD')])

  #Merge datatables using TIMESTAMP
  my.df <- Reduce(function(x, y) merge(x, y, all=FALSE,by=c("TIMESTAMP"),all.x=TRUE, all.y=TRUE),import.list,accumulate=F) 
  
  if (nrow(my.df)==0){
    stop("Merged data table has zero rows")}
  
  #Set Date_Time as time (need to check timezone)
  my.df$date_time<-as.POSIXct(my.df$TIMESTAMP, format='%Y-%m-%d %H:%M:%S', tz='UTC')
  
#   #Set Lat/Long to decimal degrees rather than ddmm.mmmm
#   my.df[,c('longitude', 'latitude')]<-lapply(my.df[,c('longitude', 'latitude')], ConvertToDD)
#   
  return(my.df)

}


