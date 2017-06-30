
# ##############################################################
# Read SuperFLAMe data, construct a single file
# Input is folder directory
# Output is a merged datatable
# ##############################################################

dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota/RawData'
dir2<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota2'

library(data.table)

ReadSuperFlame<-function(dir){
  files<-list.files(dir)
  if (length(files) == 0) {
    stop("No files in directory")}
  
  #Merge data files that contain these strings in the file name
  patterns<-'GPS|BoxMetrics|EXO|GGA|Nitrate'
  loadfiles<-files[grep(patterns, files)]
  
  # Get header names
  header.list<-lapply(paste(dir, loadfiles, sep="/"), fread, sep=",", skip=1, nrows=0)
  # Get data
  import.list <- lapply(paste(dir, loadfiles, sep="/"), fread, sep=",", skip=4, header=F, stringsAsFactors=F, na.strings=c('NA', 'NaN', '', 'null', '"NAN"'))
  
  # Put header names on correct data frame
  for (df in 1:length(import.list)){
    names(import.list[[df]])<- names(header.list[[df]])
  }

  #Omit 'Records' column
  import.list <- lapply(import.list, function(x)x[,-c('RECORD')])

  #Merge datatables using TIMESTAMP
  my.df <- Reduce(function(x, y) merge(x, y, all=FALSE,by=c("TIMESTAMP"),all.x=TRUE, all.y=TRUE),import.list,accumulate=F) 
  
  #Set Date_Time as time (need to check timezone)
  my.df$Date_Time<-as.POSIXct(my.df$TIMESTAMP, format='%Y-%m-%d %H:%M:%S', tz='UTC')

  return(my.df)

}

#Test run

data<-ReadSuperFlame(dir)

str(data)

ReadSuperFlame(dir2)

