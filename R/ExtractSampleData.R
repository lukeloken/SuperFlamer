
# Extract Flame Data at Sample Locations
library(caTools)

ExtractSample<-function(trimdata, sample, dir, Date, tz){
  
  if (nrow(sample)==0){
    warning('no samples')}
  
  samplevars<-c ("sample_id", "event_id", "Sample.Number", "Sample.Time", "Sample.Notes")
  
  sample$DateTime<-as.POSIXct(round(as.POSIXct(paste(Date,sample$'Sample Time', sep=" "), format="%Y-%m-%d %H:%M:%S", tz=tz), "mins"))
  
  trimdata$date_time<-as.POSIXct(round(trimdata$date_time, "mins"))
  
  table2<-as.data.frame(aggregate(trimdata, by=list(trimdata$date_time), FUN=mean, na.rm=TRUE))
  tablevars<-names(trimdata)
  table2<-table2[tablevars]
  
  Flame_times<-merge(sample, table2, by.x="DateTime", by.y="date_time", all.x=TRUE)
  return(Flame_times) 
}

