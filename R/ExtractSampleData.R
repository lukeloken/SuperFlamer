
# Extract Flame Data at Sample Locations
library(caTools)
library(survival)

ExtractSample<-function(cleandata, alldata, sample, dir, Date, tz){
  
  if (nrow(sample)==0){
    warning('no samples')}
  
  samplevars<-c ("sample_id", "event_id", "Sample.Number", "Sample.Time", "Sample.Notes")
  
  sample$DateTime<- as.POSIXct(round(as.POSIXct(paste(Date, sample$'Sample Time', sep=" "), format="%Y-%m-%d %H:%M:%S", tz=tz), "mins"))
  
  cleandata$date_time<-as.POSIXct(round(cleandata$date_time, "mins"))
  
  table2<-as.data.frame(aggregate(cleandata, by=list(cleandata$date_time), FUN=mean, na.rm=TRUE))
  tablevars<-names(cleandata)
  table2<-table2[tablevars]
  

  Flame_times <- merge(sample, table2, by.x="DateTime", by.y="date_time", all.x=TRUE)
  
  #If some data are missing, find gps location from alldata
  sample_missing <- filter(Flame_times, is.na(latitude*longitude)) %>%
    select(names(sample))
  if(nrow(sample_missing)>0){
    alldata$date_time<-as.POSIXct(round(alldata$date_time, "mins"))
    table3 <- as.data.frame(aggregate(alldata, by=list(alldata$date_time), 
                                      FUN=mean, na.rm=TRUE)) %>%
      select(date_time, latitude, longitude)
    
    usedaterow <- sapply(sample_missing$DateTime, function(x) which.min(abs(table3$date_time - x)))
    
    table4 <- table3[usedaterow,] %>%
      mutate(listed_date_time = sample_missing$DateTime) %>%
      select(-date_time )
    
    Flame_times_missing <- merge(sample_missing, table4, by.x="DateTime", by.y="listed_date_time", all.x=TRUE)
    
    Flame_times <- Flame_times %>%
      filter(!is.na(latitude*longitude)) %>%
      full_join(Flame_times_missing) %>%
      arrange(DateTime)
    
  }
  
  
  return(Flame_times) 
}

