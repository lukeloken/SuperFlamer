


# ##############################################################
# Cut SuperFLAMe data based on Flame on/off times
# Input is 1) dataframe of merged files
# 2) direcotry where meta table is saved
# Output is a cut datatable 
# ##############################################################

library(data.table)
library(stringr)
library(lubridate)

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
  
  # Remove error data based on strings
  # New as of Jan 2022
  # cutdata2 <- cutdata
  
  stringdata <- cutdata %>%
    select(contains("GGA_String"))
  
  # head(stringdata)
  
  i <- 1
  for (i in seq_along(ncol(stringdata))){
    
    string_i <- pull(stringdata, i)
    
    if(names(stringdata)[i] == "GGA_String"){
      
      n_commas_i <- str_count(string_i, pattern=",")
      
      #split based on commas
      string_split_list_i <- str_split(string_i, ",[ ]*")
      
      #lgr should have these values in these places
      first_string_i <- unlist(lapply(string_split_list_i, function(l) l[1])) #time
      second_string_i <- suppressWarnings(as.numeric(unlist(lapply(string_split_list_i, function(l) l[2])))) #ch4
      fourth_string_i <- suppressWarnings(as.numeric(unlist(lapply(string_split_list_i, function(l) l[4])))) #h2o
      sixth_string_i <- suppressWarnings(as.numeric(unlist(lapply(string_split_list_i, function(l) l[6])))) #co2
      last_string_i <- unlist(lapply(string_split_list_i, function(l) l[length(l)])) #last "Disabled"
      
      #See if first string can be a date
      # date_i <- as.POSIXct(first_string_i, format = "%Y/%m/%d %H:%M:%S")
      date_i <- ymd_hms(first_string_i, quiet = TRUE)
      
      
      df_check_i <- data.frame(n_commas_i, date_i,
                               second_string_i, fourth_string_i, 
                               sixth_string_i, last_string_i)
      
      #Checks
      df_check_out <- df_check_i %>%
        mutate(pass = ifelse(!is.na(date_i) &
                               is.finite(second_string_i * fourth_string_i * sixth_string_i) &
                               last_string_i == "Disabled",
                             TRUE, FALSE)) %>%
        pull(pass)
      
      cutdata[which(df_check_out == FALSE) ,
              c("CH4_Wet", "CH4_Dry", "H2O", "CO2_Wet", "CO2_Dry")] <- NA
      
    }
    
  }
  
  # head(cutdata)
  
  return(cutdata)
  
}
