

remove_bad_data_Illinois <- function(){
  library(dplyr)
  bad_data <- ".dplyr"
  
  
  bad_data <- data.frame(variable = "NA", 
                         bad_start = as.POSIXct(NA, tz = "UTC"), 
                         bad_end = as.POSIXct(NA, tz = "UTC"))
  
  #BGA
  bad_data <- bad_data %>%
    filter(variable != "NA") %>%
    bind_rows(data = data.frame(
      variable = c("BGApc_RFU", "BGApc_ugL"), 
      bad_start = as.POSIXct(("2022-05-04 17:00:00"), tz = "UTC"), 
      bad_end = as.POSIXct(("2022-05-06 05:00:00"), tz = "UTC"))) %>%
    distinct()
  
  #Fdom
  bad_data <- bad_data %>%
    filter(variable != "NA") %>%
    bind_rows(data = data.frame(
      variable = c("fDOM_RFU"), 
      bad_start = as.POSIXct("2022-05-01 05:00:00", tz = "UTC"), 
      bad_end = as.POSIXct("2022-05-02 23:00:00", tz = "UTC"))) %>%
    distinct()
  
  
  bad_data <- bad_data %>%
    filter(variable != "NA") %>%
    bind_rows(data = data.frame(
      variable = c("fDOM_RFU"), 
      bad_start = as.POSIXct(("2022-05-03 20:00:00"), tz = "UTC"), 
      bad_end = as.POSIXct(("2022-05-05 23:00:00"), tz = "UTC"))) %>%
    distinct()
  
  #Turbidity
  bad_data <- bad_data %>%
    filter(variable != "NA") %>%
    bind_rows(data = data.frame(
      variable = c("turb_FNU"), 
      bad_start = as.POSIXct(("2022-05-01 05:00:00"), tz = "UTC"), 
      bad_end = as.POSIXct(("2022-05-05 23:00:00"), tz = "UTC"))) %>%
    distinct()
  
  #Turbidity Aug 2022
  bad_data <- bad_data %>%
    filter(variable != "NA") %>%
    bind_rows(data = data.frame(
      variable = c("turb_FNU"), 
      bad_start = as.POSIXct(("2022-08-08 05:00:00"), tz = "UTC"), 
      bad_end = as.POSIXct(("2022-08-10 05:00:00"), tz = "UTC"))) %>%
    distinct()
  
  return(bad_data)
  
}
