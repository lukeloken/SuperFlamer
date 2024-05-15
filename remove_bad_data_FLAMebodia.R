

remove_bad_data_FLAMebodia <- function(){
  library(dplyr)
  bad_data <- ".dplyr"
  
  
  bad_data <- data.frame(variable = "NA", 
                         bad_start = as.POSIXct(NA, tz = "UTC"), 
                         bad_end = as.POSIXct(NA, tz = "UTC"))
  
  #CH4 and CO2
  bad_data <- bad_data %>%
    filter(variable != "NA") %>%
    bind_rows(data = data.frame(
      variable = c("CH4_Dry", "CO2_Dry"), 
      bad_start = as.POSIXct(("2022-01-21 06:46:40"), tz = "UTC"), 
      bad_end = as.POSIXct(c("2022-01-21 06:58:00", "2022-01-21 06:54:00"), tz = "UTC"))) %>%
    distinct()
  
  # #Fdom
  # bad_data <- bad_data %>%
  #   filter(variable != "NA") %>%
  #   bind_rows(data = data.frame(
  #     variable = c("fDOM_RFU"), 
  #     bad_start = as.POSIXct("2022-05-01 05:00:00", tz = "UTC"), 
  #     bad_end = as.POSIXct("2022-05-02 23:00:00", tz = "UTC"))) %>%
  #   distinct()
  # 
  # 
  
  return(bad_data)
  
}
