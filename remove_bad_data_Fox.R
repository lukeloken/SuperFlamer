

remove_bad_data_Fox <- function(){
  bad_data <- ".dplyr"
  
  
  bad_data <- data.frame(variable = "NA", 
                         bad_start = as.POSIXct(NA, tz = "UTC"), 
                         bad_end = as.POSIXct(NA, tz = "UTC"))

  
  #Turbidity
  bad_data <- bad_data %>%
    filter(variable != "NA") %>%
    bind_rows(data = data.frame(
      variable = c("turb_FNU"), 
      bad_start = as.POSIXct(c("2023-08-07 20:10:00", "2023-08-07 21:30:00", "2023-08-07 22:20:00"), tz = "UTC"), 
      bad_end = as.POSIXct(c("2023-08-07 21:05:00", "2023-08-07 21:50:00", "2023-08-07 22:35:00"), tz = "UTC"))) %>%
    distinct()
  
  
  return(bad_data)
  
}
