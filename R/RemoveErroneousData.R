
RemoveErroneousData <- function(trimdata, bad_data = NULL){
  
  trimdata2 <- trimdata
  
  #Manual remove data using `bad_data` table
  if(!is.null(bad_data)){
    cat("\n", "Checking data against 'bad_data'.", "\n")
    
    rows_bad <- which(bad_data$variable %in% names(trimdata2))
    row_i <- rows_bad[1]
    row_i <- 4
    for(row_i in rows_bad) {
      name_i <- bad_data$variable[row_i]
      col_i <- which(names(trimdata2) == name_i)
      trimdata2[trimdata2$date_time >= bad_data$bad_start[row_i] & 
                  trimdata2$date_time <= bad_data$bad_end[row_i], 
                col_i] <- NA
      plot(trimdata[,date_time], unlist(trimdata[,..col_i]), col = "red")
      points(trimdata2[,date_time], unlist(trimdata2[,..col_i]), col = "black")
      mtext(name_i, side = 3)
    }
  }
  
  #Remove data while EXO wiper is running
  if("wiperPos" %in% names(trimdata2)){
    cat("\n", "Removing EXO data when wiper was running.", "\n")
    
    median(trimdata2$wiperPos, na.rm = TRUE)
    hist(trimdata2$wiperPos)
    summary(trimdata2$wiperPos)
    # goodwiperrange <- quantile(trimdata2$wiperPos, c(.1, .9), na.rm = TRUE)
    
    goodwiperrange <- median(trimdata2$wiperPos, na.rm = TRUE) + c(-0.02, 0.02)
    
    wipedrows <- which(trimdata2$wiperPos < goodwiperrange[1] | 
                         trimdata2$wiperPos > goodwiperrange[2])
    
    plot(trimdata2$date_time, trimdata2$wiperPos)
    points(trimdata2$date_time[wipedrows], trimdata2$wiperPos[wipedrows], col = "red")
    
    exonames <- intersect(names(trimdata2), c("temp", "specCond", 
                                              "pH", "turb_FNU", 
                                              "ODO_percent", "ODO_mgL", 
                                              "chlor_RFU", "chlor_ugL", 
                                              "BGApc_RFU", "BGApc_ugL", 
                                              "fDOM_RFU", "fDOM_QSU"))
    exocolumns <- which(names(trimdata2) %in% exonames)
    
    x = exocolumns[1]
    trimdata2[wipedrows, exocolumns] <- NA
    
  } 
  return(trimdata2)
}
