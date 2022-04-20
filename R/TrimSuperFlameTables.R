
# ##############################################################
# Cut SuperFLAMe data to reduce number of columns
# Input is 1) dataframe of cutdata
# Output is a trimmed datatable 
# ##############################################################

TrimSuperFlame<-function(cutdata){
  
  #Indicate variables to remove
  gps_vars <- c('date_time', 'latitude', 'longitude', 
                'speed', 'course', 'depth')
  box_vars <- c('temp_logger')
  exo_vars <- c('temp', 'specCond', 'pH', 'pressure', 
                'chlor_RFU', 'ODO_percent', 'ODO_mgL', 
                'BGApc_RFU', 'turb_FNU', 'fDOM_RFU')
  gga_vars <- c('CH4_Dry', 'H2O', 'CO2_Dry')
  suna_vars <- c('no3_uM', 'nn03_mg', 
                 'abs254', 'abs350', 
                 'temp_int', 'temp_spect', 'temp_lamp')
  turner_vars <- c("cdom_volt", "peakT_volt")
  
  
  vars_good <- c(gps_vars, box_vars, exo_vars, gga_vars, suna_vars, turner_vars)
  vars_keep <- intersect(vars_good, names(cutdata))
  
  trimdata<-cutdata[,vars_keep, with=FALSE] 
  
  #remove columns full of nas
  
  trimdata2<-trimdata[, as.vector(!apply(trimdata, 2, function(x) all(is.na(x)))), with=F]
  NAnames<-setdiff(names(trimdata), names(trimdata2))
  if (length(NAnames)>1) {
    warning(paste("Some columns contain all NAs :", paste(NAnames, collapse=" ")))
    }
  
  if (ncol(trimdata2)==0){
    stop("No matching column names. All columns trimmed")}
  
  return(trimdata2)
  
}