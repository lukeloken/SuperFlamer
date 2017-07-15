# ####################################
# Function to calculate rolling median
# ####################################

rollingMAD<-function (x, Median_Window, MAD_Window){
  
  # Create empty vector for computed MAD values
  WindowedMAD<-as.numeric(rep(NA, length(x)))
  
  for (obs in 1:length(x)){
    # Set interval of observations for MAD
    # truncate interval for observations near start and end
    MADseq<-c((obs-MAD_Window):(obs+MAD_Window))
    MADseq<-MADseq[which(MADseq<=length(x) & MADseq>=1)]
    MADinterval<-x[MADseq]
    
    # Set interval of observations for median
    # truncate interval for observations near start and end
    Medseq<-c((obs-Median_Window):(obs+Median_Window))
    Medseq<-Medseq[which(Medseq<=length(x) & Medseq>=1)]
    Medinterval<-x[Medseq]
    
    # Compute MAD
    localmedian<-median(Medinterval, na.rm=TRUE)
    localmad<-mad(MADinterval, na.rm=TRUE)
    WindowedMAD[obs]<-abs(x[obs]-localmedian)/localmad
  }
  
  # Replace infinite values (mad==0) with NA
  WindowedMAD[which(is.finite(WindowedMAD)==FALSE)]<-NA
  
  # Return vector of windowed MAD values.
  # Equal length to x
  return (WindowedMAD)
}