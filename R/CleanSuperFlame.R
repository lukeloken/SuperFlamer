# #####################################################
# Script to clean erroneous values in flame data
# Uses Jordan Read's SensorQC program available via github
# Input is tau corrected and gas converted data table (convertdata) and rule table
# Output is cleaned data table (cleandata)
# #####################################################


library(devtools)
library(dplyr)
install_github('jread-usgs/sensorQC')
# install_github('USGS-R/sensorQC')
library(sensorQC)

CleanSuperFlame<-function(convertdata, ruletable, plotdiag=FALSE){
  
  # ==========================================================
  # Function to clean multiple parameters of a dataframe
  # col1 = times; col2 and up == variables
  # Requires a data.frame of rules that match convertdata column names
  # window = number of observations to include in both directions for rollingMAD
  # ==========================================================
  
  # Create emtpy data.frame to fill with cleaned values. 
  # Keep only column 1 (times)
  cleanedframe<- convertdata
  # cleanedframe$date_time<-NULL
  
  col=9
  #Clean all variables in convertdata starting with column 2
  for (col in 2:ncol(cleanedframe)){
    
    # Create sensor object and fill 'w' slot with rollingMAD
    data<-cleanedframe[,c(1,col), with=F]
    rules<- unlist(ruletable[which(ruletable[[1]]==names(data)[2]), -1:-3])
    
    if (length(rules)>0 & class(data[[2]])=='numeric'){
      
      sensored<-sensor(data)
      MADwindow<- unlist(ruletable[which(ruletable[,1]==names(data)[2]), 2])
      Medianwindow<- unlist(ruletable[which(ruletable[,1]==names(data)[2]), 3])
      sensored$sensor$w<-rollingMAD(sensored$sensor$x, Medianwindow, MADwindow)
      
      # Set rules manually or from ruletable and clean sensor object
      # rules<-as.character(c('w>3', 'is.na(x)'))
      
      cleanedframe[[col]]<-clean(sensored, rules, replace=NA)$sensor[[2]] 
      
      # Plot flagged (red) and retained (black) observations 
      
      if(plotdiag==TRUE){
        flags<-data[[2]]
        flags[which(flags==cleanedframe[[col]])]<-NA
        plot(data[[2]], col="black", ylab=names(data)[2], xlab="time", ylim=range(data[[2]], na.rm=TRUE))
        points(flags, col="red", pch=16)
      }
    }
    
  }
  
  return(cleanedframe)
}
