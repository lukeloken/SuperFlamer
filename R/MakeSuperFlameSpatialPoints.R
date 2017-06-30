

# ##############################################################
# Convert data to spatial points data frame
# Input is 1) dataframe of trimmed data
# Output is a spatial points dataframe 
# ##############################################################

#require spatial libraries
library(sp)
library(rgdal)

MakeSuperFlameSpatialPoints<-function(trimdata){
  
  geodata<-trimdata[which(!is.na(trimdata$latitude) & !is.na(trimdata$longitude)),  ]
  
  if (nrow(geodata)==0){
    stop("No observations with Lat/Long data")}
  
  coordinates(geodata) = ~longitude+latitude
  proj4string(geodata)=CRS("+init=epsg:4326")
  
  return(geodata)
  
}
