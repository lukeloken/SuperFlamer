





#require spatial libraries
library(sp)
library(rgdal)


#############
#function to read table or dataframe and convert to shapefile
write_shapefile=function(spatial_dataset, file_out, destination, Site, Date, plot){
  #Create subfolder 'shapefiles' if it does not already exist
  folders<-list.files(path = paste(getwd(), "", sep=""))
  if(length(folders[folders=="shapefiles"])==0){
    dir.create(paste(getwd(), "/shapefiles", sep=""))}
  #Call spatial_dataset, assign projection (Decimal Degrees), write shapefile, and plot tracks
  spatial_dataset<-spatial_dataset[which(!is.na(spatial_dataset$Latitude) & !is.na(spatial_dataset$Longitude)),  ]
  coordinates(spatial_dataset) = ~Longitude+Latitude
  proj4string(spatial_dataset)=CRS("+init=epsg:4326")
  writeOGR(spatial_dataset, dsn=paste(destination, "/shapefiles", sep="") ,layer=file_out, driver="ESRI Shapefile",  verbose=T, overwrite=T)
  if (plot==TRUE){
    tiff(file=paste("GPSTrack_", file_out,".tif", sep=""), res=300, width=4, height=4, units="in")
    plot(spatial_dataset, pch=".", col="blue")
    dev.off()}
  return(spatial_dataset)
}
