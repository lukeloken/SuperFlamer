

library(raster)
library(sf)
library(sp)
# library(rdgal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(spdplyr)
library(gstat)
library(geoR)
library(spatstat)
library(viridis)
library(rangemap)
library(e1071)
library(evd)



gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

maxdist = 10
subset = 10

color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), 
                                   rev(magma(5, begin=.25, end=.98))), 
                                 bias=1)

#Load water grid
# watergrid_300m_sp <- readRDS(file.path(onedrive_dir, "GIS",
#                                      "TonleSap_January2022_WaterGrid_300m.rds"))
# projection = proj4string(watergrid_300m_sp)

watergrid_150m_sp <- readRDS(file.path(onedrive_dir, "GIS",
                                       "TonleSap_January2022_WaterGrid_150m.rds"))

projection = proj4string(watergrid_150m_sp)

watergrid_predict <- watergrid_150m_sp

# plot(watergrid_300m_sp, col = "pink")
# plot(watergrid_150m_sp, col = "pink")
# plot(watergrid_predict, col = "pink")


# data_name <- "Merged_TonleSap_Jan_2022"
data_name <- "Merged_Mekong_TonleSap_JanApr_2022"

data_dir <- file.path(onedrive_dir,  
                      "Data", 
                      data_name)

data <- readRDS(file.path(data_dir, "Shapefiles",
                           paste0(data_name, "_Shapefile_AllData.rds")))

data <- spTransform(data, crs(watergrid_predict))

# variables <- names(data)[4:58]
variables <- names(data)[4:61]

# data2 <- data %>%
#   filter(!is.na(ODO_mgL))
# 
# predict <- gstat::idw(pull(select(data2@data, ODO_mgL)) ~ 1, 
#                       data2, 
#                       watergrid_predict, 
#                       idp=2)
# 
# print(spplot(predict, 
#              zcol='var1.pred', 
#              colorkey = TRUE, 
#              cuts = 99, 
#              col.regions = color.palette,
#              # sp.layout=list(l1, l2, l3, l4),
#              main="Predicted dissolved oxygen (mgL)",
#              xlim = bbox(watergrid_predict)[1,], 
#              ylim = bbox(watergrid_predict)[2,]))


# Copy from NHLD code
# Trying to subset based on proximity to measured values. 
data_sample <- data[sample(seq_along(data$date_time), nrow(data)/100),]

concave_cloud <- hull_polygon(data_sample, 
                              hull_type = "concave", 
                              concave_distance_lim = 100,
                              verbose = TRUE)

buffered_cloud <- gBuffer(concave_cloud, width = 1000*10)

# dev.off()
# plot(data_sample, cex = .5)
# plot(concave_cloud, add = TRUE, border = "purple")
# plot(buffered_cloud, add = TRUE, border = "magenta")


# # Generate 'data cloud' based on observations
# bdry <- ripras(coordinates(data))
# 
# # Convert data cloud to spatial polygon
# bdry_df <- data.frame(bdry[[4]][[1]]$x, bdry[[4]][[1]]$y)
# bdry_df[nrow(bdry_df)+1,]<-bdry_df[1,]
# bdry_poly<-Polygon(bdry_df)
# bdry_poly2 = Polygons(list(bdry_poly), "s1")
# bdry_poly_sp<-SpatialPolygons(list(bdry_poly2), proj4string=CRS(as.character(projection)))
# 
# # Make Buffer around data cloud polygon 
# # width = distance in meters; currently using two pixel distances
# buffered <- gBuffer(bdry_poly_sp, width = 1000*2)

# Make prediction area as intersection of buffered area and lake polygon
# Area<-gIntersection(buffered, lake_polygon)

# Check Area and Confirm
# dev.off()
# plot(watergrid_predict, col = "pink")
# plot(data_sample, add = TRUE, cex = .5)
# plot(concave_cloud, add = TRUE, border = "blue")
# plot(buffered_cloud, add = TRUE, border = "magenta")


# plot(lakes_Base, add=TRUE)
plot(watergrid_predict, add = TRUE)
# Make polygrid - This is each location to make predictions
watergrid_predict

# pts_in = over(SpatialPoints(watergrid_predict), SpatialPolygons(buffered@polygons), 
#                             returnlist = TRUE)
pts_in = over(SpatialPoints(watergrid_predict), SpatialPolygons(buffered_cloud@polygons), 
              returnlist = TRUE)

watergrid_predict_subset <- watergrid_predict[!is.na(pts_in),]

# plot(watergrid_predict, col = "pink")
# plot(watergrid_predict_subset, col = "blue", add = TRUE)



# plot data grid, boundary, and observations
# dev.off()
# plot(watergrid_predict_subset, col="blue")
# plot(buffered_cloud, add = TRUE, border = "magenta")
# plot(data_sample, add=TRUE, col="red", cex=0.2)

# Make spatial object to save surface predictions
watergrid_predict_subset_data <- watergrid_predict_subset %>%
  select(-layer)

watergrid_predict_subset_data@data[,variables] <- NA



# Make an empty summary table for each filename
# This will be populated with summary stats for each variable
summary_lake <- as.data.frame(matrix(nrow = length(variables), ncol=22))
names(summary_lake)<-c('Min', 'Q25', 'Median', 'Mean', 'Q75', 'Max', 'Q05', 'Q10', 'Q90', 'Q95', 'sd', 'SDL', 'n', 'mad', 'MADM', 'skewness', 'loc', 'scale', 'shape', 'CV', 'QuartileDispersion', 'MADMOverMedian')

# ==========================================
# Start of loop to run through each variable  
# ==========================================
var = variables[6]
for (var in variables){
  var_number <- which(variables==var)
  
  # Select only variable of interest
  data2<-data %>%
    select(all_of(var))
  
  #Identify column in data2 that contains variable (should be 1)
  column <- which(names(data2)==var) 
  
  data2 <- data2[which(!is.na(data2@data[,column])),]
  
  # Skip variable if all NAs
  if (nrow(data2)>0){
    
    #Add minimum if its a variable that has potential negative values
    # if (var %in% minvars[,1]){
    #   minimum<-minvars[which(var==minvars[,1]),2]
    #   data2@data[,column]<-data2@data[,column]-minimum
    # }
    
    # Plot Timeseries of variable
    # Make sure data seem reasonable
    # plot(data@data[,column1], type="p")
    
    #Transform data into UTM's. This way distance is in meters (m)
    data2<-spTransform(data2, CRS(projection))
    
    # Remove observations that are within maxdist (5m) of each other. 
    data2<-remove.duplicates(data2, zero=maxdist)
    
    #Plot heat map atop lake base polygon
    # spplot(data2[var], cuts=99, colorkey=TRUE, sp.layout = list(lakes_Base['Lake_Name']) )
    
    # subset (%) of the data. Take random percent of points
    # Depending on the analysis and size of data, R cannot handle entire dataset
    data3<-data2[sample(nrow(data2), nrow(data2)/subset), ]
    colnames(data3@coords)<-c("x", "y")
    
    # =========================
    # Using Inverse Distance Weighting predict values at each grid cell
    # idp = denominator exponent. 
    # idp = 1: 1/distance
    # idp = 2: 1/(distance squared)
    # =========================
    
    predict <- gstat::idw(pull(select(data3@data, all_of(var))) ~ 1,
                               data3, watergrid_predict_subset, idp = 2)
    
    names(predict) <- c(paste(var, sep=""), paste(var, "_v", sep=""))
    
    # par(mfrow=c(1,1))
    # par(mar=c(4,4,4,4), oma=c(1,1,1,1))
    # spplot(predict, names(predict)[1], colorkey=TRUE, cuts=99, sp.layout=list(lake_polygon['Lake_Name'], col=1, fill=0, lwd=3, lty=1, first=F) , main=paste(var, "_prediction_inverse_distance_weight", sep=""), xlim=bbox(lake_polygon)[1,], ylim=bbox(lake_polygon)[2,])
    
    # Create summary stats for variable
    values <- predict@data[,1]
    
    basic_stats<-summary(values)
    quantiles<-quantile(values, probs = c(0.05, .1, .9, 0.95),  na.rm = TRUE)
    
    summary_var<-c(basic_stats, 
                   quantiles, 
                   sd=sd(values), 
                   SDL=sd(log10(values), na.rm=T), 
                   n=length(values), 
                   mad=mad(values), 
                   MADM=median(abs(values-median(values))), 
                   skewness=skewness(values, na.rm = T))
    
    
    # Save summary info to summary table
    summary_lake[var_number,1:16]<-summary_var
    
    #if zero heterogeneity exists, skip evd and plotting
    if (identical(round(min(values), 3), round(max(values),3))==FALSE){
      
      # hist(values,breaks=20, xlab=var, main="", col="grey")
      evd <- fgev(values, std.err=F)
      evd$estimate
      summary_lake[var_number,17:19]<-evd$estimate
      
      
      # Save spatial data to spatial object
      watergrid_predict_subset_data@data[var_number]<-predict@data[1]
      
      # create subfolder 'maps_idw' if it does not already exist
      dir.create(file.path(data_dir, "Maps_idw", sep=""), showWarnings = FALSE)
      
      # Plot Spatial data
      png(paste(data_dir, "/Maps_idw/", var, ".png", sep=""), 
          res=200, width=6, height=6, units="in")
      xdist <- diff(bbox(watergrid_predict_subset_data)[1,1:2])
      scale <- signif(xdist/6, digits=1)
      # polyx<-c(bbox(lake_polygon)[1,1]+scale*(c(0.2,1.2)))
      # polyy<-c(bbox(lake_polygon)[2,1]+scale*c(.2,.4))
      # coords<-data.frame(x=c(polyx, rev(polyx)), y=c(rep(polyy[1], 2), rep(polyy[2], 2)))
      # poly_box<-Polygon(coords)
      # poly_box2<-Polygons(list(poly_box), "s1")
      # poly_box_sp<-SpatialPolygons(list(poly_box2), proj4string=CRS(as.character(projection)))
      # 
      polyx<-c(bbox(watergrid_predict_subset_data)[1,1]+scale*(c(0.2,1.2)))
      polyy<-c(bbox(watergrid_predict_subset_data)[2,1]+scale*c(.2,.4))
      coords<-data.frame(x=c(rep(polyx[1], 2), rep(polyx[2], 2)), y=c(rev(polyy), polyy))
      poly_line<-Line((coords))
      S1 = Lines(list(poly_line), ID="a")
      poly_line_sp<- SpatialLines(list(S1))
      
      # l1 = list(lake_polygon['Lake_Name'], col=1, fill=0, lwd=3, lty=1, first=F)
      l2 = list("SpatialPolygonsRescale", layout.north.arrow(type=1), 
                offset = 
                  c(polyx[1], polyy[1]+scale*.25),
                scale = scale*.5, first=FALSE) 
      l3<- list(poly_line_sp, fill=NA, lwd=2, lty=1, first=F)
      # l3<- list(poly_box_sp, fill=NA, lwd=2, lty=1, first=F)
      # mean(polyx), mean(polyy)
      # l3 = list("SpatialPolygonsRescale", layout.scale.bar(height=scale/1000), offset = 
      #             c(bbox(lake_polygon)[1,1]+0.5*scale,bbox(lake_polygon)[2,1]+scale),
      #             scale = scale, fill=c('black'), lwd=1, first=FALSE) 
      l4 = list("sp.text", c(mean(polyx), polyy[1]),
                paste0(scale/1000, " km"), cex=0.6, first=FALSE, pos=3) 
      
      print(spplot(watergrid_predict_subset_data, zcol = var, 
                   colorkey = TRUE, cuts = 99, 
                   col.regions = color.palette,
                   sp.layout=list(l2, l3, l4) , 
                   main = paste(var, data_name, sep = ": "),
                   sub = "Prediction using inverse distance weight",
                   xlim = bbox(watergrid_predict_subset_data)[1,], 
                   ylim=bbox(watergrid_predict_subset_data)[2,]))
      dev.off()
      closeAllConnections()
    }
  }  
}

# Add variable names to summary table
summary_lake$CV<-summary_lake$sd/summary_lake$Mean
summary_lake$QuartileDispersion<-(summary_lake$Q75 - summary_lake$Q25)/ (summary_lake$Q75 + summary_lake$Q25)
summary_lake$MADMOverMedian<-(summary_lake$MADM)/ (summary_lake$Median)
summary_lake$MaxMinusMin<-(summary_lake$Max) - (summary_lake$Min)
summary_lake$IQR<-(summary_lake$Q75) - (summary_lake$Q25)
summary_lake$Q95MinusQ05<-(summary_lake$Q95) - (summary_lake$Q05)

summary_lake$Variable<-variables


# Save shapefile of interpolated surface (spatial pixels data frame)
writeOGR(watergrid_predict_subset_data, dsn = file.path(data_dir, "Shapefiles"),
         layer = paste0(data_name, "_Shapefile_idw"),
         driver="ESRI Shapefile",  
         verbose=F, overwrite=T)

# Convert spatialpixesldataframe to raster
raster_withData <- stack(watergrid_predict_subset_data)

# Save raster of interpolated surface (stacked raster)
# Note - ArcMap cannot read this type of file
writeRaster(raster_withData, 
            file.path(data_dir, "Shapefiles", 
                      paste0(data_name, "_Raster_idw", sep="")), 
                      format='raster', overwrite=TRUE)

#Write summary to file
write.table(summary_lake, 
            file = file.path(data_dir, paste0(data_name, "_PixelSummaries.csv")),
            col.names = TRUE, row.names = FALSE, sep=",")
rm(summary_lake)
