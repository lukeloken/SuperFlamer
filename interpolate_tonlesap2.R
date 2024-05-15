

library(raster)
library(sf)
library(sp)
library(rdgal)
library(rgeos)
library(dplyr)
library(ggplot2)
# library(spdplyr)
library(gstat)
library(geoR)
library(spatstat)
library(viridis)
library(rangemap)
library(e1071)
library(evd)
library(caret)
library(doParallel)
library(car)
library(gstat)

# gis_dir <- "P:/0368/GIS"
#folder with gis layers
gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"

#folder with flame data
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

#parameters for interpolation
maxdist = 10
subset = 50

#custom color flame color palette
color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), 
                                   rev(magma(5, begin=.25, end=.98))), 
                                 bias=1)

#Need to figure out how to loop through the 5 files and do the whole code for each

date = as.Date("2022-01-14")


# #Load focal
focal_100m_500m <- readRDS(file.path(onedrive_dir, 
                                     "GIS",
                                     paste0("TonleSap_", date, "_focal_100m_500m.rds")))

focal_100m_2500m <- readRDS(file.path(onedrive_dir,
                                      "GIS",
                                      paste0("TonleSap_", date, "_focal_100m_2500m.rds")))


# #Load NIR
# #300 m resolution
# nir_raster_300m_classified <- readRDS(file.path(onedrive_dir,
#                                                 "GIS",
#                                                 "TonleSap_January2022_nir_raster_300m_classified.rds"))
# raster_classified <- nir_raster_300m_classified

# Load water grid (spatial pixels dataframe)
# This will be where we make our predictions
# watergrid_300m_sp <- readRDS(file.path(onedrive_dir, "GIS",
#                                        "TonleSap_January2022_WaterGrid_300m.rds"))

# res = 1000
# watergrid_1000m_sp <- readRDS(file.path(onedrive_dir, "GIS",
#                                         "TonleSap_2022-01-14_WaterGrid_1000m.rds"))
# 
# 
# water_raster_1000m_classified <- readRDS(file.path(onedrive_dir,
#                                                    "GIS",
#                                                    paste0("TonleSap_", date, "_raster_1000m_classified.rds")))
# 
# #identify projection to ensure all other features have the same projection
# watergrid_predict <- watergrid_1000m_sp
# water_raster_classified <- water_raster_1000m_classified

res = 500
watergrid_500m_sp <- readRDS(file.path(onedrive_dir, "GIS",
                                        "TonleSap_2022-01-14_WaterGrid_500m.rds"))


water_raster_500m_classified <- readRDS(file.path(onedrive_dir,
                                                   "GIS",
                                                   paste0("TonleSap_", date, "_raster_500m_classified.rds")))

#identify projection to ensure all other features have the same projection
watergrid_predict <- watergrid_500m_sp
water_raster_classified <- water_raster_500m_classified
projection = proj4string(watergrid_predict)


#load Tonle Sap Rivers
TL_rivers <- st_read(file.path(gis_dir, "Mekong"), "TonleSapRiverInputs2") %>%
  st_combine() %>%
  st_transform(projection)

TL_union <- readRDS(file.path(onedrive_dir, 
                              "GIS",
                              "TL_union.rds"))


#folder names in flamebodia data folder
#Eventually we will loop through these, one for each campaign on the lake
# (Jan 2022, April 2022, Sept 2022, Jan 2023)
data_name <- "Merged_TonleSap_Jan_2022"
# data_name <- "Merged_TonleSap_Sep_2022"
# data_name <- "Merged_TonleSap_Apr_2022"
# data_name <- "Merged_TonleSap_Jan_2023"

data_dir <- file.path(onedrive_dir,  
                      "Data", 
                      data_name)

data <- readRDS(file.path(data_dir, "Shapefiles",
                          paste0(data_name, "_Shapefile_AllData.rds")))



data <- spTransform(data, crs(watergrid_predict))

#establish variables to interpolate
variables <- names(data)[4:57]
variables <- c("ODO_percent", "ODO_percent_tau", 
               "ODO_mgL", "ODO_mgL_tau",
               "CH4Sat", "CH4Sat_tau",
               "CH4uM", "CH4uM_tau", 
               "CO2uM", "CO2uM_tau", 
               "CO2Sat", "CO2Sat_tau")

#simple interpolation using inverse distance weighting
var = variables[1]

#Identify column number in data that contains variable
column <- which(names(data)==var) 

#filter data so no NAs
data_idw <- data[which(!is.na(data@data[,column])),]

#predict concentrations
predict <- gstat::idw(pull(select(data_idw@data, all_of(var))) ~ 1,
                      data_idw,
                      watergrid_predict,
                      idp=2)

#notice how bad the image looks. The boat paths are clearly visible.
print(spplot(predict,
             zcol='var1.pred',
             colorkey = TRUE,
             cuts = 99,
             col.regions = color.palette,
             # sp.layout=list(l1, l2, l3, l4),
             main= var,
             xlim = bbox(watergrid_predict)[1,],
             ylim = bbox(watergrid_predict)[2,]))


#subset data to improve procesing speed
data_sample <- data[sample(seq_along(data$date_time), nrow(data)/subset),]


# # Trying to subset based on proximity to measured values. 
# #old code using rangemap. Not available for R 4.3
# 
# concave_cloud <- rangemap::hull_polygon(data_sample, 
#                                         hull_type = "concave", 
#                                         concave_distance_lim = 100,
#                                         verbose = TRUE)
# 
# buffered_cloud <- gBuffer(concave_cloud, width = 1000*20)

# dev.off()
# plot(data_sample, cex = .5)
# plot(concave_cloud, add = TRUE, border = "purple")
# plot(buffered_cloud, add = TRUE, border = "magenta")


# #old code probably deletec
# raster_cropped <- crop(raster_classified, buffered_cloud)
# crop_box <- as(extent(228585, 624885, 1345000, 1555215), "SpatialPolygons")
# crop_box <- as(extent(335000, 501000, 1345000, 1480000), "SpatialPolygons")
# crs(crop_box) <- crs(raster_cropped)
# raster_cropped2 <- crop(raster_classified, crop_box)
# # raster_cropped2 <- crop(raster_classified, crop_box)
# 
# plot(raster_cropped2)
# plot(crop_box, add = TRUE)
# plot(data_sample, cex = .5, add = TRUE)
# 
# data_sample_cropped <- crop(data_sample, raster_classified)
# data_sample_cropped_sf <- st_filter(st_as_sf(data_sample), TL_union)
# data_sample_cropped <- as(data_sample_cropped_sf, "Spatial")
# 
# plot(raster_classified)
# plot(st_geometry(data_sample_cropped_sf), add = TRUE)
# plot(data_sample_cropped, add = TRUE, col = "blue")

# raster_cropped2 <- crop(raster_cropped, crop_box)
# watergrid_predict2 <- crop(watergrid_predict, crop_box)

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

# ##################################################
# Add distance to land to data
# ##################################################
# create classification matrix
#This was originally done to make all 'land' pixels the same value
#As long as water and land are two different integers (0, 1) this should work. 
# reclass_df_2 <- c(0, 0.5, 0,
#                   0.5, 1, 1,
#                   1, Inf, 3)
# reclass_df_2
# 
# reclass_m_2 <- matrix(reclass_df_2,
#                       ncol = 3,
#                       byrow = TRUE)
# 
# reclass_m_2
# 
# r_classified <- reclassify(raster_classified,
#                            reclass_m_2)

time_start <- Sys.time()

out_data <- list()

for (i in 1:nrow(data_sample)) {
  # for (i in 3871:5000) {
  d <- distanceFromPoints(water_raster_classified, data_sample[i, drop = F])
  out_data[[i]] <- zonal(d, water_raster_classified, min)[,2] #may need to change this to pull the right distance metric
}
a_data <- do.call(rbind, out_data)
b_data <- cbind(data_sample, a_data)
# names(b)[-1] <- paste("DistTo_", 1:6)

b_data$dist_to_land = b_data@data$X1

spplot(b_data, zcol = "dist_to_land", cuts = c(600, seq(800, max(b_data$dist_to_land), length.out = 10)))


# b_data$dist_to_land = apply(b_data@data[,c("X2", "X3")], 1, function(x) min(x, na.rm = TRUE)) #might need to modify with 0, 1 instead of 1:6

#not that helpful anymore
# b_data$dist_to_green = b_data$X2


b_data$focal_100m_500m <- raster::extract(focal_100m_500m, 
                                          b_data, 
                                          weights=FALSE, 
                                          fun=mean, 
                                          na.rm = TRUE)
b_data$focal_100m_2500m <- raster::extract(focal_100m_2500m, 
                                           b_data, 
                                           weights=FALSE, 
                                           fun=mean, 
                                           na.rm = TRUE)

b_sf <- st_as_sf(b_data) 

b_sf$dist_to_river = st_distance(b_sf, st_sf(TL_rivers))
b_data$dist_to_river <-  as.numeric(b_sf$dist_to_river)

# plot(b_data["dist_to_river"], add = TRUE)
# plot(TL_rivers)


spplot(b_data, zcol = "dist_to_land", cuts = c(0, seq(300, max(b_data$dist_to_land), length.out = 10)))


spplot(b_data, zcol = "dist_to_river")

spplot(b_data, zcol = "focal_100m_500m")
spplot(b_data, zcol = "focal_100m_2500m")



time_end <- Sys.time()
message(round(difftime(time_end, time_start, units = "hours"), 3), " hours to process")



# plot(lakes_Base, add=TRUE)
plot(watergrid_predict)
# Make polygrid - This is each location to make predictions
# watergrid_predict

# pts_in = over(SpatialPoints(watergrid_predict), SpatialPolygons(buffered@polygons), 
#                             returnlist = TRUE)
# pts_in = over(SpatialPoints(watergrid_predict), SpatialPolygons(buffered_cloud@polygons), 
#               returnlist = TRUE)

# watergrid_predict_subset <- watergrid_predict[!is.na(pts_in),] %>%
#   mutate(log10_dist_to_land = log10(dist_to_land))

watergrid_predict_subset <- watergrid_predict 

watergrid_predict_subset@data <- watergrid_predict_subset@data %>% 
  mutate(log10_dist_to_land = log10(dist_to_land))


plot(watergrid_predict, col = "pink")
plot(watergrid_predict_subset, col = "blue")

# spplot(raster_cropped2)
# plot(watergrid_predict_subset, add = TRUE)
spplot(b_data, zcol = "dist_to_land")


# plot data grid, boundary, and observations
# dev.off()
# plot(watergrid_predict_subset, col="blue")
# plot(buffered_cloud, add = TRUE, border = "magenta")
# plot(data_sample, add=TRUE, col="red", cex=0.2)

# Make spatial object to save surface predictions
watergrid_predict_subset_data <- watergrid_predict_subset

watergrid_predict_subset_data@data <- watergrid_predict_subset_data@data %>%
  dplyr::select(-VH)
watergrid_predict_subset_data@data[,variables] <- NA

watergrid_predict_subset_data@data <- watergrid_predict_subset_data@data %>%
  dplyr::select(all_of(variables), everything())

# Make an empty summary table for each filename
# This will be populated with summary stats for each variable
summary_lake <- as.data.frame(matrix(nrow = length(variables), ncol=22))
names(summary_lake)<-c('Min', 'Q25', 'Median', 'Mean', 'Q75', 'Max', 'Q05', 'Q10', 'Q90', 'Q95', 'sd', 'SDL', 'n', 'mad', 'MADM', 'skewness', 'loc', 'scale', 'shape', 'CV', 'QuartileDispersion', 'MADMOverMedian')

# ==========================================
# Start of loop to run through each variable  
# ==========================================
var = variables[7]
for (var in variables[1:length(variables)]){
  var_number <- which(variables == var)
  
  # Select only variable of interest
  data2 <- b_data 
  data2@data <- data2@data %>%
    dplyr::select(all_of(var), "dist_to_land", 
                  "focal_100m_500m", "focal_100m_2500m",
                  "dist_to_river")
  
  # if (grepl("CH4", var)){
  #   data2@data <- data2@data %>%
  #     dplyr::select(all_of(var), "dist_to_land", 
  #                   "focal_100m_500m", "focal_100m_2500m",
  #                   "dist_to_river")
  # }
  
  #Identify column in data2 that contains variable (should be 1)
  column <- which(names(data2)==var) 
  
  data2 <- data2[which(!is.na(data2@data[,column])),]
  
  # Skip variable if all NAs
  if (nrow(data2)>0){
    
    #Determine extent and set window size so bins are every 3 meters
    coords = cbind(data2@coords[ ,1], data2@coords[ ,2])
    coordsmatrix <- as.matrix(coords)
    dist   <- dist(coordsmatrix)
    range  <- range(dist)
    diff   <-range[2]-range[1]
    window <-diff/100
    cutoff <- diff/2
    
    
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
    # data3 <- data2[sample(nrow(data2), nrow(data2)/subset), ]
    
    row_good <- which(!is.na(data2@data$dist_to_land) & 
                        !is.na(data2@data$dist_to_river) &
                      !is.na(data2@data$focal_100m_500m) &
                      !is.na(data2@data$focal_100m_2500m)
    )
    # filter(!is.na(dist_to_land), !is.na(dist_to_river), 
    #        !is.na(focal_300m_4500m), !is.na(focal_300m_1500m))
    # 
    data3 <- data2[row_good,]
    
    data3@data <- data3@data %>%
      mutate(log10_dist_to_land = log10(dist_to_land)) 
    
    
    colnames(data3@coords)<-c("x", "y")
    
    raw_values <- pull(dplyr::select(data3@data, all_of(var)))
    negative_offset <- min(raw_values, na.rm = TRUE) - 0.001
    if(negative_offset < 0) {negative_offset == 0}
    adjust_values <- raw_values - negative_offset
    
    #options for transfomring data
    #use data to identify ideal power adjustment. Not sure exactly how this works
    # power <- powerTransform(adjust_values)
    # data3$bc <- bcPower(adjust_values, power$lambda)
    
    # if(grepl("CH4", var)){
      data3$bc <- log10(adjust_values) #testing for CH4
    # }
    
    
    PREDICTOR <- select(data3@data, dist_to_land, dist_to_river, 
                        focal_100m_500m, focal_100m_2500m
    )
    RESPONSE <- pull(select(data3@data, bc))
    
    # #########################
    # Regression Kriging
    # 1. Fit a Genealized Linear Model (GLM) with the co-variate (distance to shore)
    # 2. Compute residuals from the model
    # 3. Fit a variogram of the residuals
    # 4. Simple Kriging (SK) will be applied to the model residuals to generate spatial prediction (regional trends)
    # 5. GLM predictions and SK of model residuals will be added to interpolate each variable
    # https://zia207.github.io/geospatial-r-github.io/regression-kriging.html#:~:text=Regression%20kriging%20combines%20a%20regression,spatial%20prediction%20of%20the%20residuals.
    
    # # use multiple cores
    # mc <- makeCluster(detectCores())
    # registerDoParallel(mc)
    # 
    # myControl <- trainControl(method = "repeatedcv", 
    #                           number = 10, 
    #                           repeats = 5,
    #                           allowParallel = TRUE)
    
    #    Generalized Linear Model
    set.seed(1856)
    GLM <- caret::train(x = PREDICTOR,
                        y = RESPONSE,
                        method = "gam",
                        # trControl = myControl,
                        preProc=c('center', 'scale'))
    print(GLM)
    summary(GLM)
    
    data3$residuals.glm <- resid(GLM)
    
    v.glm <- variogram(residuals.glm ~ 1, data = data3, cutoff = cutoff, width = window)
    
    est_sill<-median(v.glm$gamma)
    est_range<-cutoff/4
    est_nugget<-v.glm$gamma[1]
    
    m.f.glm <- fit.variogram(v.glm, vgm(est_sill=est_sill, 
                                        c("Nug", "Lin", "Sph"), 
                                        est_range=est_range, 
                                        nugget=est_nugget), 
                             fit.method = 2)
    
    plot(v.glm, m.f.glm)
    
    #Predicting at each location based on distance to land
    watergrid_predict_subset$GLM <- predict(GLM, watergrid_predict_subset@data)
    
    #Krige residuals
    t0 <- Sys.time()
    SK.GLM <- krige(residuals.glm~1, 
                    loc = data3,        # Data frame
                    newdata = watergrid_predict_subset,     # Prediction location
                    model = m.f.glm,     # fitted variogram model
                    beta = 0)            # residuals from a trend; expected value is 0  
    t1 <- Sys.time()
    message(round(difftime(t1, t0, units = "hours"), 3), " hours to Krige")
    
    names(SK.GLM) <- c(paste(var, sep=""), paste(var, "_v", sep=""))
    
    spplot(SK.GLM, names(SK.GLM)[1], colorkey=TRUE, cuts=99) # plot of residuals in transformed space
    spplot(watergrid_predict_subset, "GLM", colorkey=TRUE, cuts=99) #plot of regression estimates transformed
    
    
    watergrid_predict_subset$SK.GLM <- SK.GLM@data[, names(SK.GLM)[1]]
    
    #add glm model estimates with kriged estimates of residuals
    watergrid_predict_subset$RK.GLM.bc <- (watergrid_predict_subset$GLM+watergrid_predict_subset$SK.GLM)
    
    # #return from transformed space
    # k1 <- 1/power$lambda      
    # watergrid_predict_subset$RK.GLM <-((watergrid_predict_subset$RK.GLM.bc *power$lambda+1)^k1) + negative_offset
    # # summary(watergrid_predict_subset)
    
    #return from transformed space log10 for CH4
    # if (grepl("CH4", var)){
      watergrid_predict_subset$RK.GLM <-(10^watergrid_predict_subset$RK.GLM.bc) + negative_offset
    # }
    
    #map of predictions in native unit
    spplot(watergrid_predict_subset, "RK.GLM", colorkey=TRUE, cuts=99, 
           main=paste(var, " prediction_Krig_Regression_DistToLand", sep=""), 
           xlim=bbox(water_raster_classified)[1,], ylim=bbox(water_raster_classified)[2,])
    
    #log predictionspace
    spplot(watergrid_predict_subset, "RK.GLM.bc", colorkey=TRUE, cuts=99, 
           main=paste(var, " prediction_Krig_Regression_DistToLand", sep=""), 
           xlim=bbox(water_raster_classified)[1,], ylim=bbox(water_raster_classified)[2,])
    
    
    
    # Create summary stats for variable
    values <-  watergrid_predict_subset$RK.GLM
    values <- values[which(!is.na(values))]
    
    basic_stats <- summary(values)
    quantiles<-quantile(values, probs = c(0.05, .1, .9, 0.95),  na.rm = TRUE)
    
    summary_var<-c(basic_stats, 
                   quantiles, 
                   sd = sd(values), 
                   SDL = sd(log10(values), na.rm=T), 
                   n = length(values), 
                   mad = mad(values), 
                   MADM = median(abs(values-median(values))), 
                   skewness = skewness(values, na.rm = T))
    
    
    # Save summary info to summary table
    summary_lake[var_number,1:16] <- summary_var
    
    
    
    #if zero heterogeneity exists, skip evd and plotting
    if (identical(round(min(values), 3), round(max(values),3))==FALSE){
      
      # hist(values,breaks=20, xlab=var, main="", col="grey")
      evd <- fgev(values, std.err=F)
      evd$estimate
      summary_lake[var_number,17:19]<-evd$estimate
      
      # Save spatial data to spatial object
      watergrid_predict_subset_data@data[var_number] <- watergrid_predict_subset$RK.GLM
      
      # create subfolder 'maps_dist' if it does not already exist
      dir.create(file.path(data_dir, paste0("Maps_dist_", res)), showWarnings = FALSE)
      
      # Plot Spatial data
      png(paste(data_dir, "/Maps_dist_", res, "/", var, ".png", sep=""), 
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
      l3 <- list(poly_line_sp, fill=NA, lwd=2, lty=1, first=F)
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
                   sub = "Prediction using regression Kriging",
                   xlim = bbox(watergrid_predict_subset_data)[1,], 
                   ylim=bbox(watergrid_predict_subset_data)[2,]))
      dev.off()
      closeAllConnections()
    }
    #remove a bunch of temporary stuff
    
    rm(PREDICTOR, RESPONSE, power, 
       GLM, v.glm, est_sill, est_range, est_nugget, m.f.glm, 
       SK.GLM, k1, values, 
       basic_stats, quantiles, 
       summary_var, evd)
    
    
    data3$bc <- NA
    watergrid_predict_subset$RK.GLM.bc <- NA
    watergrid_predict_subset$SK.GLM <- NA
    watergrid_predict_subset$GLM <- NA
    data3$residuals.glm <- NA
  }
}

# plot(v.glm, pl=F, 
#      model=m.f.glm,
#      col="black", 
#      cex=0.9, 
#      lwd=0.5,
#      lty=1,
#      pch=19,
#      main="Variogram and Fitted Model\n Residuals of GLM model",
#      xlab="Distance (m)",
#      ylab="Semivariance")

# =========================
# Using Inverse Distance Weighting predict values at each grid cell
# idp = denominator exponent. 
# idp = 1: 1/distance
# idp = 2: 1/(distance squared)
# =========================

# 
#     v = variogram(pull(select(data3@data, all_of(var)))~1, 
#                   data3, cutoff = cutoff, width = window)
#     
#     est_sill<-median(v$gamma)
#     est_range<-cutoff/4
#     est_nugget<-v$gamma[1]
#     
#     #fit model to variogram
#     v.fit <- fit.variogram(v, vgm(est_sill=est_sill, 
#                                   c("Nug", "Lin", "Sph"), 
#                                   est_range=est_range, 
#                                   nugget=est_nugget), 
#                            fit.method = 2)
#     
#     plot(v, v.fit)
#     
#     v.ked <- variogram(pull(select(data3@data, all_of(var)))~log10(dist_to_land) + 1, 
#                        data3, cutoff = cutoff, width = window)
#     
#     vmf.ked <- fit.variogram(v.ked,
#                              vgm(est_sill=est_sill, 
#                                  c("Nug", "Lin", "Sph"), 
#                                  est_range=est_range, 
#                                  nugget=est_nugget), 
#                              fit.method = 2)
#     
#     plot(v.ked, vmf.ked)
#     
#     k.ked <- krige(pull(select(data3@data, all_of(var)))~log10(dist_to_land) + 1,
#                    locations=data3,
#                    newdata=watergrid_predict_subset, model=vmf.ked)
#     
#     
#     predict_idw <- gstat::idw(pull(select(data3@data, all_of(var))) ~ 1,
#                           data3, watergrid_predict_subset, idp = 2)
#     
#     
#     predict <- gstat::idw(pull(select(data3@data, all_of(var))) ~ log10(dist_to_land),
#                           data3, watergrid_predict_subset, idp = 2)
#     
#     predict_krige <- gstat::krige(pull(select(data3@data, all_of(var))) ~ log10(dist_to_land),
#                            data3, watergrid_predict_subset, model = v.fit)
#     
#     names(predict_idw) <- c(paste(var, sep=""), paste(var, "_v", sep=""))
#     names(predict) <- c(paste(var, sep=""), paste(var, "_v", sep=""))
#     
#     # par(mfrow=c(1,1))
#     # par(mar=c(4,4,4,4), oma=c(1,1,1,1))
#     # spplot(predict, names(predict)[1], colorkey=TRUE, cuts=99, sp.layout=list(lake_polygon['Lake_Name'], col=1, fill=0, lwd=3, lty=1, first=F) , main=paste(var, "_prediction_inverse_distance_weight", sep=""), xlim=bbox(lake_polygon)[1,], ylim=bbox(lake_polygon)[2,])
#     
#     spplot(predict_idw, names(predict)[1], colorkey=TRUE, cuts=99, 
#            main=paste(var, "_prediction_inverse_distance_weight", sep=""), 
#            xlim=bbox(raster_cropped2)[1,], ylim=bbox(raster_cropped2)[2,])
#     
#     spplot(predict, names(predict)[1], colorkey=TRUE, cuts=99, 
#            main=paste(var, "_prediction_inverse_distance_weight", sep=""), 
#            xlim=bbox(raster_cropped2)[1,], ylim=bbox(raster_cropped2)[2,])
#     
# 
# # Create summary stats for variable
# values <- predict@data[,1]
# 
# basic_stats<-summary(values)
# quantiles<-quantile(values, probs = c(0.05, .1, .9, 0.95),  na.rm = TRUE)
# 
# summary_var<-c(basic_stats, 
#                quantiles, 
#                sd=sd(values), 
#                SDL=sd(log10(values), na.rm=T), 
#                n=length(values), 
#                mad=mad(values), 
#                MADM=median(abs(values-median(values))), 
#                skewness=skewness(values, na.rm = T))
# 
# 
# # Save summary info to summary table
# summary_lake[var_number,1:16]<-summary_var

#if zero heterogeneity exists, skip evd and plotting
# if (identical(round(min(values), 3), round(max(values),3))==FALSE){
#   
#   # hist(values,breaks=20, xlab=var, main="", col="grey")
#   evd <- fgev(values, std.err=F)
#   evd$estimate
#   summary_lake[var_number,17:19]<-evd$estimate
#   
#   
#   # Save spatial data to spatial object
#   watergrid_predict_subset_data@data[var_number] <- predict@data[1]
#   
#   # create subfolder 'maps_idw' if it does not already exist
#   dir.create(file.path(data_dir, "Maps_dist", sep=""), showWarnings = FALSE)
#   
#   # Plot Spatial data
#   png(paste(data_dir, "/Maps_dist/", var, ".png", sep=""), 
#       res=200, width=6, height=6, units="in")
#   xdist <- diff(bbox(watergrid_predict_subset_data)[1,1:2])
#   scale <- signif(xdist/6, digits=1)
#   # polyx<-c(bbox(lake_polygon)[1,1]+scale*(c(0.2,1.2)))
#   # polyy<-c(bbox(lake_polygon)[2,1]+scale*c(.2,.4))
#   # coords<-data.frame(x=c(polyx, rev(polyx)), y=c(rep(polyy[1], 2), rep(polyy[2], 2)))
#   # poly_box<-Polygon(coords)
#   # poly_box2<-Polygons(list(poly_box), "s1")
#   # poly_box_sp<-SpatialPolygons(list(poly_box2), proj4string=CRS(as.character(projection)))
#   # 
#   polyx<-c(bbox(watergrid_predict_subset_data)[1,1]+scale*(c(0.2,1.2)))
#   polyy<-c(bbox(watergrid_predict_subset_data)[2,1]+scale*c(.2,.4))
#   coords<-data.frame(x=c(rep(polyx[1], 2), rep(polyx[2], 2)), y=c(rev(polyy), polyy))
#   poly_line<-Line((coords))
#   S1 = Lines(list(poly_line), ID="a")
#   poly_line_sp<- SpatialLines(list(S1))
#   
#   # l1 = list(lake_polygon['Lake_Name'], col=1, fill=0, lwd=3, lty=1, first=F)
#   l2 = list("SpatialPolygonsRescale", layout.north.arrow(type=1), 
#             offset = 
#               c(polyx[1], polyy[1]+scale*.25),
#             scale = scale*.5, first=FALSE) 
#   l3 <- list(poly_line_sp, fill=NA, lwd=2, lty=1, first=F)
#   # l3<- list(poly_box_sp, fill=NA, lwd=2, lty=1, first=F)
#   # mean(polyx), mean(polyy)
#   # l3 = list("SpatialPolygonsRescale", layout.scale.bar(height=scale/1000), offset = 
#   #             c(bbox(lake_polygon)[1,1]+0.5*scale,bbox(lake_polygon)[2,1]+scale),
#   #             scale = scale, fill=c('black'), lwd=1, first=FALSE) 
#   l4 = list("sp.text", c(mean(polyx), polyy[1]),
#             paste0(scale/1000, " km"), cex=0.6, first=FALSE, pos=3) 
#   
#   print(spplot(watergrid_predict_subset_data, zcol = var, 
#                colorkey = TRUE, cuts = 99, 
#                col.regions = color.palette,
#                sp.layout=list(l2, l3, l4) , 
#                main = paste(var, data_name, sep = ": "),
#                sub = "Prediction using inverse distance weight and distance to land",
#                xlim = bbox(watergrid_predict_subset_data)[1,], 
#                ylim=bbox(watergrid_predict_subset_data)[2,]))
#   dev.off()
#   closeAllConnections()
#     }
#   }  
# }

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
         layer = paste0(data_name, "_Shapefile_krige"),
         driver="ESRI Shapefile",  
         verbose=F, overwrite=T)

# Convert spatialpixesldataframe to raster
raster_withData <- stack(watergrid_predict_subset_data)

# Save raster of interpolated surface (stacked raster)
# Note - ArcMap cannot read this type of file
writeRaster(raster_withData, 
            file.path(data_dir, "Shapefiles", 
                      paste0(data_name, "_Raster_krige", sep="")), 
            format='raster', overwrite=TRUE)

#Write summary to file
write.table(summary_lake, 
            file = file.path(data_dir, paste0(data_name, "_PixelSummaries_krige.csv")),
            col.names = TRUE, row.names = FALSE, sep=",")
rm(summary_lake)
