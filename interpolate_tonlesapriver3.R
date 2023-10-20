
#load libraries
#Some of these are not necessary

library(PerformanceAnalytics)
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
# library(rangemap)
library(e1071)
library(evd)
library(caret)
library(doParallel)
library(car)
library(gstat)
library(stringr)
library(lubridate)
library(riverdist)

# gis_dir <- "P:/0368/GIS"
#folder with gis layers
gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"

#folder with flame data
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

# MAP_dir <- file.path(onedrive_dir, "GIS", "MAP")
# list.files(MAP_dir)

#parameters for interpolation
maxdist = 10
subset = 50

#custom color flame color palette
color.palette = colorRampPalette(c(viridis(6, begin=.02, end=.98), 
                                   rev(magma(5, begin=.5, end=.98))), 
                                 bias=1)


#load an example watergrid to get the projection
MAP_dir <- file.path(onedrive_dir, "GIS", "MAP")
res = "500m"
extent = "20km"
watergrid_predict <- readRDS(file.path(MAP_dir,
                                       paste0("TonleSap_",
                                              "2022-01-16", 
                                              "_WaterGrid_", res, "_", extent, "_buffer.rds")))


watergrid_predict
projection = proj4string(watergrid_predict)
crs_predict <- crs(watergrid_predict)
# projection = c("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs")




#load lots of shapefiles for mapping and reference

#Load the proper shapefiles
TL_admin <- st_read(file.path(gis_dir, "Mekong"), "TonleSapLakeBoundary") %>%
  st_transform(projection)

TLC_admin <- st_read(file.path(gis_dir, "Mekong"), "TonleChamarPolygon") %>%
  st_transform(projection)

TLS_watershed <- st_read(file.path(gis_dir, "Mekong"), "TonleSapWatershed_Polygon") %>%
  st_transform(projection)


#load Tonle Sap Rivers
TL_rivers <- st_read(file.path(gis_dir, "Mekong"), "TonleSapRiverInputs2") %>%
  st_combine() %>%
  st_transform(projection)

TL_union <- readRDS(file.path(onedrive_dir, 
                              "GIS",
                              "TL_union.rds"))

TLR <- st_read(file.path(gis_dir, "Mekong",  "TonleSapRiver_BelowLake.shp")) %>%
  st_combine() %>%
  st_transform(projection)



MR <- st_read(file.path(gis_dir, "Mekong",  "Mekong_SingleLine.shp")) %>%
  st_combine() %>%
  st_transform(projection)

plot(st_geometry(TLS_watershed))
plot(st_geometry(TL_admin), add = TRUE, col = "darkblue")
plot(st_geometry(TL_union), add = TRUE, border = "darkblue")
plot(st_geometry(TLR), add = TRUE, col = "blue")


#make a buffer around river
TLR_buffer <- st_buffer(TLR, dist = 1000)
plot(st_geometry(TLR), col = "blue")

plot(st_geometry(TLR_buffer), add = TRUE, border = "red")




# Folder names in flamebodia data folder
# We will loop through these

flame_dates <- c("Merged_TonleSap_Jan_2022", 
                 "Merged_TonleSap_Apr_2022",
                 "Merged_TonleSap_Sep_2022",
                 "Merged_TonleSap_Jan_2023")

# or we can run one at a time
# flame_date_i <- "Merged_TonleSap_Jan_2022"
# flame_date_i <- "Merged_TonleSap_Sep_2022"
# flame_date_i <- "Merged_TonleSap_Apr_2022"
# flame_date_i <- "Merged_TonleSap_Jan_2023"



#Set the file number so that the correct flame data are referenced. 
file_nu <- 1
flame_date_i <- flame_dates[file_nu]
title_i <- paste(unlist(strsplit(flame_date_i, "_"))[3:4], collapse = " ")

cat("\n", "Starting river interpolation with with", flame_date_i, "\n\n")


# get Flame data
data_dir <- file.path(onedrive_dir,  
                      "Data", 
                      flame_date_i)


data_all <- readRDS(file.path(data_dir, "Shapefiles",
                              paste0(flame_date_i, "_Shapefile_AllData.rds")))

# plot(data_all, add = TRUE)

data_all <- spTransform(data_all, crs_predict)

#convert to an sf object
data_sf <- as(data_all, "sf")

# plot(st_geometry(data_sf))
# plot(st_geometry(TLR_buffer), border = "red", add = TRUE)

plot(st_geometry(TLR_buffer), border = "red")
plot(st_geometry(data_sf), add = TRUE)

TLR_points <- sf::st_filter(data_sf, TLR_buffer) %>%
  mutate(group = "TLR")

plot(st_geometry(TLR_buffer), border = "red", axes = TRUE)
plot(st_geometry(data_sf), add = TRUE, col = "blue")
plot(st_geometry(TLR_points), col = "black", add= TRUE)


# x_range <- range(st_coordinates(TLR_points)[,1])
# y_range <- range(st_coordinates(TLR_points)[,2])

#Crop data to fit within boundary or river
tlr_x_range <- range(st_coordinates(TLR)[,1])
tlr_y_range <- range(st_coordinates(TLR)[,2])

filter_sf <- function(.data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  bb <- sf::st_bbox(.data)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  sf::st_filter(.data, sf::st_as_sfc(bb), .predicate = sf::st_within)
}

TLR_points <- data_sf %>%
  filter_sf(ymax = tlr_y_range[2] + 800) %>%
  filter_sf(ymin = tlr_y_range[1] - 00) %>%
  filter_sf(xmax = tlr_x_range[2] - 1000) %>%
  mutate(group = "TLR")

plot(st_geometry(TLR_buffer), border = "red", axes = TRUE)
plot(st_geometry(TLR_points), col = "black", add= TRUE)
plot(st_geometry(TLR), col = "blue", add= TRUE)
plot(st_geometry(MR), col = "darkblue", add= TRUE)

abline(h = tlr_y_range[2] + 1000, col = "green")
abline(h = tlr_y_range[1] -00, col = "green")


#objects to run into analysis
data <- as(TLR_points, "Spatial")

TLR_sp <- as(TLR, "Spatial")
TLR_buffer_sp <- as(TLR_buffer, "Spatial")



#################################################################
#Prepare empty line feature where your predictions will be saved
#################################################################

# Create linenetwork and compute cumulative distance. Only do this once
# TLRNetwork <- line2network(sp = TLR_sp)
# TLRNetwork_clean<-cleanup(TLRNetwork)
# saveRDS(TLRNetwork_clean, file = file.path(gis_dir, 'Mekong', 'TonleSapRiver_BelowLakeClean.rds'))

#load network you just created
TLRNetwork_clean <- readRDS(file = file.path(gis_dir, 'Mekong', 'TonleSapRiver_BelowLakeClean.rds'))

#Extract spatial lines from network
line_out <- TLRNetwork_clean$sp

coords <- coordinates(TLRNetwork_clean$sp[1])

#Create empty points data.frame from line network
# This is where your predictions will go

points_out <- data.frame(x = coords[[1]][[1]][,1], y = coords[[1]][[1]][,2])
coordinates(points_out) <- ~x+y
proj4string(points_out) = CRS(projection)



plot(st_geometry(TLR_buffer), border = "red", axes = TRUE)
plot(points_out, add= TRUE)
points_out$Dist <- unlist(TLRNetwork_clean$cumuldist)
points_out$Dist_round <- round(points_out$Dist, 0) #round to nearest meter

spplot(points_out, zcol = "Dist", colorkey = TRUE, cuts = 99)

# create a two-dimension feature for predictions
# But this is a straight line with a make believe coordinate system
# This has x = distance, y = 1. We will use a x/y interpolation but only use one dimension for prediction
# here the sequence is every 10 m
line_predict <- data.frame(x = seq(min(unlist(TLRNetwork_clean$cumuldist)), 
                                   max(unlist(TLRNetwork_clean$cumuldist)), 
                                   10), 
                           y = 1)
coordinates(line_predict) = ~x+y


# subset points feature 
#filter points_out to 10m resolution matching the line_predict feature
which_points <- which(points_out@data$Dist_round %in% line_predict$x)

points_out_10m <- points_out[which_points,]

data_out <- points_out_10m



##############
# Prepare data
##############

#Snap data to linenetwork
data_snapped <- xy2segvert(x = coordinates(data)[,1], y=coordinates(data)[,2], rivers=TLRNetwork_clean)

# add linear distance to all of your data points
data$Dist <- unlist(TLRNetwork_clean$cumuldist)[data_snapped$vert]



# proj4string(line_predict) = CRS(projection)

#convert data to linear (two-dimensional) feature
# x = distance, y = 1. Matches the make believe prediction feature above
data_linear <- data.frame(data@data, y = 1)
coordinates(data_linear) = ~Dist+y
# proj4string(data_linear) = CRS(projection)


#establish variables to interpolate
# variables <- names(data)[4:57]
variables <- c(
  "ODO_percent", "ODO_percent_tau",
  "ODO_mgL", "ODO_mgL_tau",
  "CH4Sat", "CH4Sat_tau",
  "CH4uM", "CH4uM_tau",
  "CO2Sat", "CO2Sat_tau",
  "CO2uM", "CO2uM_tau"
)


variable_labels <- list(
  expression(paste("Dissolved oxygen-raw (%)")),
  expression(paste("Dissolved oxygen-tau (%)")),
  expression(paste("Dissolved oxygen-raw (mg L"^"-1", ")")),
  expression(paste("Dissolved oxygen-tau (mg L"^"-1", ")")),
  expression(paste("Methane-raw (%)")),
  expression(paste("Methane-tau (%)")),
  expression(paste("Methane-raw (", mu, "M)")),
  expression(paste("Methane-tau (", mu, "M)")),
  expression(paste("Carbon dioxide-raw (%)")),
  expression(paste("Carbon dioxide-tau (%)")),
  expression(paste("Carbon dioxide-raw (", mu, "M)")),
  expression(paste("Carbon dioxide-tau (", mu, "M)")))

#add variables to output object
# data_out@data[,variables] <- NA

df_temp <- data.frame(matrix(nrow = nrow(data_out@data), ncol = length(variables)))
names(df_temp) <- variables

# ==========================================
# Start of loop to run through each variable
# ==========================================

# Make an empty summary table for each filename
# This will be populated with summary stats for each variable
summary_river <- as.data.frame(matrix(nrow = length(variables), ncol=22))
names(summary_river)<-c('Min', 'Q25', 'Median', 'Mean', 'Q75', 'Max', 'Q05', 'Q10', 'Q90', 'Q95', 'sd', 'SDL', 'n', 'mad', 'MADM', 'skewness', 'loc', 'scale', 'shape', 'CV', 'QuartileDispersion', 'MADMOverMedian')

predict_df <- data.frame(coordinates(line_predict), var1.pred = NA)


var = variables[3]
for (var in variables[1:length(variables)]){
  var_number <- which(variables == var)
  
  cat("\n", "Starting ", var, "\n\n")
  
  
  #Identify column number in data that contains variable
  column <- which(names(data_linear)==var)
  
  #filter data so no NAs
  data_idw <- data_linear[which(!is.na(data_linear@data[,column])),]
  
  #predict concentrations
  predict <- gstat::idw(pull(dplyr::select(data_idw@data, all_of(var))) ~ 1,
                        data_idw,
                        line_predict,
                        idp = 2)
  
  predict_df$var1.pred <- predict$var1.pred
  

  points_out_10m@data <- points_out_10m@data %>%
    dplyr::select(1:2) %>% 
    left_join(dplyr::select(predict_df, Dist_round = x, var1.pred), 
              by = "Dist_round")
  
  df_temp[,var_number] <- as.numeric(points_out_10m$var1.pred)

  
  # spplot(points_out_10m, "var1.pred", colorkey = TRUE, cuts = 99,
  #        main = paste(var, " prediction_inversedistanceweight", sep=""),
  #        xlim = bbox(TLR_buffer_sp)[1,], ylim = bbox(TLR_buffer_sp)[2,])
  # 
  # 
       
  # Create summary stats for variable
  values <-  points_out_10m$var1.pred
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
  summary_river[var_number,1:16] <- summary_var


  

  #if zero heterogeneity exists, skip evd and plotting
  if (identical(round(min(values), 3), round(max(values),3))==FALSE){
    
    # hist(values,breaks=20, xlab=var, main="", col="grey")
    evd <- evd::fgev(values, std.err=F)
    evd$estimate
    summary_river[var_number,17:19]<-evd$estimate

    
    # Save spatial data to spatial object
    # data_out@data[which(names(data_out@data) == var)] <- points_out_10m$var1.pred
    
    
    
    # create subfolder 'maps_dist' if it does not already exist
    dir.create(file.path(data_dir, paste0("Maps_TonleSapRiver_10m")), showWarnings = FALSE)
    
    # Plot Spatial data
    png(paste(data_dir, "/Maps_TonleSapRiver_10m/", var, ".png", sep=""),
        res=200, width=4, height=7, units="in")
    xdist <- diff(bbox(data_out)[1,1:2])
    scale <- signif(xdist/4.5, digits=1)
    # polyx<-c(bbox(lake_polygon)[1,1]+scale*(c(0.2,1.2)))
    # polyy<-c(bbox(lake_polygon)[2,1]+scale*c(.2,.4))
    # coords<-data.frame(x=c(polyx, rev(polyx)), y=c(rep(polyy[1], 2), rep(polyy[2], 2)))
    # poly_box<-Polygon(coords)
    # poly_box2<-Polygons(list(poly_box), "s1")
    # poly_box_sp<-SpatialPolygons(list(poly_box2), proj4string=CRS(as.character(projection)))
    #
    polyx<-c(bbox(data_out)[1,1]+scale*(c(0.2,1.2)))
    polyy<-c(bbox(data_out)[2,1]+scale*c(.2,.4))
    coords<-data.frame(x=c(rep(polyx[1], 2), rep(polyx[2], 2)), y=c(rev(polyy), polyy))
    poly_line<-Line((coords))
    S1 = Lines(list(poly_line), ID="a")
    poly_line_sp<- SpatialLines(list(S1))

    # l1 = list(lake_polygon['Lake_Name'], col=1, fill=0, lwd=3, lty=1, first=F)
    l2 = list("SpatialPolygonsRescale", layout.north.arrow(type=1),
              offset =
                c(polyx[1], polyy[1]+scale*.25),
              scale = scale*.75, first=FALSE)
    l3 <- list(poly_line_sp, fill=NA, lwd=2, lty=1, first=F)
    # l3<- list(poly_box_sp, fill=NA, lwd=2, lty=1, first=F)
    # mean(polyx), mean(polyy)
    # l3 = list("SpatialPolygonsRescale", layout.scale.bar(height=scale/1000), offset =
    #             c(bbox(lake_polygon)[1,1]+0.5*scale,bbox(lake_polygon)[2,1]+scale),
    #             scale = scale, fill=c('black'), lwd=1, first=FALSE)
    l4 = list("sp.text", c(mean(polyx), polyy[1]),
              paste0(scale/1000, " km"), cex=0.6, first=FALSE, pos=3)

    print(spplot(points_out_10m, zcol = "var1.pred",
                 colorkey = TRUE, cuts = 99,
                 # col.regions = color.palette,
                 sp.layout=list(l2, l3, l4) ,
                 main = paste(var, flame_date_i, sep = ": "),
                 sub = "Prediction using inverse distance weight",
                 xlim = bbox(TLR_buffer_sp)[1,],
                 ylim = bbox(TLR_buffer_sp)[2,]))
    dev.off()
    closeAllConnections()
    
    
  }
  #remove a bunch of temporary stuff
  
  # rm(values, predict,
  #    basic_stats, quantiles,
  #    summary_var, evd)
  # 
  # 
  # # data3$bc <- NA
  rm(predict)
  points_out_10m@data <- points_out_10m@data[,1:2]
  predict_df <- predict_df[,1:2]

}
data_out@data <- cbind(data_out@data, df_temp)
# 
# r <- reclassify(water_raster_500m_classified, cbind(-Inf, Inf, 1))
# pp <- rasterToPolygons(r, dissolve=TRUE)
# 
# # look at the results
# plot(water_raster_500m_classified)
# plot(pp, lwd=3, border='blue', add=TRUE)
# 
# pp_sf <- as(pp, "sf")

data_out_sf <- as(data_out, "sf")

var = variables[3]
for (var in variables[1:length(variables)]){
  
  # label_i <- expression(paste("Dissolved oxygen-raw (%)"))
  # label_i <- expression(paste("Carbon dioxide (", mu, "M)"))
  
  label_i = variable_labels[[which(variables == var)]]
  
  df_i <- filter(data_out_sf, !is.na(.data[[var]]))
  
  if(nrow(df_i) == 0){next}
  
  
  g1 <- ggplot(df_i) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    # theme(panel.background = element_rect(fill = "gray")) +
    # geom_sf(data = pp_sf, color = "NA", linewidth = 0.5, fill = "white") +
    geom_sf(aes(color = .data[[var]]), shape = 15, size = 1.5, inherit.aes = FALSE) +
    # geom_sf(data = TL_admin, fill = "NA", color = "black") +
    labs(color = label_i, x= "Longitude", y = "Latitude") +
    # geom_sf(data = TL_rivers, color = "blue") +
    # scale_color_continuous(type = "viridis", trans = "log10")
    scale_color_gradientn(colors = color.palette(11)) +
    theme(legend.position = c(0.01, 0.01),
          legend.justification = c(0,0),
          legend.box.background = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10)) +
    guides(color = guide_colorbar(direction = "horizontal",
                                  title.position = "top",
                                  label.position = "bottom",
                                  title.hjust = 0.5,
                                  barwidth = unit(1.8, units = "in"),
                                  barheight = unit(0.2, units = "in"),
                                  frame.colour = "black")) +
    scale_x_continuous(expand = c(0.01,0.01), breaks = seq(-180, 180, by = 0.1)) +
    scale_y_continuous(expand = c(0.01,0.01)) +
    geom_text(data = data.frame(x = Inf, y = Inf, text = title_i),
              aes(x = x, y = y, label = title_i),
              hjust = 1.1, vjust = 1.5)
  
  
  if (grepl("CH4", var) | grepl("CO2", var)){
    g1 <- g1 +
      scale_color_gradientn(colors = color.palette(11), trans = "log10")
  }
  
  print(g1)
  
  ggsave(paste(data_dir, "/Maps_TonleSapRiver_10m/", var, "_gg.png", sep=""),
         g1,
         dpi = 400, width = 5, height = 8, units = "in")
  
}


# Add variable names to summary table
summary_river$CV<-summary_river$sd/summary_river$Mean
summary_river$QuartileDispersion<-(summary_river$Q75 - summary_river$Q25)/ (summary_river$Q75 + summary_river$Q25)
summary_river$MADMOverMedian<-(summary_river$MADM)/ (summary_river$Median)
summary_river$MaxMinusMin<-(summary_river$Max) - (summary_river$Min)
summary_river$IQR<-(summary_river$Q75) - (summary_river$Q25)
summary_river$Q95MinusQ05<-(summary_river$Q95) - (summary_river$Q05)

summary_river$Variable<-variables

saveRDS(data_out_sf, file.path(data_dir, "Shapefiles", paste0(flame_date_i, "_Predict_tonlesapriver_sf.rds")))

# Save shapefile of interpolated surface (spatial pixels data frame)
# writeOGR(watergrid_predict_subset_data, dsn = file.path(data_dir, "Shapefiles"),
#          layer = paste0(flame_date_i, "_Shapefile_krige"),
#          driver="ESRI Shapefile",
#          verbose=F, overwrite=T)

st_write(data_out_sf,
         file.path(data_dir, "Shapefiles", paste0(flame_date_i, "_Shapefile_tonlesapriver.shp")), append = FALSE)


#Write summary to file
write.table(summary_river,
            file = file.path(data_dir, paste0(flame_date_i, "_PixelSummaries_tonlesapriver.csv")),
            col.names = TRUE, row.names = FALSE, sep=",")
rm(summary_river)

