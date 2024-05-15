
#Explore the tonle sap rasters of NIR imagery. 
#First data is from January 2022

library(raster)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)
# library(spdplyr)
library(qlcMatrix)

#Load wetted perimeter raster
gis_dir <- "P:/0368/gis" 

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

#Load the admin boundaries for Tonle Sap and Tonle Chamar
#load using sf
TLS_watershed <- st_read(file.path(gis_dir, "Mekong"), "TonleSapWatershed_Polygon")
TLS_outline <- st_read(file.path(gis_dir, "Mekong"), "TonleSapLakeBoundary")
TLC_outline <- st_read(file.path(gis_dir, "Mekong"), "TonleChamarPolygon")

#Make a buffer around lakes and subset raster (using 20 km)
TLS_outline_buffer <- st_buffer(TLS_outline, dist = 1000*20)
TLC_outline_buffer <- st_buffer(TLC_outline, dist = 1000*20)
TL_union <- st_union(TLS_outline_buffer, TLC_outline_buffer)


TL_rivers <- st_read(file.path(gis_dir, "Mekong"), "TonleSapRiverInputs2") %>%
  st_combine()


#Need to figure out how to loop through the 5 files and do the whole code for each
TLS_water_files <- list.files(file.path(gis_dir, "GEE", "edit16_100m_reducer_conditional_2023"), ".tif")


file = TLS_water_files[1]
date = gsub("TLS_", "", file)
date = as.Date(gsub("VH.tif", "", date))

water_raster <- raster(file.path(gis_dir, "Gee", "edit16_100m_reducer_conditional_2023", 
                                 file))
terra::res(water_raster)


water_raster_UTM <- projectRaster(water_raster, crs = crs(TLS_watershed))
terra::res(water_raster_UTM)

TL_rivers <- st_transform(TL_rivers, crs(water_raster_UTM))

#or project all of the shapefiles to the raster projection
# TLS_watershed <- st_transform(TLS_watershed, crs(water_raster))
# TL_union <- st_transform(TL_union, crs(water_raster))
# TL_rivers <- st_transform(TL_rivers, crs(water_raster))
# TLS_outline <- st_transform(TLS_outline, crs(water_raster))
# TLC_outline <- st_transform(TLC_outline, crs(water_raster))


water_raster_cropped <- crop(water_raster_UTM, TL_union)
water_raster_masked <- mask(water_raster_cropped, TL_union)

plot(water_raster_masked, col = c("tan", "darkblue"), legend = FALSE)
plot(st_geometry(TL_union), add = TRUE, border = "black", fill =NA)

# #load the NIR raster
# nir_raster <- raster(file.path(gis_dir, "TonleSap_FromAlan", 
#                                "LC08_L1TP_126051_20220102_20220106_02_T1_B5_MERGED.tif"))
# names(nir_raster) <- "layer"
# 
# nir_raster <- reclassify(nir_raster, cbind(-Inf, 0.000001, NA), right = FALSE)
# # nir_raster <- clamp(nir_raster, lower = 0.0000001)
# names(nir_raster) <- "layer"

# 
# #load the 5 SAR rasters
# sar_files <- list.files(file.path(gis_dir, "Sentinel1", "Oct_2022_VH"), full.names = TRUE)
# sar_list <- lapply(sar_files, raster)
# plot(sar_list[[1]])
# plot(sar_list[[2]])
# 
# m1 <- mosaic(sar_list[[1]], sar_list[[2]], fun=max, tolerance = .5)
# m1 <- mosaic(m1, sar_list[[3]], fun=max, tolerance = .5)
# m1 <- mosaic(m1, sar_list[[4]], fun=max, tolerance = .5)
# m1 <- mosaic(m1, sar_list[[5]], fun=max, tolerance = .5)
# 
# plot(m1)
# 
# saveRDS(m1, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "Oct_2022_VH_mosaic.rds"))


# plot(st_geometry(TLS_watershed))
# plot(water_raster_masked, col = c("tan", "darkblue"), legend = FALSE, add = TRUE)
# plot(st_geometry(TLS_outline), border = "skyblue", add = TRUE)
# plot(st_geometry(TLC_outline), border = "skyblue", add = TRUE)
# plot(st_geometry(TL_rivers), col = "black", add = TRUE)

# #Make a buffer around lakes and subset raster (using 20 km)
# TLS_outline_buffer <- st_buffer(TLS_outline, dist = 1000*20)
# TLC_outline_buffer <- st_buffer(TLC_outline, dist = 1000*20)
# TL_union <- st_union(TLS_outline_buffer, TLC_outline_buffer)
# 
# plot(st_geometry(TLS_outline_buffer), add = TRUE, fill = NA)
# plot(st_geometry(TLC_outline_buffer), add = TRUE, fill = NA)
# plot(st_geometry(TL_union), add = TRUE, fill = NA, border = "blue", lwd = 2)



plot(water_raster_masked)
# plot(st_geometry(TLS_watershed), add = TRUE)
plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
plot(st_geometry(TL_union), add = TRUE, fill = NA, border = "grey50", lwd = 2)
plot(st_geometry(TL_rivers), col = "darkblue", add = TRUE)


#aggregate from native resolution to a number of scales
#Some of these will be used to identify where to predict
#Courser scales will be used as co-predictors (how much water is around you?)

#aggregate to 300x300 (factor = 3)
water_raster_300m <- aggregate(water_raster_masked, fact = 3)
terra::res(water_raster_300m)
plot(water_raster_300m)

#aggregate to 500x500 (factor = 5)
water_raster_500m <- aggregate(water_raster_masked, fact = 5)
terra::res(water_raster_500m)
plot(water_raster_500m)

#aggregate to 1000x1000 (factor = 10)
water_raster_1000m <- aggregate(water_raster_masked, fact = 10)
terra::res(water_raster_1000m)
plot(water_raster_1000m)

#aggregate to 2000x2000 (factor = 20)
water_raster_2000m <- aggregate(water_raster_masked, fact = 20)
terra::res(water_raster_2000m)
plot(water_raster_2000m)

#aggregate to 4000x4000 (factor = 40)
water_raster_4000m <- aggregate(water_raster_masked, fact = 40)
terra::res(water_raster_4000m)
plot(water_raster_4000m)


#Identify how many neighboring cells are land/water
#These give a sense of the surrounding habitat around a given cell
#500 m x 500 m cell around 100 m focal cell
focal_100m_500m = focal(water_raster_masked, w = matrix(1, nrow = 5, ncol = 5), fun = mean)
plot(focal_100m_500m)

#1500 m x 1500 m cell around 100 m focal cell
# focal_100m_1500m = focal(water_raster_masked, w = matrix(1, nrow = 15, ncol = 15), fun = mean)
# plot(focal_100m_1500m)

#2500 m x 2500 m cell around 100 m focal cell
focal_100m_2500m = focal(water_raster_masked, w = matrix(1, nrow = 25, ncol = 25), fun = mean)
plot(focal_100m_2500m)


# saveRDS(TL_union, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TL_union.rds"))


saveRDS(focal_100m_500m, 
        file.path(onedrive_dir, 
                  "GIS",
                  paste0("TonleSap_", date, "_focal_100m_500m.rds")))

saveRDS(focal_100m_2500m, 
        file.path(onedrive_dir, 
                  "GIS",
                  paste0("TonleSap_", date, "_focal_100m_2500m.rds")))



# #Test summarize neighborhood (150m)
# focal_150m_1350m = focal(nir_raster_150m, w = matrix(1, nrow = 9, ncol = 9), fun = mean)
# plot(focal_150m_1350m)
# plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
# plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
# 
# 
# focal_150m_4350m = focal(nir_raster_150m, w = matrix(1, nrow = 29, ncol = 29), fun = mean)
# plot(focal_150m_4350m)
# plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
# plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
# 
# 
# # focal_150m_1350m_max = focal(nir_raster_150m, w = matrix(1, nrow = 9, ncol = 9), fun = max)
# # plot(focal_150m_1350m_max)
# # plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
# # plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
# # 
# # 
# # focal_150m_4350m_max = focal(nir_raster_150m, w = matrix(1, nrow = 29, ncol = 29), fun = max)
# # plot(focal_150m_4350m_max)
# # plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
# # plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
# 
# 
# classify values. group 1 is water
# create classification matrix
reclass_df <- c(0, 0.5, 0,
                0.5, 1, 1)
reclass_df

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

reclass_m

water_raster_classified <- reclassify(water_raster_masked,
                                      reclass_m)
water_raster_300m_classified <- reclassify(water_raster_300m,
                                           reclass_m)
water_raster_500m_classified <- reclassify(water_raster_500m,
                                           reclass_m)
water_raster_1000m_classified <- reclassify(water_raster_1000m,
                                            reclass_m)
water_raster_2000m_classified <- reclassify(water_raster_2000m,
                                            reclass_m)
water_raster_4000m_classified <- reclassify(water_raster_4000m,
                                            reclass_m)

#Save each - not sure which resolution will work best
saveRDS(water_raster_4000m_classified,
        file.path(onedrive_dir,
                  "GIS",
                  paste0("TonleSap_", date, "_raster_4000m_classified.rds")))

saveRDS(water_raster_2000m_classified,
        file.path(onedrive_dir,
                  "GIS",
                  paste0("TonleSap_", date, "_raster_2000m_classified.rds")))

saveRDS(water_raster_1000m_classified,
        file.path(onedrive_dir,
                  "GIS",
                  paste0("TonleSap_", date, "_raster_1000m_classified.rds")))

saveRDS(water_raster_500m_classified,
        file.path(onedrive_dir,
                  "GIS",
                  paste0("TonleSap_", date, "_raster_500m_classified.rds")))

saveRDS(water_raster_300m_classified,
        file.path(onedrive_dir,
                  "GIS",
                  paste0("TonleSap_", date, "_raster_300m_classified.rds")))

saveRDS(water_raster_masked,
        file.path(onedrive_dir,
                  "GIS",
                  paste0("TonleSap_", date, "_raster_100m_classified.rds")))


# 
# #Save each
# saveRDS(nir_raster_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_30m_classified.rds"))
# 
# saveRDS(nir_raster_150m_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_150m_classified.rds"))
# 
# saveRDS(nir_raster_300m_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_300m_classified.rds"))
# 
# saveRDS(nir_raster_600m_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_600m_classified.rds"))
# 
# saveRDS(nir_raster_1200m_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_1200m_classified.rds"))
# 
# saveRDS(nir_raster_2400m_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_2400m_classified.rds"))
# 
# saveRDS(nir_raster_4800m_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_4800m_classified.rds"))
# 
# saveRDS(nir_raster_9600m_classified, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_nir_raster_9600m_classified.rds"))
# 
# nir_colors <- c("#0077b6", "#c9cba3", "#ffe1a8", "#e26d5c", "#723d46", "#472d30")
# 
# 
# 
# par(mar= c(2,2,2,2))
# plot(nir_raster_classified, col = nir_colors, axes = FALSE)
# plot(nir_raster_150m_classified, col = nir_colors, axes = FALSE)
# plot(nir_raster_300m_classified, col = nir_colors, axes = FALSE)
# plot(nir_raster_600m_classified, col = nir_colors, axes = FALSE)
# plot(nir_raster_1200m_classified, col = nir_colors, axes = FALSE)
# plot(nir_raster_2400m_classified, col = nir_colors, axes = FALSE)
# plot(nir_raster_4800m_classified, col = nir_colors, axes = FALSE)
# plot(nir_raster_9600m_classified, col = nir_colors, axes = FALSE)



#preparing raster object to plot with geom_tile in ggplot2
r_points = rasterToPoints(water_raster_classified, spatial = TRUE)
r_df = data.frame(r_points) 
head(r_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

#preparing raster object to plot with geom_tile in ggplot2
r_300m_points = rasterToPoints(water_raster_300m_classified, spatial = TRUE)
r_300m_df = data.frame(r_300m_points)
head(r_300m_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

#preparing raster object to plot with geom_tile in ggplot2
r_500m_points = rasterToPoints(water_raster_500m_classified, spatial = TRUE)
r_500m_df = data.frame(r_500m_points)
head(r_500m_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

#preparing raster object to plot with geom_tile in ggplot2
r_1000m_points = rasterToPoints(water_raster_1000m_classified, spatial = TRUE)
r_1000m_df = data.frame(r_1000m_points)
head(r_1000m_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

#preparing raster object to plot with geom_tile in ggplot2
r_2000m_points = rasterToPoints(water_raster_2000m_classified, spatial = TRUE)
r_2000m_df = data.frame(r_2000m_points)
head(r_2000m_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

#preparing raster object to plot with geom_tile in ggplot2
r_4000m_points = rasterToPoints(water_raster_4000m_classified, spatial = TRUE)
r_4000m_df = data.frame(r_4000m_points)
head(r_4000m_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks


class_list <- rev(list(
  water_raster_300m_classified,
  water_raster_500m_classified,
  water_raster_1000m_classified,
  water_raster_2000m_classified,
  water_raster_4000m_classified
))

points_list <- rev(list(
  r_300m_points,
  r_500m_points,
  r_1000m_points,
  r_2000m_points,
  r_4000m_points
  
))

#loop through resolutions and calculate distance to land/river for every water pixel
res <- rev(c(300, 500, 1000, 2000, 4000))
res_i <- 1
for(res_i in 1:length(res)){
  time_start <- Sys.time()
  
  points <- points_list[[res_i]]
  r <- class_list[[res_i]]
  
  # create classification matrix
  reclass_df_2 <- c(0, 0.5, 0,
                    0.5, 1, 1,
                    1, Inf, 2)
  reclass_df_2
  
  reclass_m_2 <- matrix(reclass_df_2,
                        ncol = 3,
                        byrow = TRUE)
  
  reclass_m_2
  
  r_classified <- reclassify(r,
                             reclass_m_2)
  
  water_points <- points[points$VH == 1,]
  land_points <- points[points$VH == 0,]
  NA_points <- points[points$VH == 2,]
  
  
  out <- list()
  for (i in 1:nrow(water_points)) {
    # for (i in 3871:5000) {
    d <- distanceFromPoints(r_classified, water_points[i, drop = F])
    out[[i]] <- zonal(d, r_classified, min)[,2]
  }
  a <- do.call(rbind, out)
  b <- cbind(water_points, a)
  # names(b)[-1] <- paste("DistTo_", 1:6)
  
  # b$dist_to_land = apply(b@data$X1, 1, function(x) min(x, na.rm = TRUE))
  
  b$dist_to_land = b@data$X1
  
  
  spplot(b, zcol = "dist_to_land", cuts = c(600, seq(800, max(b$dist_to_land), length.out = 10)), main = paste0(res[res_i], "m-resolution"))
  
  watergrid_sp <- water_points
  
  watergrid_sp$dist_to_land <- b$dist_to_land
  
  watergrid_sp$focal_100m_500m <- raster::extract(focal_100m_500m, 
                                                   watergrid_sp, 
                                                   weights=FALSE, 
                                                   fun=mean, 
                                                   na.rm = TRUE)
  
  watergrid_sp$focal_100m_2500m <- raster::extract(focal_100m_2500m, 
                                                   watergrid_sp, 
                                                   weights=FALSE, 
                                                   fun=mean, 
                                                   na.rm = TRUE)
  
  watergrid_sf <- st_as_sf(watergrid_sp) 
  
  
  watergrid_sf$dist_to_river = st_distance(watergrid_sf, st_sf(TL_rivers))
  watergrid_sp$dist_to_river <-  as.numeric(watergrid_sf$dist_to_river)
  
  plot(watergrid_sf["dist_to_river"])
  
  gridded(watergrid_sp) <- TRUE
  
  spplot(watergrid_sp, zcol = "dist_to_land", main = paste0("Distance to land: ", res[res_i], "m-resolution"), pretty = TRUE)
  spplot(watergrid_sp, zcol = "dist_to_river", main = paste0("Distance to river: ", res[res_i], "m-resolution"), pretty = TRUE)
  spplot(watergrid_sp, zcol = "focal_100m_500m", pretty = TRUE)
  spplot(watergrid_sp, zcol = "focal_100m_2500m", pretty = TRUE)
  
  
  saveRDS(watergrid_sp, file.path(onedrive_dir, "GIS",
                                  paste0("TonleSap_", date, "_WaterGrid_", res[res_i], "m.rds")))
  
  time_end <- Sys.time()
  message(round(difftime(time_end, time_start, units = "hours"), 2), " hours to process")
  
  
}

# dist_to_land <- distanceFromPoints(r, dry_points) 




# spplot(b, zcol = "X2", cuts = c(600, seq(800, max(b$X2), length.out = 10)))


# #Raw imagery
# #Very slow
# plot1 <- ggplot(data = r_df) + 
#   geom_tile(aes(x = x, y = y, fill = as.character(layer))) + 
#   scale_fill_manual(values = nir_colors) + 
#   coord_equal() +
#   theme_bw() +
#   theme(panel.grid.major = element_blank()) +
#   xlab("Longitude") + ylab("Latitude") +
#   labs(fill = "NIR group") +
#   theme(axis.text = element_blank()) +
#   scale_x_continuous(expand = expansion(mult = c(0,0))) +
#   scale_y_continuous(expand = expansion(mult = c(0,0))) +
#   ggtitle("30m resolution: Jan 2022")
# 
# print(plot1)
# 
# ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", "NIRimagery_Jan2022_30m.png"), plot1, dpi = 500)

#500m resolution
plot3 <- ggplot(data = r_300m_df) + 
  geom_tile(aes(x = x, y = y, fill = as.character(VH))) + 
  scale_fill_manual(values = c("tan", "darkblue")) + 
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  labs(fill = "water/land") +
  theme(axis.text = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  ggtitle("300m resolution: Jan 2022")

print(plot3)

ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", 
                 paste0("SARimagery_", date, "_300m.png")), 
       plot3, dpi = 500)



#500m resolution
plot2 <- ggplot(data = r_500m_df) + 
  geom_tile(aes(x = x, y = y, fill = as.character(VH))) + 
  scale_fill_manual(values = c("tan", "darkblue")) + 
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  labs(fill = "water/land") +
  theme(axis.text = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  ggtitle("500m resolution: Jan 2022")

print(plot2)

ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", 
                 paste0("SARimagery_", date, "_500m.png")), 
       plot2, dpi = 500)


#1000m resolution
plot3 <- ggplot(data = r_1000m_df) + 
  geom_tile(aes(x = x, y = y, fill = as.character(VH))) + 
  scale_fill_manual(values = c("tan", "darkblue")) + 
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  labs(fill = "water/land") +
  theme(axis.text = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  ggtitle("1000m resolution: Jan 2022")

print(plot3)

ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", 
                 paste0("SARimagery_", date, "_1000m.png")), 
       plot2, dpi = 500)


#Make pixel layer to interpolate
# str(r_300m_points)

#using sf package
# watergrid_300m_sf <- st_as_sf(r_300m_points) %>%
#   filter(layer == 1)
# ggplot(watergrid_300m_sf) +
#   geom_sf()
# watergrid_300m <- as(watergrid_300m_sf, Class = "Spatial") 

# watergrid_600m_sp <- r_600m_points %>%
#   filter(layer == 1) %>%
#   mutate(dist_to_land = b$dist_to_land, 
#          dist_to_green = b$X2)
# 
# watergrid_300m_sp <- r_300m_points %>%
#   filter(layer == 1)
# 
# watergrid_150m_sp <- r_150m_points %>%
#   filter(layer == 1)
# 
# watergrid_30m_sp <- r_points %>%
#   filter(layer == 1)
# 
# 
# projection <- proj4string(watergrid_300m_sp)
# 
# gridded(watergrid_600m_sp) <- TRUE
# gridded(watergrid_300m_sp) <- TRUE
# gridded(watergrid_150m_sp) <- TRUE
# 
# plot(watergrid_600m_sp, col = "blue")
# plot(watergrid_300m_sp, col = "blue")
# plot(watergrid_150m_sp, col = "blue")
# 
# spplot(watergrid_600m_sp, zcol = "dist_to_land")
# spplot(watergrid_600m_sp, zcol = "dist_to_green")
# 
# 
# saveRDS(watergrid_300m_sp, file.path(onedrive_dir, "GIS",
#                                      "TonleSap_January2022_WaterGrid_300m.rds"))
# saveRDS(watergrid_150m_sp, file.path(onedrive_dir, "GIS",
#                                      "TonleSap_January2022_WaterGrid_150m.rds"))
# saveRDS(watergrid_30m_sp, file.path(onedrive_dir, "GIS",
#                                     "TonleSap_January2022_WaterGrid_30m.rds"))

