
#Explore the tonle sap rasters of NIR imagery. 
#First data is from January 2022

library(raster)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(spdplyr)
library(qlcMatrix)

#Load wetted perimeter raster
gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

#Load the admin boundaries for Tonle Sap and Tonle Chamar
#load using sf
TLS_watershed <- st_read(file.path(gis_dir, "Mekong"), "TonleSapWatershed_Polygon")
TLS_outline <- st_read(file.path(gis_dir, "Mekong"), "TonleSapLakeBoundary")
TLC_outline <- st_read(file.path(gis_dir, "Mekong"), "TonleChamarPolygon")

TL_rivers <- st_read(file.path(gis_dir, "Mekong"), "TonleSapRiverInputs2") %>%
  st_combine()

# #load the NIR raster
nir_raster <- raster(file.path(gis_dir, "TonleSap_FromAlan",
                               "LC08_L1TP_126051_20220102_20220106_02_T1_B5_MERGED.tif"))
# names(nir_raster) <- "layer"
# 
# nir_raster <- reclassify(nir_raster, cbind(-Inf, 0.000001, NA), right = FALSE)
# # nir_raster <- clamp(nir_raster, lower = 0.0000001)
# names(nir_raster) <- "layer"

common_crs <- crs(nir_raster)

TL_rivers <- st_transform(TL_rivers, common_crs)

rm(nir_raster)

#Make a buffer around lakes to use for subsetting raster (using 20 km)
TLS_outline_buffer <- st_buffer(TLS_outline, dist = 1000*20)
TLC_outline_buffer <- st_buffer(TLC_outline, dist = 1000*20)
TL_union <- st_union(TLS_outline_buffer, TLC_outline_buffer)

plot(st_geometry(TLS_watershed))
plot(st_geometry(TLS_outline_buffer), add = TRUE, fill = NA)
plot(st_geometry(TLC_outline_buffer), add = TRUE, fill = NA)
plot(st_geometry(TL_union), add = TRUE, fill = NA, border = "blue", lwd = 2)


#loop through sentinel folders and mosaic images
sar_folders <- list.files(file.path(gis_dir, "Sentinel1"))
sar_folders_group <- substr(sar_folders, 1, 8)   
unique_groups <- unique(sar_folders_group)
f <- 1
m10_list <- masked10_list <- masked30_list <- masked50_list <- masked150_list <- list()


#Step 2. Load all sentinel images, mosaic them, and mask them based on lake polygon
f = 5
for (f in 1:length(sar_folders)){
  #skip running if files already exist
  mosaic_name <- file.path(onedrive_dir, 
                           "GIS",
                           paste0(sar_folders[f], "_mosaic.rds"))
  
  mask_name <- file.path(onedrive_dir, 
                         "GIS",
                         paste0(sar_folders[f], "_mosaic_masked.rds"))
  
  
  if (file.exists(mosaic_name) & file.exists(mask_name)){
    message("Skipping: ", sar_folders[f])
            next}
  
  
  #load the 5 SAR rasters
  files_f <- list.files(file.path(gis_dir, "Sentinel1", sar_folders[f]), full.names = TRUE)
  list_f <- lapply(files_f, raster)
  # plot(list_f[[1]])
  # plot(list_f[[2]])
  
  for (i in 1:length(list_f)){
    if (i == 1){
      m10 <- list_f[[1]]
    } else if (i > 1){
      m10 <- terra::mosaic(m10, list_f[[i]], fun = max, tolerance = .5)
    }
  }
  
  # raster::plot(x = m10, col = c("white", "blue"), colNA = "black", legend = FALSE)
  # plot(TLS_watershed, add = TRUE, col = NA, border = "black")
  # 
  # m10_list[[f]] <- m10
  
  saveRDS(m10, 
          file.path(onedrive_dir, 
                    "GIS",
                    paste0(sar_folders[f], "_mosaic.rds")))
  
  #crop raster to same boundary
  m10_cropped <- crop(m10, TL_union)
  m10_masked <- mask(m10_cropped, TL_union)
  
  # plot(m10_masked, col = c("grey95", "dodgerblue"), legend = FALSE)
  # #Add features for scale
  # plot(st_geometry(TLS_outline), border = "black", add = TRUE, lwd = 2, alpha = 0.5)
  # plot(st_geometry(TLC_outline), border = "black", add = TRUE, lwd = 2, alpha = 0.5)
  # plot(st_geometry(TL_union), add = TRUE, fill = NA, border = "grey50", lwd = 2)
  # plot(st_geometry(TL_rivers), col = "black", add = TRUE, alpha = .5)
  
  saveRDS(m10_masked, 
          file.path(onedrive_dir, 
                    "GIS",
                    paste0(sar_folders[f], "_mosaic_masked.rds")))
  
  # masked10_list[[f]] <- m10_masked
  
  # #zoom out
  # m30_masked <- aggregate(m10_masked, fact = 3)
  # m50_masked <- aggregate(m10_masked, fact = 5)
  # m150_masked <- aggregate(m10_masked, fact = 15)
  
  # masked30_list[[f]] <- m50_masked
  # masked50_list[[f]] <- m50_masked
  # masked150_list[[f]] <- m150_masked
  
  rm(m10_masked, m10_cropped, m10, list_f, files_f, 
     mosaic_name, mask_name)
  
  message("Completed: ", sar_folders[f])
  
}


#Step 3. Changing order. Mosaic two files from same time period
g <- 1
final_masked_list <- list()
for (g in 1:length(unique_groups)){
  
  group_i <- unique_groups[g]
  
  folders_i <- sar_folders[which(grepl(group_i, sar_folders))]
  
  # #Load mosaic file if it exists
  # mosaic_names <- file.path(onedrive_dir, 
  #                          "GIS",
  #                          paste0(folders_i, "_mosaic.rds"))
  # 
  # if (length(mosaic_names) > 0){
  #   
  #   m10_i <- lapply(mosaic_names, readRDS)
  #   m10_both <- terra::mosaic(m10_i[[1]], 
  #                             m10_i[[2]], 
  #                             fun = "max", 
  #                             tolerance = 5)
  #   
  #   plot(m10_both)
  #   plot(m10_i[[1]])
  #   plot(m10_i[[2]])
  #   
  # }
  
  #Load masked file if it exists
  mask_names <- file.path(onedrive_dir, 
                         "GIS",
                         paste0(folders_i, "_mosaic_masked.rds"))
  
  if (length(mask_names) >0){
    
    m10_masked_i <- lapply(mask_names, readRDS)
    m10_masked_both <- terra::mosaic(m10_masked_i[[1]], 
                                     m10_masked_i[[2]], 
                                     fun = "max", 
                                     tolerance = 5)
    layout(1)
    plot(m10_masked_both, main = group_i)
    # plot(m10_masked_i[[1]])
    # plot(m10_masked_i[[2]])
    
    saveRDS(m10_masked_both, 
            file.path(onedrive_dir, 
                      "GIS",
                      paste0(group_i, "_AD_mosaic_masked.rds")))
    
    final_masked_list[[g]] <- m10_masked_both
    rm(m10_masked_both, m10_masked_i, mask_names, folders_i, group_i)
    
  }
}
names(final_masked_list) <- unique_groups


j = 1
dev.off()
panel_layout <- rbind(c(1, 2), c(3, 4))
layout(panel_layout)
for(j in 1:length(final_masked_list)){
  plot(final_masked_list[[j]], 
       legend = FALSE, 
       main = names(final_masked_list)[j])
       # col = c("grey95", "dodgerblue"))
}

    #zoom out
    m30_masked <- aggregate(m10_masked, fact = 3)
    # m50_masked <- aggregate(m10_masked, fact = 5)
    # m150_masked <- aggregate(m10_masked, fact = 15)
    
    masked10_list[[f]] <- m10_masked
    # masked30_list[[f]] <- m50_masked
    # masked50_list[[f]] <- m50_masked
    # masked150_list[[f]] <- m150_masked
  }
}


# 
# panel_layout <- rbind(c(1, 2), c(3, 4), c(5,6), c(7,8))
# layout(panel_layout)
# 
# for(j in 1:length(m_list)){
#   image(m_list[[j]])
# }

# # m1 <- mosaic(sar_list[[1]], sar_list[[2]], fun=max, tolerance = .5)
# # m1 <- mosaic(m1, sar_list[[3]], fun=max, tolerance = .5)
# # m1 <- mosaic(m1, sar_list[[4]], fun=max, tolerance = .5)
# # m1 <- mosaic(m1, sar_list[[5]], fun=max, tolerance = .5)
# # 
# # plot(m1)
# 
# saveRDS(m1, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "Oct_2022_VH_mosaic.rds"))



# # plot(nir_raster)
# plot(st_geometry(TLS_watershed), add = TRUE)
# plot(st_geometry(TLS_outline), border = "magenta", add = TRUE)
# plot(st_geometry(TLC_outline), border = "magenta", add = TRUE)
# plot(st_geometry(TL_rivers), col = "darkblue", add = TRUE)
# 


# nir_raster_cropped <- crop(nir_raster, TL_union)
# nir_raster_masked <- mask(nir_raster_cropped, TL_union)
# plot(nir_raster_masked)

# m1_cropped <- crop(m1, TL_union)
# m1_masked <- mask(m1_cropped, TL_union)
# plot(m1_masked, col = c("tan", "lightskyblue2"), legend = FALSE)
# 
# 
# # plot(st_geometry(TLS_watershed), add = TRUE)
# plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
# plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
# plot(st_geometry(TL_union), add = TRUE, fill = NA, border = "grey50", lwd = 2)
# plot(st_geometry(TL_rivers), col = "darkblue", add = TRUE)
# 
# 
# #aggregate from 30x30 resolution to a number of scales
# #Some of these will be used to identify where to predict
# #Courser scales will be used as co-predictors (how much water is around you?)
# 
# #aggregate to 150x150 (factor = 15)
# m1_150m <- aggregate(m1_masked, fact = 15)
# terra::res(m1_150m)
# plot(m1_150m)
# 
# #aggregate to 300x300 (factor = 30)
# m1_300m <- aggregate(m1_masked, fact = 30)
# terra::res(m1_300m)
# plot(m1_300m)
# 
# #aggregate to 600x600m (factor = 20)
# m1_600m <- aggregate(m1_masked, fact = 60)
# terra::res(m1_600m)
# plot(m1_600m)
# 
# #aggregate to 1000x1000m (factor = 40)
# m1_1000m <- aggregate(m1_masked, fact = 100)
# terra::res(m1_1000m)
# plot(m1_1000m)
# 
# #aggregate to 400x2400m (factor = 80)
# m1_2500m <- aggregate(m1_masked, fact = 250)
# terra::res(m1_2500m)
# plot(m1_2500m)
# 
# #aggregate to 4800x4800m (factor = 160)
# m1_5000m <- aggregate(m1_masked, fact = 500)
# terra::res(m1_5000m)
# plot(m1_5000m)
# 
# #aggregate to 9600x9600m (factor = 320)
# m1_10000m <- aggregate(m1_masked, fact = 1000)
# terra::res(m1_10000m)
# plot(m1_10000m)
# 
# #Test summarize neighborhood (300m)
# focal_300m_1500m = focal(m1_300m, w = matrix(1, nrow = 5, ncol = 5), fun = mean)
# plot(focal_300m_1500m)
# 
# focal_300m_4500m = focal(m1_300m, w = matrix(1, nrow = 15, ncol = 15), fun = mean)
# plot(focal_300m_4500m)
# 
# # focal_300m_1500m_max = focal(nir_raster_300m, w = matrix(1, nrow = 5, ncol = 5), fun = max)
# # plot(focal_300m_1500m_max)
# # 
# # focal_300m_4500m_max = focal(nir_raster_300m, w = matrix(1, nrow = 15, ncol = 15), fun = max)
# # plot(focal_300m_4500m_max)
# 
# saveRDS(TL_union, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TL_union.rds"))
# 
# 
# saveRDS(focal_300m_1500m, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_focal_300m_1500m.rds"))
# 
# saveRDS(focal_300m_4500m, 
#         file.path(onedrive_dir, 
#                   "GIS",
#                   "TonleSap_January2022_focal_300m_4500m.rds"))
# 
# 
# # #Test summarize neighborhood (150m)
# # focal_150m_1350m = focal(nir_raster_150m, w = matrix(1, nrow = 9, ncol = 9), fun = mean)
# # plot(focal_150m_1350m)
# # plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
# # plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
# # 
# # 
# # focal_150m_4350m = focal(nir_raster_150m, w = matrix(1, nrow = 29, ncol = 29), fun = mean)
# # plot(focal_150m_4350m)
# # plot(st_geometry(TLS_outline), border = "darkblue", add = TRUE, lwd = 2)
# # plot(st_geometry(TLC_outline), border = "darkblue", add = TRUE, lwd = 2)
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
# # classify values. group 1 is water
# # create classification matrix
# reclass_df <- c(0, 7360, 1,
#                 7360, 13357, 2,
#                 13357, 14239, 3, 
#                 14239, 14925, 4,
#                 14925, 15824, 5,
#                 15824, Inf, 6)
# reclass_df
# 
# reclass_m <- matrix(reclass_df,
#                     ncol = 3,
#                     byrow = TRUE)
# 
# reclass_m
# 
# nir_raster_classified <- reclassify(nir_raster_masked,
#                                     reclass_m)
# nir_raster_150m_classified <- reclassify(nir_raster_150m,
#                                          reclass_m)
# nir_raster_300m_classified <- reclassify(nir_raster_300m,
#                                          reclass_m)
# nir_raster_600m_classified <- reclassify(nir_raster_600m,
#                                          reclass_m)
# nir_raster_1200m_classified <- reclassify(nir_raster_1200m,
#                                           reclass_m)
# nir_raster_2400m_classified <- reclassify(nir_raster_2400m,
#                                           reclass_m)
# nir_raster_4800m_classified <- reclassify(nir_raster_4800m,
#                                           reclass_m)
# nir_raster_9600m_classified <- reclassify(nir_raster_9600m,
#                                           reclass_m)
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
# 
# 
# 
# #preparing raster object to plot with geom_tile in ggplot2
# r_points = rasterToPoints(nir_raster_classified, spatial = TRUE)
# r_df = data.frame(r_points) 
# head(r_df) #breaks will be set to column "layer"
# # r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks
# 
# #preparing raster object to plot with geom_tile in ggplot2
# r_150m_points = rasterToPoints(nir_raster_150m_classified, spatial = TRUE)
# r_150m_df = data.frame(r_150m_points)
# head(r_150m_df) #breaks will be set to column "layer"
# # r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks
# 
# #preparing raster object to plot with geom_tile in ggplot2
# r_300m_points = rasterToPoints(nir_raster_300m_classified, spatial = TRUE)
# r_300m_df = data.frame(r_300m_points)
# head(r_300m_df) #breaks will be set to column "layer"
# # r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks
# 
# #preparing raster object to plot with geom_tile in ggplot2
# r_600m_points = rasterToPoints(nir_raster_600m_classified, spatial = TRUE)
# r_600m_df = data.frame(r_600m_points)
# head(r_600m_df) #breaks will be set to column "layer"
# # r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks
# 
# 
# 
# class_list <- rev(list(
#   #nir_raster_classified, 
#   # nir_raster_150m_classified
#   nir_raster_300m_classified
#   # nir_raster_600m_classified
# ))
# 
# points_list <- rev(list(
#   # r_points, 
#   # r_150m_points
#   r_300m_points
#   # r_600m_points
# ))
# 
# res <- rev(c(30, 150, 300, 600))[2]
# res_i <- 1
# for(res_i in 1:length(res)){
#   time_start <- Sys.time()
#   
#   points <- points_list[[res_i]]
#   r <- class_list[[res_i]]
#   
#   # create classification matrix
#   reclass_df_2 <- c(0, 1, 1,
#                     1, 2, 2,
#                     2, Inf, 3)
#   reclass_df_2
#   
#   reclass_m_2 <- matrix(reclass_df_2,
#                         ncol = 3,
#                         byrow = TRUE)
#   
#   reclass_m_2
#   
#   r_classified <- reclassify(r,
#                              reclass_m_2)
#   
#   water_points <- filter(points, layer == 1)
#   wet_points <- filter(points, layer == 2)
#   dry_points <- filter(points, layer >= 2)
#   
#   out <- list()
#   for (i in 1:nrow(water_points)) {
#     # for (i in 3871:5000) {
#     d <- distanceFromPoints(r_classified, water_points[i, drop = F])
#     out[[i]] <- zonal(d, r_classified, min)[,2]
#   }
#   a <- do.call(rbind, out)
#   b <- cbind(water_points, a)
#   # names(b)[-1] <- paste("DistTo_", 1:6)
#   
#   b$dist_to_land = apply(b@data[,3:ncol(b@data)], 1, function(x) min(x, na.rm = TRUE))
#   
#   spplot(b, zcol = "dist_to_land", cuts = c(600, seq(800, max(b$dist_to_land), length.out = 10)), main = paste0(res[res_i], "m-resolution"))
#   
#   watergrid_sp <- water_points %>%
#     mutate(dist_to_land = b$dist_to_land, 
#            dist_to_green = b$X2)
#   
#   watergrid_sp$focal_300m_4500m <- raster::extract(focal_300m_4500m, 
#                                                    watergrid_sp, 
#                                                    weights=FALSE, 
#                                                    fun=mean, 
#                                                    na.rm = TRUE)
#   watergrid_sp$focal_300m_1500m <- raster::extract(focal_300m_1500m, 
#                                                    watergrid_sp, 
#                                                    weights=FALSE, 
#                                                    fun=mean, 
#                                                    na.rm = TRUE)
#   
#   watergrid_sf <- st_as_sf(watergrid_sp) 
#   
#   watergrid_sf$dist_to_river = st_distance(watergrid_sf, st_sf(TL_rivers))
#   watergrid_sp$dist_to_river <-  as.numeric(watergrid_sf$dist_to_river)
#   
#   plot(watergrid_sf["dist_to_river"])
#   
#   gridded(watergrid_sp) <- TRUE
#   
#   spplot(watergrid_sp, zcol = "dist_to_land", main = paste0("Distance to land: ", res[res_i], "m-resolution"), pretty = TRUE)
#   spplot(watergrid_sp, zcol = "dist_to_green", main = paste0("Distance to green: ", res[res_i], "m-resolution"), pretty = TRUE)
#   spplot(watergrid_sp, zcol = "dist_to_river", main = paste0("Distance to river: ", res[res_i], "m-resolution"), pretty = TRUE)
#   spplot(watergrid_sp, zcol = "focal_300m_1500m", pretty = TRUE)
#   spplot(watergrid_sp, zcol = "focal_300m_4500m", pretty = TRUE)
#   
#   
#   saveRDS(watergrid_sp, file.path(onedrive_dir, "GIS",
#                                   paste0(                             "TonleSap_January2022_WaterGrid_", res[res_i], "m.rds")))
#   
#   time_end <- Sys.time()
#   message(round(difftime(time_end, time_start, units = "hours"), 2), " hours to process")
#   
#   
# }
# 
# # dist_to_land <- distanceFromPoints(r, dry_points) 
# 
# 
# 
# 
# # spplot(b, zcol = "X2", cuts = c(600, seq(800, max(b$X2), length.out = 10)))
# 
# 
# # #Raw imagery
# # #Very slow
# # plot1 <- ggplot(data = r_df) + 
# #   geom_tile(aes(x = x, y = y, fill = as.character(layer))) + 
# #   scale_fill_manual(values = nir_colors) + 
# #   coord_equal() +
# #   theme_bw() +
# #   theme(panel.grid.major = element_blank()) +
# #   xlab("Longitude") + ylab("Latitude") +
# #   labs(fill = "NIR group") +
# #   theme(axis.text = element_blank()) +
# #   scale_x_continuous(expand = expansion(mult = c(0,0))) +
# #   scale_y_continuous(expand = expansion(mult = c(0,0))) +
# #   ggtitle("30m resolution: Jan 2022")
# # 
# # print(plot1)
# # 
# # ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", "NIRimagery_Jan2022_30m.png"), plot1, dpi = 500)
# 
# #300m resolution
# plot2 <- ggplot(data = r_300m_df) + 
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
#   ggtitle("300m resolution: Jan 2022")
# 
# print(plot2)
# 
# ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", "NIRimagery_Jan2022_300m.png"), plot2, dpi = 500)
# 
# 
# #150m resolution
# plot3 <- ggplot(data = r_150m_df) + 
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
#   ggtitle("150m resolution: Jan 2022")
# 
# print(plot3)
# 
# ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", "NIRimagery_Jan2022_150m.png"), plot3, dpi = 500)
# 
# 
# 
# #Make pixel layer to interpolate
# # str(r_300m_points)
# 
# #using sf package
# # watergrid_300m_sf <- st_as_sf(r_300m_points) %>%
# #   filter(layer == 1)
# # ggplot(watergrid_300m_sf) +
# #   geom_sf()
# # watergrid_300m <- as(watergrid_300m_sf, Class = "Spatial") 
# 
# # watergrid_600m_sp <- r_600m_points %>%
# #   filter(layer == 1) %>%
# #   mutate(dist_to_land = b$dist_to_land, 
# #          dist_to_green = b$X2)
# # 
# # watergrid_300m_sp <- r_300m_points %>%
# #   filter(layer == 1)
# # 
# # watergrid_150m_sp <- r_150m_points %>%
# #   filter(layer == 1)
# # 
# # watergrid_30m_sp <- r_points %>%
# #   filter(layer == 1)
# # 
# # 
# # projection <- proj4string(watergrid_300m_sp)
# # 
# # gridded(watergrid_600m_sp) <- TRUE
# # gridded(watergrid_300m_sp) <- TRUE
# # gridded(watergrid_150m_sp) <- TRUE
# # 
# # plot(watergrid_600m_sp, col = "blue")
# # plot(watergrid_300m_sp, col = "blue")
# # plot(watergrid_150m_sp, col = "blue")
# # 
# # spplot(watergrid_600m_sp, zcol = "dist_to_land")
# # spplot(watergrid_600m_sp, zcol = "dist_to_green")
# # 
# # 
# # saveRDS(watergrid_300m_sp, file.path(onedrive_dir, "GIS",
# #                                      "TonleSap_January2022_WaterGrid_300m.rds"))
# # saveRDS(watergrid_150m_sp, file.path(onedrive_dir, "GIS",
# #                                      "TonleSap_January2022_WaterGrid_150m.rds"))
# # saveRDS(watergrid_30m_sp, file.path(onedrive_dir, "GIS",
# #                                     "TonleSap_January2022_WaterGrid_30m.rds"))
# 
