#loads flame data and snaps points to river network object
#save flame data with river distance calculations

# library(sf)
# library(riverdist)
# library(dplyr)
# library(tidyverse)
# 
# spatial_dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/SpatialData"
# home_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"
# flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/Shapefiles"

# Read in flame data and snap to river network object
points <- readRDS(file.path(processed_path, "flame_intersected_aqa.rds"))
# 
# network_clean <- readRDS(file.path(spatial_dir, "IL_network.rds"))

#transforms from sp object to sf object
snap_points_to_river <- function(points, projection, processed_path, flame_file){
  
  points <- st_as_sf(points)
  points <- st_transform(points, crs=projection)

  #read in Illinois River main channel file to crop IL_points
  #transform into utm 
  #should have saved as utm but didn't apparently?

  # #crop points in main channel
  # main_channel <- readRDS(file.path(processed_path, "main_channel.RDS"))
  # main_channel <- st_transform(main_channel, crs=projection)
  # points_mc <- st_intersection(points, main_channel)
  # # bb_mc <- st_bbox(main_channel)
  # 
  # #crop points in aquatic areas only (removes non-aquatic category)
  # aquatic_only <- readRDS(file.path(processed_path, "aquatic_only.rds"))
  # aquatic_only <- st_transform(aquatic_only, crs=projection)
  # points_ao <- st_intersection(points, aquatic_only)
  # # bb_ao <- st_bbox(aquatic_only)
  # 
  # #crop points in floodplain lakes only (contiguous, isolated, contiguous shallow)
  # floodplain_lakes <- readRDS(file.path(processed_path, "floodplain_lakes.rds"))
  # floodplain_lakes <- st_transform(floodplain_lakes, crs=projection)
  # points_fl <- st_intersection(points, floodplain_lakes)
  # # bb_fl <- st_bbox(floodplain_lakes)
  # 
  # #crop points in trib channels only 
  # tribs <- readRDS(file.path(processed_path, "trib_channel.rds"))
  # tribs <- st_transform(tribs, crs=projection)
  # points_tribs <- st_intersection(points, tribs)
  # 
  # # bb_tribs <- st_bbox(tribs)
  
  
  
  #separate long and lat from geometry column
  points <- points%>%
    mutate(long = unlist(map(points$geometry,1)),
           lat = unlist(map(points$geometry,2)))
  
  # points_mc <- points_mc %>%
  #   mutate(long = unlist(map(points_mc$geometry,1)),
  #          lat = unlist(map(points_mc$geometry,2)))
  # 
  # points_ao <- points_ao %>%
  #   mutate(long = unlist(map(points_ao$geometry,1)),
  #          lat = unlist(map(points_ao$geometry,2)))
  # 
  # points_fl <- points_fl %>%
  #   mutate(long = unlist(map(points_fl$geometry,1)),
  #          lat = unlist(map(points_fl$geometry,2)))
  # 
  # points_tribs <- points_tribs %>%
  #   mutate(long = unlist(map(points_tribs$geometry,1)),
  #          lat = unlist(map(points_tribs$geometry,2)))
  
  #snap flame data points to Illinois River network object
  snapped_all <- xy2segvert(points$long, points$lat, rivers=network_clean)
  # snapped_mc <- xy2segvert(points_mc$long, points_mc$lat, rivers=network_clean)
  # snapped_ao <- xy2segvert(points_ao$long, points_ao$lat, rivers=network_clean)
  # snapped_fl <- xy2segvert(points_fl$long, points_fl$lat, rivers=network_clean)
  # snapped_tribs <- xy2segvert(points_tribs$long, points_tribs$lat, rivers=network_clean)
  
  points$Dist <- unlist(network_clean$cumuldist)[snapped_all$vert]
  # points_mc$Dist <- unlist(network_clean$cumuldist)[snapped_mc$vert]
  # points_ao$Dist <- unlist(network_clean$cumuldist)[snapped_ao$vert]
  # points_fl$Dist <- unlist(network_clean$cumuldist)[snapped_fl$vert]
  # points_tribs$Dist <- unlist(network_clean$cumuldist)[snapped_tribs$vert]
  # 
  
#somehow get flame shapefile name in function so the output can be named automatically
  saveRDS(points, file=file.path(processed_path, paste(flame_file, "_all_snapped", ".rds", sep="")))
  # saveRDS(points_mc, file=file.path(processed_path, paste(flame_file, "_mc_snapped", ".rds", sep="")))
  # saveRDS(points_ao, file=file.path(processed_path, paste(flame_file, "_ao_snapped", ".rds", sep="")))  
  # saveRDS(points_fl, file=file.path(processed_path, paste(flame_file, "_fl_snapped", ".rds", sep="")))
  # saveRDS(points_tribs, file=file.path(processed_path, paste(flame_file, "_tribs_snapped", ".rds", sep="")))
}

