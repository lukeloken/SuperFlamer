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
# points <- readRDS(file.path(processed_path, "flame_intersected_aqa.rds"))
# 
# network_clean <- readRDS(file.path(spatial_dir, "IL_network.rds"))

#transforms from sp object to sf object
snap_points_to_river <- function(points, processed_path, flame_file){
  
  #separate long and lat from geometry column
  points <- points%>%
    mutate(long = unlist(map(points$geometry,1)),
           lat = unlist(map(points$geometry,2)))
  
  #snap flame data points to Illinois River network object
  print("starting snap")
  print(format(Sys.time()))
  snapped_all <- xy2segvert(points$long, points$lat, rivers=network_clean)

  points$Dist_m <- unlist(network_clean$cumuldist)[snapped_all$vert]
  # points$snapped_dist_m <- unlist(network_clean$cumuldist)[snapped_all$snapdist]

  saveRDS(points, file=file.path(processed_path, "2_flame_snapped", paste(date, "_snapped.rds", sep="")))
  
  return(points)
  
}
