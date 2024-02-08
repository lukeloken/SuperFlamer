library(sbtools)
library(dplyr)
library(sf)
library(ggplot2)
library(terra)

source("./intersect_flame_aqa.R")

dir <- "C:/Users/slafond-hudson/OneDrive - DOI/SLL/flame/Mississippi2015"
aqa_path <- "C:/Users/slafond-hudson/OneDrive - DOI/SLL/flame/Aquatic areas"
processed_path <- "C:/Users/slafond-hudson/OneDrive - DOI/SLL/flame/Processed_objects"

projection = "+init=epsg:26915"


#load flame data and set projection
# points <- readRDS(file.path(flame_path, flame_file))
# points <- st_as_sf(points)
# points <- st_transform(points, crs=projection)

df <- read.csv(file.path(dir, "Pool8_SpatialSurfaceChemistry.csv"))

points <- st_as_sf(df, coords=c("Longitude", "Latitude"), crs="+init=epsg:4326")
points <- st_transform(points, crs=projection)
points <- points %>% mutate(ltime = as.POSIXct(ltime, tz="America/Chicago"))

aqa <- st_read(file.path(aqa_path, "aqa_2010_p08_new", "aqa_2010_p08_new.shp"))

polygon_check_list <- intersect_flame_aqa(points, aqa, processed_path)
points_aqa <- polygon_check_list[[1]]


fig <- ggplot()+
  geom_sf(data=sf, aes(color=pH))+
  theme_classic()
print(fig)

library(sbtools)
p8 <- item_get("58385188e4b0d9329c801cc6")
names(p8)
item_list_files(p8)
#how to load shapefile?

