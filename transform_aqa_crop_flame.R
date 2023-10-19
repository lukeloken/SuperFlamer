library(dplyr)
library(sf)

home_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"

# read in aquatic area shapefile and check projection
aqa_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas"

pools <- c('aqa_2010_alt_new', 
           'aqa_2010_lag_new', 
           'aqa_2010_mar_new',
           'aqa_2010_peo_new',
           'aqa_2010_sta_new',
           'aqa_2011_bra_new',
           'aqa_2011_dre_new',
           'aqa_2011_loc_new')

#create transformed aquatic area objects
for (pool in pools){
  aqa <- st_read(dsn=file.path(aqa_path, pool), layer=pool)
  aqa84 <- st_transform(aqa, 4326)
  saveRDS(aqa84, file.path(home_path, "ProcessedObjects", 
                           paste(pool, "84", ".RDS", sep="")))
}

# read in flame data
flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/Shapefiles"
geodata <- readRDS(file.path(flame_path, "Merged_Illinois_Jul_2023_Shapefile_AllData.rds"))

# transform into sf object and crop to same size as aquatic areas shapefile
geodata <- st_as_sf(geodata, coords=c("latitude", "longitude"))

#read in transformed aqa files
processed_path <- file.path(home_path, "ProcessedObjects")

for (pool in pools){
  aqa84 <- readRDS(file.path(processed_path, paste(pool, "84", ".RDS", sep="")))
  bb <- st_bbox(aqa84)
  geodata_crop <- st_crop(geodata, bb)
  saveRDS(geodata_crop, file.path(home_path, "ProcessedObjects", paste(pool, "_geodata.RDS", sep="")))
}