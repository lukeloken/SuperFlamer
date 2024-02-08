library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(riverdist)
library(ipdw)
library(spatstat)

processed_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/ProcessedObjects"
spatial_dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/SpatialData"
flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/Shapefiles"
flame_file <- "Merged_Illinois_Jul_2023_Shapefile_AllData"
aqa_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas"

network_clean <- readRDS(file.path(spatial_dir, "river_network.rds"))
points <- readRDS(file=file.path(processed_path, paste(flame_file, "_all_snapped", ".rds", sep="")))

#pull out distances from river network object and create a dataframe
predict_df <- data.frame(network_clean$cumuldist[[1]])
names(predict_df) <- c("dist")

#create a matrix with all the predicted point distances from each other
dist_matrix <- as.matrix(stats::dist(predict_df$dist, upper = TRUE))


###################################################
## Trying out ipdw package

#first create object from all aquatic areas merged.
pool_path <- list.files(path = aqa_path, pattern = "_new")

pools <- lapply(file.path(aqa_path, pool_path), st_read)

# add a column here titled pool that pastes in the abbreviation of the pool
# so that once merged, we can id which pool data are in (potential for categorical plotting)
names(pools) <- c("alt", "lag", "mar", "peo", "sta", "bra", "dre", "loc")

#merge all pools
df <- bind_rows(pools, .id='Pool') %>%
  dplyr::select(-OBJECTID)

saveRDS(df, file.path(processed_path, "aquatic_areas_all.rds"))


#polygon file
aqa <- readRDS(file.path(processed_path, "aquatic_areas_all.rds"))
#points file
points <- readRDS(file=file.path(processed_path, paste(flame_file, "_all_snapped", ".rds", sep="")))

#create cost raster-whoops, this takes all day unless you specify the resolution
costras <- costrasterGen(points, aqa, extent = "points", projstr = projection(aqa), res=100)

W <- owin(range(c(st_bbox(points)["xmin"], st_bbox(points)["xmax"])),
          range(c(st_bbox(points)["ymin"], st_bbox(points)["ymax"])))

IL.pp <- ppp(st_coordinates(points)[,1], st_coordinates(points)[,2], window=W)
mean.neighdist <- mean(nndist(IL.pp))

gridsize <- mean.neighdist*2
grainscale.fac <- gridsize/res(costras)[1]
gridras <- (aggregate(costras, fact = grainscale.fac))

paramlist <-c("CH4_Dry")
il_ipdw <- ipdw(points, costras)