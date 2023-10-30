# load aquatic areas from all pools and merge
# separate by aquatic areas and save

# library(tidyverse)
# library(ggplot2)
# library(sf)
# library(dplyr)
# 
# aqa_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas"

# pools <- c('aqa_2010_alt_new', 
#            'aqa_2010_lag_new', 
#            'aqa_2010_mar_new',
#            'aqa_2010_peo_new',
#            'aqa_2010_sta_new',
#            'aqa_2011_bra_new',
#            'aqa_2011_dre_new',
#            'aqa_2011_loc_new')

merge_aquatic_areas <- function(aqa_path, processed_path){
  pool_path <- list.files(path = aqa_path, pattern = "_new")
  
  pools <- lapply(file.path(aqa_path, pool_path), st_read)
  
  # add a column here titled pool that pastes in the abbreviation of the pool
  # so that once merged, we can id which pool data are in (potential for categorical plotting)
  names(pools) <- c("alt", "lag", "mar", "peo", "sta", "bra", "dre", "loc")
  
  #merge all pools
  df <- bind_rows(pools, .id='Pool') %>%
    select(-OBJECTID)
  
  points <- readRDS(file.path(flame_path, paste(flame_file, ".rds", sep="")))
  points <- st_as_sf(points)
  points <- st_transform(points, crs=projection)
  
  points_aqa <- st_intersection(points, df)
  
  saveRDS(points_aqa, file.path(processed_path, "1_flame_intersected", paste(date, "flame_intersected_aqa.rds", sep=""))) #give this a more descriptive name

}

