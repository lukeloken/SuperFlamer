# load aquatic areas from all pools and merge
# separate by aquatic areas and save

library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)

#inputs for Illinois River
# pattern <- "_new_loken.shp"
# pool_names <- c("alt", "bra", "dre", "lag", "loc", "mar", "or1", "p26", "peo", "sta")


merge_aquatic_area_pools <- function(aqa_path, pattern, pool_names, processed_path){
  
  #load GIS files from each pool
  all_files <- list.files(path = aqa_path, pattern = pattern)
  load_files <- all_files[!grepl(".xml", all_files)]
  pools <- lapply(file.path(aqa_path, load_files), st_read)
  
  # add a column here titled pool that pastes in the abbreviation of the pool
  # so that once merged, we can id which pool data are in (potential for categorical plotting)
  names(pools) <- pool_names
  
  #merge all pools
  aqa <- bind_rows(pools, .id='Pool')%>%
    dplyr::select(-OBJECTID)
  
  unique(aqa$AQUA_CODE)
  dim(aqa)
  
  return(aqa)
  
}
