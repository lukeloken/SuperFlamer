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
  
  points_aqa <- st_intersection(points, df)
  
  saveRDS(points_aqa, file.path(processed_path, "flame_intersected_aqa.rds")) #give this a more descriptive name
  
  # #filter out specific aquatic areas and save
  # main_channel <- df %>% 
  #   filter(AQUA_CODE == "MNC")
  # 
  # saveRDS(main_channel, file.path(processed_path, "main_channel.rds"))
  # 
  # aquatic_only <- df %>%
  #   filter(LAND_WATER == "Water")
  # 
  # saveRDS(aquatic_only, file.path(processed_path, "aquatic_only.rds"))
  # 
  # trib_channel <- df %>%
  #   filter(AQUA_CODE == "TRC")
  # 
  # saveRDS(trib_channel, file.path(processed_path, "trib_channel.rds"))
  # 
  # floodplain_lakes <- df %>%
  #   filter(AQUA_CODE == c("CFSA", "IFL", "CFL"))
  # saveRDS(floodplain_lakes, file.path(processed_path, "floodplain_lakes.rds"))
}

