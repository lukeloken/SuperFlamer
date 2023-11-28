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
  flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_May_2022_Jul_2023/Shapefiles"
  flame_file <- "Merged_Illinois_May_2022_Jul_2023_Shapefile_AllData.rds"
  
  all_files <- list.files(path = aqa_path, pattern = "_new_loken.shp")
  load_files <- all_files[!grepl(".xml", all_files)]
  pools <- lapply(file.path(aqa_path, load_files), st_read)
  
  
  # add a column here titled pool that pastes in the abbreviation of the pool
  # so that once merged, we can id which pool data are in (potential for categorical plotting)
  names(pools) <- c("alt", "bra", "dre", "lag", "loc", "mar", "or1", "p26", "peo", "sta")
  
  #merge all pools
  df <- bind_rows(pools, .id='Pool') %>%
    select(-OBJECTID)
  
  points <- readRDS(file.path(flame_path, flame_file))
  points <- st_as_sf(points)
  points <- st_transform(points, crs=projection)
  
  points_aqa <- st_intersection(points, df)
  # points_out <- st_difference(points, df)
  
  saveRDS(points_aqa, file.path(processed_path, "1_flame_intersected", paste("all_trips", "flame_intersected_aqa.rds", sep=""))) #give this a more descriptive name

}
points_aqa <- readRDS(file.path(processed_path, "1_flame_intersected", "all_tripsflame_intersected_aqa.rds"))

p <- points_aqa %>%
  select(AQUA_CODE, AQUA_DESC, CH4_Dry, Pool, geometry, Name)

p2 <- p%>% filter(AQUA_CODE != "MNC", AQUA_CODE != "CB")

p3 <- p %>% filter(AQUA_CODE=="N")

aqa_count <- p %>% count(AQUA_DESC)

named_areas <- p2 %>% count(Name)

point_plot2 <- ggplot()+
  geom_sf(data=points, aes(geometry=geometry), color="red")+
  geom_sf(data=p, aes(geometry=geometry), color="black")+
  theme_classic()
print(point_plot2)

nonaquatic <- ggplot()+
  geom_sf(data=p, aes(geometry = geometry), color='black')+
  geom_sf(data=p3, aes(geometry = geometry), color="darkgreen")+
  theme_classic()
print(nonaquatic)
#looks like they're clustered at locks?