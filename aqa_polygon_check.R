library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)

#run after merge_aquatic_areas.R if intersected flame file not made yet

#paths to intersected flame data and original flame data
dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/ProcessedObjects"
flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_May_2022_Jul_2023/Shapefiles"
flame_file <- "Merged_Illinois_May_2022_Jul_2023_Shapefile_AllData.rds"

#intersected with aquatic areas polygons
points_aqa <- readRDS(file.path(dir, "1_flame_intersected", "all_tripsflame_intersected_aqa.rds"))

check_aqa_polygons <- function(points_aqa, points, aqa){
  points_aqa_short <- points_aqa %>%
    # select(date_time, geometry, AQUA_CODE)
    select(date_time, geometry)
  
  points_short <- points %>%
    select(date_time, geometry)
  
  #original flame data
  # points <- readRDS(file.path(flame_path, flame_file))
  # points <- st_as_sf(points)%>%st_transform(crs=st_crs(points_aqa))
  
  #merged aqa polygon with Luke's edits
  # aqa <- readRDS(file.path(dir, "new_aqa_loken_merged.rds"))
  
  #data in backwaters
  p2 <- points_aqa%>% filter(AQUA_CODE != "MNC", AQUA_CODE != "CB")
  
  #data in non-aquatic areas
  p3 <- points_aqa %>% filter(AQUA_CODE=="N")
  
  #points outside of aquatic areas polygons
  # p_diff <- setdiff(points$geometry, points_aqa$geometry)
  # p_diff2 <- anti_join(points_short, points_aqa_short)
  # if (p_diff null){print "geometry points are the same between original flame and intersected flame data"}
  #else if (p_diff not null){print "original flame has more points than aqa intersected flame"}
  outside <- points_aqa[!lengths(st_intersects(points, points_aqa)),]
  
  ggplot()+
    geom_sf(data=df, aes(geometry=geometry))+
    geom_sf(data=outside, aes(geometry=geometry), color="red")+
    xlim(c(749000, 960000))+
    ylim(c(4460000, 4670000))+
    theme_classic()
    
  
  
  #look to see if there are date_time entries with n>1
  doubles <- names(which(table(points_aqa$date_time) > 1))
  duplicates <- points_aqa[which(points_aqa$date_time %in% doubles),]
  if (nrow(summary2)>0){print ("Points duplicated due to overlapping polygons. See duplicates for more details")}
  
  #frequency table of flame data in each aquatic area
  aqa_count <- points_aqa %>% count(AQUA_DESC)
  print("AQUA_CODEs in points_aqa")
  print(unique(points_aqa$AQUA_CODE))
  
  #frequency table of flame data in each named backwater (and count of unnamed)
  named_areas <- p2 %>% count(Name)
  
  #plot points that fall outside of new polygons
  outside_aqa <- ggplot()+
    geom_sf(data=sf1, aes(geometry=geometry), color="black")+
    geom_sf(data=p_diff, aes(geometry=geometry), color="orangered3", size=2)+
    labs(title="points outside of aquatic areas polygons")+
    theme_classic()
  print(outside_aqa)
  
  outside_zoom <- ggplot()+
    geom_sf(data=sf1, aes(geometry=geometry), color="black")+
    geom_sf(data=p_diff, aes(geometry=geometry), color="orangered3", size=2)+
    labs(title="points outside of aquatic areas polygons")+
    xlim(c(905000, 960000))+
    ylim(c(4600000, 4670000))+
    theme_classic()
  print(outside_zoom)
  
  #plot points still classified as non-aquatic
  nonaquatic <- ggplot()+
    geom_sf(data=sf1, aes(geometry=geometry), color="black")+
    geom_sf(data=p3, aes(geometry = geometry), color="orangered3")+
    labs(title="Points classified as non-aquatic")+
    theme_classic()
  print(nonaquatic)
  
  polygon_check_list <- list(summary2, aqa_count, named_areas, outside_aqa, nonaquatic)
  names(polygon_check_list) <- c("summary2", "aqa_count", "named_areas", "outside_aqa", "nonaquatic")
  
  return(polygon_check_list)
}
