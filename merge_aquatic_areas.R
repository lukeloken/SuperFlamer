# load aquatic areas from all pools and merge
# separate by aquatic areas and save

library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)

  flame_file <- "Merged_Illinois_May_2022_Jul_2023_Shapefile_AllData.rds"
  
  #load GIS files from each pool
  all_files <- list.files(path = aqa_path, pattern = "_new_loken.shp")
  load_files <- all_files[!grepl(".xml", all_files)]
  pools <- lapply(file.path(aqa_path, load_files), st_read)
  
  # add a column here titled pool that pastes in the abbreviation of the pool
  # so that once merged, we can id which pool data are in (potential for categorical plotting)
  names(pools) <- c("alt", "bra", "dre", "lag", "loc", "mar", "or1", "p26", "peo", "sta")
  
  #merge all pools
  dim(df)
  
  #load flame data and set projection
  projection = "+init=epsg:26915"
  
  points <- readRDS(file.path(flame_path, flame_file))
  points <- st_as_sf(points)
  points <- st_transform(points, crs=projection)
  
  #intersect the data: this is taking 30+ minutes
  print("starting intersection")
  print(format(Sys.time()))
  points_aqa <- st_intersection(points, df)

  } else if (nrow(points) > nrow(points_aqa)) {print ("More data in original, not all flame data found a polygon")
      } else if (nrow(points) < nrow(points_aqa)) {print ("More data in new file. Some flame obs intersected with multiple polygons")}
  
  #look to see if there are date_time entries with n>1
  doubles <- names(which(table(points_aqa$date_time) > 1))
  duplicates <- points_aqa[which(points_aqa$date_time %in% doubles),]

  
  #save table with points outside of aquatic area polygons
  outside <- points[!lengths(st_intersects(points, points_aqa)),]
  

  
 outside_plot <- ggplot()+
    geom_sf(data=df, aes(geometry=geometry))+
    geom_sf(data=outside, aes(geometry=geometry), color="red")+
    theme_classic()
 
 #could figure out how to use outside data to set the axis
 zoom <- st_bbox(outside)
 
 outside_plot_zoom <- ggplot()+
   geom_sf(data=df, aes(geometry=geometry))+
   geom_sf(data=outside, aes(geometry=geometry), color="red")+
   xlim(c(900000, 950000))+
   ylim(c(4600000, 4670000))+
   theme_classic() 
 

 #frequency of flame points in each aquatic area type 
  aqa_count <- points_aqa %>% count(AQUA_DESC)

  print(unique(points_aqa$AQUA_CODE))
  

  named_areas <- p2 %>% count(Name)
  
  polygon_check_list <- list(points_aqa, outside, aqa_count, named_areas, outside_plot, outside_plot_zoom)
  names(polygon_check_list) <- c("points_aqa", "outside", "aqa_count", "named_areas", "outside_plot", "outside_plot_zoom")
  
  return(polygon_check_list)
  
  # saveRDS(points_aqa, file.path(processed_path, "1_flame_intersected", paste("all_trips", "flame_intersected_aqa.rds", sep=""))) #give this a more descriptive name

}
