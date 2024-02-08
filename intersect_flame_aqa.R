intersect_flame_aqa <- function(points, aqa, processed_path){
  
  #intersect the data: this is taking 30+ minutes
  print("starting intersection")
  print(format(Sys.time()))
  points_aqa <- st_intersection(points, aqa)
  
  if(nrow(points) == nrow(points_aqa)){print ("Great job")
  } else if (nrow(points) > nrow(points_aqa)) {print ("More data in original, not all flame data found a polygon")
  } else if (nrow(points) < nrow(points_aqa)) {print ("More data in new file. Some flame obs intersected with multiple polygons")}
  
  #look to see if there are date_time entries with n>1
  doubles <- names(which(table(points_aqa$date_time) > 1))
  duplicates <- points_aqa[which(points_aqa$date_time %in% doubles),]
  if (nrow(duplicates)>0){print ("Points duplicated due to overlapping polygons. See duplicates for more details")}
  
  #save table with points outside of aquatic area polygons
  outside <- points[!lengths(st_intersects(points, points_aqa)),]
  
  if (length(outside) > 0){print("some flame points fall outside of aqa polygons, saving shapefile with outside points")
    st_write(outside, file.path(processed_path, "points_outside_aqa.shp"), append=FALSE)}
  # else if (length(outside)==0){print("all flame points have a polygon home")}
  
  outside_plot <- ggplot()+
    geom_sf(data=aqa, aes(geometry=geometry))+
    geom_sf(data=outside, aes(geometry=geometry), color="red")+
    theme_classic()
  
  #could figure out how to use outside data to set the axis
  zoom <- st_bbox(outside)
  
  outside_plot_zoom <- ggplot()+
    geom_sf(data=aqa, aes(geometry=geometry))+
    geom_sf(data=outside, aes(geometry=geometry), color="red")+
    xlim(c(900000, 950000))+
    ylim(c(4600000, 4670000))+
    theme_classic() 
  
  #frequency of flame points in each aquatic area type 
  aqa_count <- points_aqa %>% count(AQUA_DESC)
  
  print(unique(points_aqa$AQUA_CODE))
  
  #frequency of flame points in each named backwater
  # p2 <- points_aqa%>% filter(AQUA_CODE != "MNC", AQUA_CODE != "CB")
  # named_areas <- p2 %>% count(Name)
  
  # polygon_check_list <- list(points_aqa, outside, aqa_count, named_areas, outside_plot, outside_plot_zoom)
  # names(polygon_check_list) <- c("points_aqa", "outside", "aqa_count", "named_areas", "outside_plot", "outside_plot_zoom")
  
  polygon_check_list <- list(points_aqa, outside, aqa_count, outside_plot, outside_plot_zoom)
  names(polygon_check_list) <- c("points_aqa", "outside", "aqa_count", "outside_plot", "outside_plot_zoom")

  return(polygon_check_list)
  
  saveRDS(points_aqa, file.path(processed_path, "1_flame_intersected", paste("all_trips", "flame_intersected_aqa.rds", sep=""))) #give this a more descriptive name
}  
  
  