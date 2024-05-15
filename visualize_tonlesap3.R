
library(raster)
library(sf)
library(sp)
library(rdgal)
library(rgeos)
library(dplyr)
library(ggplot2)
# library(spdplyr)
library(gstat)
library(geoR)
library(spatstat)
library(viridis)
library(rangemap)
library(e1071)
library(evd)
library(caret)
library(doParallel)
library(car)
library(gstat)
library(stringr)
library(lubridate)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map, epsg = NULL) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  if (is.null(epsg)){
    epsg = 3857
    message("setting epsg to 3857")
  }
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_epsg <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), epsg))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_epsg["ymin"]
  attr(map, "bb")$ll.lon <- bbox_epsg["xmin"]
  attr(map, "bb")$ur.lat <- bbox_epsg["ymax"]
  attr(map, "bb")$ur.lon <- bbox_epsg["xmax"]
  map
}


# gis_dir <- "P:/0368/GIS"
#folder with gis layers
gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"

#folder with flame data
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

MAP_dir <- file.path(onedrive_dir, "GIS", "MAP")
# list.files(MAP_dir)



color.palette = colorRampPalette(c(viridis(6, begin=.02, end=.98), 
                                   rev(magma(5, begin=.5, end=.98))), 
                                 bias=1)


flame_dates <- c("Merged_TonleSap_Jan_2022", 
                 "Merged_TonleSap_Apr_2022",
                 "Merged_TonleSap_Sep_2022",
                 "Merged_TonleSap_Jan_2023")



#Set the file number so that the corect MAP and flame data are referneced. 

res = "500m"
extent = "20km"

water_raster_classified <- readRDS(file.path(MAP_dir,
                                             # "GIS",
                                             paste0("TonleSap_",
                                                    "2022-03-29", 
                                                    "_raster_", res, "_classified_", extent, "_buffer.rds")))



r <- reclassify(water_raster_classified, cbind(-Inf, Inf, 1))
pp <- rasterToPolygons(r, dissolve=TRUE)


projection = proj4string(water_raster_classified)

TL_admin <- st_read(file.path(gis_dir, "Mekong"), "TonleSapLakeBoundary") %>%
  st_transform(projection)

TLC_admin <- st_read(file.path(gis_dir, "Mekong"), "TonleChamarPolygon") %>%
  st_transform(projection)


#load Tonle Sap Rivers
TL_rivers <- st_read(file.path(gis_dir, "Mekong"), "TonleSapRiverInputs2") %>%
  st_combine() %>%
  st_transform(projection)

TL_union <- readRDS(file.path(onedrive_dir, 
                              "GIS",
                              "TL_union.rds"))



# look at the results
plot(water_raster_classified)
plot(pp, lwd=3, border='blue', add=TRUE)

pp_sf <- as(pp, "sf")



water_grid_list <- list()
titles <- c()
file_nu <- 4
for (file_nu in 1:length(flame_dates)){
  
  flame_date_i <- flame_dates[file_nu]
  title_i <- paste(unlist(strsplit(flame_date_i, "_"))[3:4], collapse = " ")
  titles[file_nu] <- title_i
  
  
  data_dir <- file.path(onedrive_dir,  
                        "Data", 
                        flame_date_i)
  
  
  watergrid_sf <- readRDS(file.path(data_dir, "Shapefiles", paste0(flame_date_i, "_", res, "_", extent, "_Predict_sf.rds")))
  
  water_grid_list[[file_nu]] <- watergrid_sf
  names(water_grid_list)[file_nu] <- title_i
}


river_list <- list()
titles <- c()
file_nu <- 4
for (file_nu in 1:length(flame_dates)){
  
  flame_date_i <- flame_dates[file_nu]
  title_i <- paste(unlist(strsplit(flame_date_i, "_"))[3:4], collapse = " ")
  titles[file_nu] <- title_i
  
  
  data_dir <- file.path(onedrive_dir,  
                        "Data", 
                        flame_date_i)
  
  river_sf <- readRDS(file.path(data_dir, "Shapefiles", paste0(flame_date_i, "_Predict_tonlesapriver_sf.rds")))
  
  river_list[[file_nu]] <- river_sf
  names(river_list)[file_nu] <- title_i
}


#establish variables to interpolate
variables <- c(
  "ODO_percent", "ODO_percent_tau",
  "ODO_mgL", "ODO_mgL_tau",
  "CH4Sat", "CH4Sat_tau",
  "CH4uM", "CH4uM_tau", 
  "CO2Sat", "CO2Sat_tau",
  "CO2uM", "CO2uM_tau" 
)



variable_labels <- list(
  expression(paste("Dissolved oxygen (%)")),
  expression(paste("Dissolved oxygen (%)")),
  expression(paste("Dissolved oxygen (mg L"^"-1", ")")), 
  expression(paste("Dissolved oxygen (mg L"^"-1", ")")), 
  expression(paste("Methane (%)")),
  expression(paste("Methane (%)")),
  expression(paste("Methane (", mu, "M)")), 
  expression(paste("Methane (", mu, "M)")), 
  expression(paste("Carbon dioxide (%)")),
  expression(paste("Carbon dioxide (%)")),
  expression(paste("Carbon dioxide (", mu, "M)")), 
  expression(paste("Carbon dioxide (", mu, "M)")))

titles <- gsub("Sep", "Oct", titles)
titles <- gsub("Jan 2022", "Falling-water (Jan 2022)", titles)
titles <- gsub("Jan 2023", "Falling-water (Jan 2023)", titles)
titles <- gsub("Apr 2022", "Low-water (Apr 2022)", titles)
titles <- gsub("Oct 2022", "High-water (Oct 2022)", titles)


g_all <- list()
var = variables[1]
for (var in variables[1:length(variables)]){
  
  
  label_i = variable_labels[[which(variables == var)]]
  
  facet_label = ifelse(grepl("CO2", var), "Carbon dioxide", 
                       ifelse(grepl("CH4", var), "Methane", 
                              ifelse(grepl("ODO", var), "Dissolved oxygen")))
  
  list_i <- lapply(water_grid_list, function(l) filter(l, !is.na(.data[[var]])) %>%
                     mutate(id = facet_label))
  
  riverlist_i <- lapply(river_list, function(l) filter(l, !is.na(.data[[var]])) %>%
                     mutate(id = facet_label))
  
  
  if(nrow(list_i[[1]]) == 0){next}
  
  range_i <- range(lapply(list_i, function(l) pull(l, .data[[var]]) %>% range()))
  
  g_list <- list()
  date_j <- 2
  title_df <- data.frame(x = Inf, y = Inf, text = titles)
  
  for (date_j in 1:length(list_i)){
    
    title_j <- titles[date_j]
    
    g_list[[date_j]] <- ggplot(list_i[[date_j]]) +
      theme_bw() + 
      theme(panel.grid = element_blank()) + 
      theme(panel.background = element_rect(fill = "gray")) +
      geom_sf(data = pp_sf, color = "NA", linewidth = 0.5, fill = "white") + 
      geom_sf(aes(color = .data[[var]]), shape = 15, size = .5) +
      # geom_sf(aes(color = .data[[var]]), data = riverlist_i[[date_j]]) + #river
      geom_sf(data = TL_admin, fill = "NA", color = "black") + 
      geom_sf(data = TLC_admin, fill = "NA", color = "black") + 
      labs(color = label_i, 
           x = "", y = "") + 
      # geom_sf(data = TL_rivers, color = "blue") +
      # scale_color_continuous(type = "viridis", trans = "log10")
      scale_color_gradientn(colors = color.palette(11), limits = range_i) +
      theme(legend.position = c(0.01, 0.01), 
            legend.justification = c(0,0), 
            legend.box.background = element_blank(), 
            legend.background = element_blank(), 
            legend.text = element_text(size = 10), 
            legend.title = element_text(size = 10), 
            axis.title = element_blank()) +
      guides(color = guide_colorbar(direction = "horizontal", 
                                    title.position = "top",
                                    label.position = "bottom",
                                    title.hjust = 0.5, 
                                    barwidth = unit(1.8, units = "in"),
                                    barheight = unit(0.2, units = "in"),
                                    frame.colour = "black")) +
      scale_x_continuous(expand = c(0.01,0.01)) + 
      scale_y_continuous(expand = c(0.01,0.01)) +
      # geom_text(data = title_df[date_j,], 
      #           aes(x = x, y = y, label = text), 
      #           hjust = 1.1, vjust = 1.5) +
      ggtitle(title_j) + 
      theme(plot.title = element_text(hjust = 0.95, vjust = -10, size = 10)) + 
      facet_grid(rows = vars(id)) +
      theme(strip.background = element_rect(fill = "NA", color = "NA"), 
            strip.text = element_text(size = 12))
    
    
    if (grepl("CH4", var) | grepl("CO2", var)){
      g_list[[date_j]] <-  g_list[[date_j]]  + 
        scale_color_gradientn(colors = color.palette(11), 
                              trans = "log10", 
                              limits = range_i)
    }
    # print(g_list[[date_j]])
    
  }
  
  g_all[[which(variables == var)]] <- g_list
  
  g_list_plot <- g_list[1:3]
  
  g_list_plot[2:length(g_list_plot) - 1] <- lapply(  g_list_plot[2:length(g_list_plot) - 1], 
                                                     function(l) l + theme(strip.text = element_blank()))
  g_list_plot[2:length(g_list_plot)] <- lapply(  g_list_plot[2:length(g_list_plot)], 
                                                 function(l) l + theme(legend.position = "none"))
  
  
  g_vari <- egg::ggarrange(plots = g_list_plot, nrow = 1)
  
  print(g_vari)
  
  ggsave(paste(onedrive_dir, "/Figures/Multipanel_Krige/", res, "_", extent, "_", var, ".png", sep=""),
         g_vari, 
         dpi = 400, width = 18, height = 6, units = "in")
  
  
}

final_vars <- c(3, 11, 7) #DO-mgL, CO2-uM, CH4-UM
g_finalvars <- list()

g_finalvars <- g_all[final_vars]
g_finallist <- list()
g_finallist[1:3] <- g_finalvars[[1]][1:3]
g_finallist[4:6] <- g_finalvars[[2]][1:3]
g_finallist[7:9] <- g_finalvars[[3]][1:3]


#Manual change some plot features for multi-panel
# g_finallist2 <- lapply(g_finallist, 
#                        function(l) l + theme(strip.text = element_text(size = 12)))

g_finallist2 <- g_finallist

#remove legend and y-axis labels from center and right columns
g_finallist2[c(2:3, 5:6, 8:9)] <- lapply(g_finallist2[c(2:3, 5:6, 8:9)], 
                                         function(l) l + theme(legend.position = "none", 
                                                               axis.text.y = element_blank()))

#remove legend and y-axis labels from center and right columns
g_finallist2[c(1:2, 4:5, 7:8)] <- lapply(g_finallist2[c(1:2, 4:5, 7:8)], 
                                         function(l) l + theme(strip.text = element_blank()))


#remove x-axis labels from upper and middle rows
#add plot title (date) on top
g_finallist2[1:6] <- lapply(g_finallist2[1:6], 
                            function(l) l + theme(axis.text.x = element_blank(), 
                                                  plot.title = element_text(hjust = 0.5, 
                                                                            vjust = 1, 
                                                                            size = 12)))

#remove date labels from lower and middle rows
g_finallist2[4:9] <- lapply(g_finallist2[4:9], 
                            function(l) l + theme(plot.title = element_blank()))



g_final <- egg::ggarrange(plots = g_finallist2, ncol = 3, left = "Latitude", bottom = "Longitude")


# g_final <- egg::ggarrange(plots = g_finallist, ncol = 3)

ggsave(paste(onedrive_dir, "/Figures/Multipanel_Krige/", res, "_", extent, "_NinePanel.png", sep=""),
       g_final, 
       dpi = 400, width = 15, height = 15, units = "in")





# ##########################
#Same thing with rivers on
# ##########################

map_Mekong_big3_color <- readRDS(file.path(onedrive_dir, 'SpatialData', 'MekongRiver_big3_color_ggmap.rds'))
map_Mekong_big3 <- readRDS(file.path(onedrive_dir, 'SpatialData', 'MekongRiver_big3_ggmap.rds'))

#background google map
epsg = 32648
map_epsg <- ggmap_bbox(map_Mekong_big3, epsg)
# map_epsg <- ggmap_bbox(map_Mekong_big3_color, epsg)


#transform everything into the same projection
pp_sf_epsg <- st_transform(pp_sf, epsg)
TL_admin_epsg <- st_transform(TL_admin, epsg)
TLC_admin_epsg <- st_transform(TLC_admin, epsg)
TLR_buffer_epsg <- st_transform(TLR_buffer, epsg) 



g_all <- list()
var = variables[11]
for (var in variables[1:length(variables)]){
  
  
  label_i = variable_labels[[which(variables == var)]]
  
  facet_label = ifelse(grepl("CO2", var), "Carbon dioxide", 
                       ifelse(grepl("CH4", var), "Methane", 
                              ifelse(grepl("ODO", var), "Dissolved oxygen")))
  
  list_i <- lapply(water_grid_list, function(l) filter(l, !is.na(.data[[var]])) %>%
                     mutate(id = facet_label))
  
  riverlist_i <- lapply(river_list, function(l) filter(l, !is.na(.data[[var]])) %>%
                          mutate(id = facet_label))
  
  
  if(nrow(list_i[[1]]) == 0){next}
  
  rangelake_i <- range(lapply(list_i, function(l) pull(l, .data[[var]]) %>% range()))
  rangeriver_i <- range(lapply(riverlist_i, function(l) pull(l, .data[[var]]) %>% range()))
  range_i <- range(c(rangelake_i, rangeriver_i))
  
  g_list <- list()
  date_j <- 2
  title_df <- data.frame(x = Inf, y = Inf, text = titles)
  
  for (date_j in 1:length(list_i)){
    
    title_j <- titles[date_j]
    

    # Transform data to EPSG 
    tonlesap_epsg <- st_transform(list_i[[date_j]], epsg)
    river_epsg <- st_transform(riverlist_i[[date_j]], epsg)

    g_list[[date_j]]  <- ggmap(map_epsg) + 
      coord_sf(crs = st_crs(epsg)) +
      scale_y_continuous(limits = c(1275000, 1480000)) +
      scale_x_continuous(limits = c(345000, 500000) ) +
      theme_bw() + 
      theme(panel.grid = element_blank()) + 
      # theme(panel.background = element_rect(fill = "gray")) +
      geom_sf(data = pp_sf_epsg, color = "black", linewidth = 0.5, linetype = "solid", fill = "NA", inherit.aes = FALSE) + 
      geom_sf(data = pp_sf_epsg, color = "white", linewidth = 0.5, linetype = "dashed", fill = "NA", inherit.aes = FALSE) + 
      geom_sf(data = TLR_buffer_epsg, color = "black", linewidth = 0.5, fill = "NA", inherit.aes = FALSE) + 
      geom_sf(data = TLR_buffer_epsg, color = "white", linetype = "dashed", linewidth = 0.5, fill = "NA", inherit.aes = FALSE) + 
      geom_sf(aes(color = .data[[var]]), data = tonlesap_epsg, shape = 15, size = .5, inherit.aes = FALSE) +
      geom_sf(aes(color = .data[[var]]), data = river_epsg, inherit.aes = FALSE, size = 0.5) + #river
      # geom_sf(data = TL_admin, fill = "NA", color = "black") + 
      # geom_sf(data = TLC_admin, fill = "NA", color = "black") + 
      labs(color = label_i, 
           x = "", y = "") + 
      # geom_sf(data = TL_rivers, color = "blue") +
      # scale_color_continuous(type = "viridis", trans = "log10")
      scale_color_gradientn(colors = color.palette(11), limits = range_i) +
      theme(legend.position = c(0.01, 0.01), 
            legend.justification = c(0,0), 
            legend.box.background = element_blank(), 
            legend.background = element_blank(), 
            legend.text = element_text(size = 10, color = "white"), 
            legend.title = element_text(size = 12, color = "white"), 
            axis.title = element_blank()) +
      guides(color = guide_colorbar(direction = "horizontal", 
                                    title.position = "top",
                                    label.position = "bottom",
                                    title.hjust = 0.5, 
                                    barwidth = unit(2.5, units = "in"),
                                    barheight = unit(0.3, units = "in"),
                                    frame.colour = "black")) +
      # scale_x_continuous(expand = c(0.01,0.01)) + 
      # scale_y_continuous(expand = c(0.01,0.01)) +
      # geom_text(data = title_df[date_j,], 
      #           aes(x = x, y = y, label = text), 
      #           hjust = 1.1, vjust = 1.5) +
      ggtitle(title_j) + 
      theme(plot.title = element_text(hjust = 0.95, vjust = -10, size = 10, color = "white")) + 
      facet_grid(rows = vars(id)) +
      theme(strip.background = element_rect(fill = "NA", color = "NA"), 
            strip.text = element_text(size = 12)) 
      
    
    # print(g_list[[date_j]] )
    
    if (grepl("CH4", var) | grepl("CO2", var)){
      g_list[[date_j]] <-  g_list[[date_j]]  + 
        scale_color_gradientn(colors = color.palette(11), 
                              trans = "log10", 
                              limits = range_i)
    }
    # print(g_list[[date_j]])
    
  }
  
  g_all[[which(variables == var)]] <- g_list
  
  g_list_plot <- g_list[1:3]
  
  g_list_plot[2:length(g_list_plot) - 1] <- lapply(  g_list_plot[2:length(g_list_plot) - 1], 
                                                     function(l) l + theme(strip.text = element_blank()))
  g_list_plot[2:length(g_list_plot)] <- lapply(  g_list_plot[2:length(g_list_plot)], 
                                                 function(l) l + theme(legend.position = "none"))
  
  
  g_vari <- egg::ggarrange(plots = g_list_plot, nrow = 1)
  
  # print(g_vari)
  
  ggsave(paste(onedrive_dir, "/Figures/Multipanel_Krige/river/", res, "_", extent, "_", var, "_river.png", sep=""),
         g_vari, 
         dpi = 400, width = 18, height = 7.5, units = "in")
  
  
}

final_vars <- c(3, 11, 7) #DO-mgL, CO2-uM, CH4-UM
g_finalvars <- list()

g_finalvars <- g_all[final_vars]
g_finallist <- list()
g_finallist[1:3] <- g_finalvars[[1]][1:3]
g_finallist[4:6] <- g_finalvars[[2]][1:3]
g_finallist[7:9] <- g_finalvars[[3]][1:3]


#Manual change some plot features for multi-panel
# g_finallist2 <- lapply(g_finallist, 
#                        function(l) l + theme(strip.text = element_text(size = 12)))

g_finallist2 <- g_finallist

#remove legend and y-axis labels from center and right columns
g_finallist2[c(2:3, 5:6, 8:9)] <- lapply(g_finallist2[c(2:3, 5:6, 8:9)], 
                                         function(l) l + theme(legend.position = "none", 
                                                               axis.text.y = element_blank()))

#remove legend and y-axis labels from center and right columns
g_finallist2[c(1:2, 4:5, 7:8)] <- lapply(g_finallist2[c(1:2, 4:5, 7:8)], 
                                         function(l) l + theme(strip.text = element_blank()))


#remove x-axis labels from upper and middle rows
#add plot title (date) on top
g_finallist2[1:6] <- lapply(g_finallist2[1:6], 
                            function(l) l + theme(axis.text.x = element_blank(), 
                                                  plot.title = element_text(hjust = 0.5, 
                                                                            vjust = 1, 
                                                                            size = 12, 
                                                                            color = "black")))

#remove date labels from lower and middle rows
g_finallist2[4:9] <- lapply(g_finallist2[4:9], 
                            function(l) l + theme(plot.title = element_blank()))

#Scalebar
scalebar <- data.frame(y = c(1280000, 1280000, 1288000, 1288000), x = c(350000, 400000, 400000, 350000))
scalecenter <- scalebar %>% 
  summarize(y = mean(y), 
            x = mean(x), 
            label = as.numeric(max(scalebar$x) - min(scalebar$x))/1000) %>%
  mutate(label = paste0(label, " km"))


g_finallist2[[2]] <- g_finallist2[[2]] + 
  geom_polygon(aes(x = x, y = y), data = scalebar, inherit.aes = FALSE, 
               fill = "white", alpha = 0.7, color = "black") + 
  geom_text(aes(x = x, y = y, label = label), 
            data = scalecenter, inherit.aes = FALSE, 
            color = "black", alpha = 0.5)


g_final <- egg::ggarrange(plots = g_finallist2, ncol = 3, left = "Latitude", bottom = "Longitude")


# g_final <- egg::ggarrange(plots = g_finallist, ncol = 3)

ggsave(paste(onedrive_dir, "/Figures/Multipanel_Krige/river/", res, "_", extent, "_NinePanel.png", sep=""),
       g_final, 
       dpi = 400, width = 12, height = 15, units = "in")






# summarize maps within lake and outside of lake

water_grid_list # flame interpolations of tonle sap
river_list # flame interpolations of tonlesap river
TL_admin # tonle sap boundary
TLC_admin # tonle chamar boundary


# sf <- water_grid_list[[1]]
water_grid_list_df <- river_data_list <- list()
sf_i <- 1
for (sf_i in 1:length(titles)){
  
  sf <- water_grid_list[[sf_i]]


  TL_points <- st_filter(sf, TL_admin) %>%
    mutate(group = "TL")
  TLC_points <- st_filter(sf, TLC_admin) %>%
    mutate(group = "TLC")
  out_points <- st_filter(sf, st_union(TL_admin), .predicate = st_disjoint) %>%
    st_filter(st_union(TLC_admin), .predicate = st_disjoint)  %>%
    mutate(group = "outside")
  river_points <- river_list[[sf_i]]  %>%
    mutate(date = titles[sf_i], 
           group = "TSR")  %>%
    st_drop_geometry()
  

  bind_i <- rbind(TL_points, TLC_points) %>%
    rbind(out_points) %>%
    st_drop_geometry() %>%
    full_join(river_points) %>%
    mutate(date = titles[sf_i])


  water_grid_list_df[[sf_i]] <- bind_i


  
}


water_df <- bind_rows(water_grid_list_df) %>%
  mutate(group = factor(group, levels = c("TL", "TLC", "outside", "TSR")), 
         date = factor(date, levels =  titles)) %>%
  filter(grepl("2022", date))

levels(water_df$group) <- c("Tonle Sap", "Tonle Chamar", "Lake Margin", "Tonle Sap River")

common_box <- list(
  theme_bw(), 
  geom_boxplot(outlier.shape = 20, outlier.size = 1, outlier.alpha = .5,
               width = 0.4, position = position_dodge(width = 0.7)), 
  scale_fill_brewer(palette = "Dark2"), 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        axis.ticks.length.x = unit(0, "in")),
  geom_vline(xintercept = seq(1.5, 3.5, 1), color = "grey"), 
  scale_x_discrete(expand = expansion(add = c(0.5,0.5))), 
  guides(fill=guide_legend(ncol=1)))

box1 <- ggplot(water_df, aes(x = group, y = ODO_mgL, fill = date)) +
  common_box + 
  labs(y = variable_labels[[3]], x = "Aquatic habitat")


box2 <- ggplot(water_df, aes(x = group, y = CO2uM, fill = date)) +
  common_box + 
  scale_y_log10() +
  labs(y = variable_labels[[11]], x = "Aquatic habitat")

box3 <- ggplot(water_df, aes(x = group, y = CH4uM, fill = date)) +
  common_box + 
  scale_y_log10() +
  labs(y = variable_labels[[7]], x = "Aquatic habitat", fill = "Date") +
  theme(legend.position = "bottom")

box_list <- list(box1 + theme(legend.position = "none", 
                              axis.title.x = element_blank(), 
                              axis.text.x = element_blank()), 
                 box2 + theme(legend.position = "none", 
                              axis.title.x = element_blank(), 
                              axis.text.x = element_blank()), 
                 box3 + theme(legend.position = "bottom", 
                              legend.title = element_blank()))

box_all <- egg::ggarrange(plots = box_list, 
                          ncol = 1)


ggsave(paste(onedrive_dir, "/Figures/Multipanel_Krige/", res, "_", extent, "_BoxSummary.png", sep=""),
       box_all, 
       dpi = 400, width = 5.6, height = 7, units = "in")


common_den <- list(
  theme_bw(), 
  geom_density(alpha = 0.4), 
  scale_fill_brewer(palette = "Dark2"), 
  theme(legend.position = c(0.01, 0.99), 
        legend.justification = c(0,1), 
        legend.box.background = element_blank(), 
        legend.background = element_blank(), 
        legend.title = element_blank()),
  scale_y_continuous(expand = expansion(mult = c(0,0.05))),
  facet_grid(rows = vars(group), scales = "free_y"), 
  theme(strip.background = element_blank()))


den1 <- ggplot(water_df, aes(x = ODO_mgL, fill = date)) +
  common_den + 
  # geom_density(,
  #              alpha = 0.4) +
  # facet_grid(rows = vars(group)) + 
  # theme_bw() +
  # scale_fill_brewer(palette = "Dark2") +
  labs(x = variable_labels[[3]], y = "Density")
  # theme(legend.position = c(0.01, 0.99), 
  #       legend.justification = c(0,1), 
  #       legend.box.background = element_blank(), 
  #       legend.background = element_blank(), 
  #       legend.title = element_blank()) +
  # scale_y_continuous(expand = expansion(mult = c(0,0.05)))


den1

den2 <- ggplot(water_df, aes(x = CO2uM, fill = date)) +
  common_den + 
  # geom_density( alpha = 0.4) +
  # facet_grid(rows = vars(group)) + 
  # theme_bw() +
  # scale_fill_brewer(palette = "Dark2") +
  labs(x = variable_labels[[11]], y = "Density") + 
  # theme(legend.position = c(0.01, 0.99), 
  #       legend.justification = c(0,1), 
  #       legend.box.background = element_blank(), 
  #       legend.background = element_blank(), 
  #       legend.title = element_blank()) +
  # theme(strip.background = element_blank()) +
  scale_x_log10()
  # scale_y_continuous(expand = expansion(mult = c(0,0.05)))


den2

den3 <- ggplot(water_df, aes(x = CH4uM, fill = date)) +
  common_den + 
  # geom_density(alpha = 0.4) +
  # facet_grid(rows = vars(group)) + 
  # theme_bw() +
  # scale_fill_brewer(palette = "Dark2") +
  labs(x = variable_labels[[7]], y = "Density") +
  # theme(legend.position = c(0.01, 0.99), 
  #       legend.justification = c(0,1), 
  #       legend.box.background = element_blank(), 
  #       legend.background = element_blank(), 
  #       legend.title = element_blank()) +
  # theme(strip.background = element_blank()) +
  scale_x_log10()
  # scale_y_continuous(expand = expansion(mult = c(0,0.05)))

den3


den_list <- list(den1 + theme(legend.position = "none", 
                              strip.text = element_blank()), 
                 
                 den2 + theme(legend.position = "bottom", 
                              legend.justification = c(0.5, 0.5), 
                              axis.title.y = element_blank(), 
                              strip.text = element_blank()), 
                 
                 den3 + theme(legend.position = "none", 
                              axis.title.y = element_blank()))

den_all <- egg::ggarrange(plots = den_list, 
                          nrow = 1, 
                          right = "Aquatic habitat")


ggsave(paste(onedrive_dir, "/Figures/Multipanel_Krige/", res, "_", extent, "_DensitySummary.png", sep=""),
       den_all, 
       dpi = 400, width = 8, height = 5, units = "in")

# den1rev <- ggplot(water_df) +
#   geom_density(aes(x = ODO_mgL, fill = group),
#                alpha = 0.4) +
#   facet_grid(rows = vars(date)) + 
#   theme_bw() +
#   scale_fill_brewer(palette = "Dark2") +
#   labs(x = variable_labels[[3]], y = "Aquatic habitat") +
#   theme(legend.position = c(0.01, 0.99), 
#         legend.justification = c(0,1), 
#         legend.box.background = element_blank(), 
#         legend.background = element_blank(), 
#         legend.title = element_blank()) +
#   theme(strip.background = element_blank())
# 
# den1rev

# 
# plot(st_geometry(out_points))
# plot(st_geometry(TL_points), col = "blue", add = TRUE)
# plot(st_geometry(TLC_points), col = "red", add= TRUE)

summary(TL_points)
summary(TLC_points)
summary(out_points)

