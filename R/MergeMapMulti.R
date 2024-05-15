
#Make merged maps of multiple flame days

MergeMapMulti <- function(home_path, 
                          multi_merge_name, 
                          names_to_merge, 
                          name_labels = NULL,
                          maps, 
                          legend = "bottomleft", 
                          plot_title = "", 
                          dpi = 400){
  
  if(is.null(name_labels)){name_labels = names_to_merge}
  
  library(rgdal)
  library(sp)
  library(RODBC)
  library(RgoogleMaps)
  library(ggmap)
  library(riverdist)
  library(viridis)
  library(maptools)
  library(ggsn)
  # library(spdplyr)
  library(ggplot2)
  library(egg)
  
  #How many maps are there?
  seq_maps <- paste0("Maps", seq_along(maps)+1)
  
  
  if(length(seq_maps) == 0){
    message("No ggmap basemap provided")
  } else if(length(seq_maps) > 0){
    
    #Create a directory for each basemap and the shapefiles
    sapply(seq_maps, function(x) dir.create(file.path(home_path, multi_merge_name, x),
                                            showWarnings = FALSE))
    dir.create(file.path(home_path, multi_merge_name, "Shapefiles"), showWarnings = FALSE)
    
    #Read and merge shapefiles
    geo_list <- list()
    geoclean_i <- 1
    for (geoclean_i in seq_along(names_to_merge)){
      files_i <- list.files(file.path(home_path, names_to_merge[geoclean_i], "Shapefiles"), 
                            full.names = TRUE)
      file_load <- files_i[grepl("AllData.rds", files_i)]
      geo_list[[geoclean_i]] <- st_as_sf(readRDS(file_load)) %>%
        mutate(id = factor(names_to_merge[geoclean_i], names_to_merge))
      
      if(c("nn03_mg") %in% names(geo_list[[geoclean_i]])){
        names(geo_list[[geoclean_i]])[which(names(geo_list[[geoclean_i]]) == "nn03_mg")] <- "NO3_mgL"
      }
      if(c("no3_uM") %in% names(geo_list[[geoclean_i]])){
        names(geo_list[[geoclean_i]])[which(names(geo_list[[geoclean_i]]) == "no3_uM")] <- "NO3_uM"
      }
      
    }
    
    geo_sf <-  bind_rows(geo_list)
    levels(geo_sf$id) <- name_labels
    n_panels <- length(levels(geo_sf$id))
    
    vars_start <- c("id", "date_time", 
                    "latitude", "longitude",
                    "speed", "course", 
                    "extPower", "wiperPos", "barom_mmHg",
                    "temp_int", "temp_spect", "temp_lamp", "temp_logger", 
                    "depth", "water_temp")
    
    geo_merge <- as(geo_sf, Class = "Spatial") 
    geo_merge@data <- geo_merge@data %>%
      mutate(latitude = st_coordinates(geo_sf)[,2], 
             longitude = st_coordinates(geo_sf)[,1]) %>%
      select(intersect(vars_start, names(geo_sf)), 
             everything())
    
    
    
    
    # Old way. Fails if columns are mismatched
    # geo_merge <- do.call(rbind, geo_list) 
    
    geo_location <- geo_merge 
    geo_location@data <- geo_merge@data %>%
      dplyr::select(id, date_time, latitude, longitude)
    
    
    geo_chla <- geo_merge 
    geo_chla@data <- geo_merge@data %>%
      dplyr::select(date_time, temp, ODO_percent, ODO_mgL, chlor_RFU)
    
    geo_dataviz <- geo_merge 
    geo_dataviz@data <- geo_merge@data %>%
      dplyr::select(intersect(names(geo_merge@data), 
                              c("id", "date_time",
                                "latitude", "longitude",
                                "temp", "specCond",
                                "pH", "turb_FNU",
                                "ODO_percent", "ODO_mgL",
                                "chlor_RFU", "chlor_ugL",
                                "BGApc_RFU", "BGApc_ugL",
                                "NO3_mgL", "nn03_mg")))
    
    
    # geo_location <- geo_merge %>%
    #   dplyr::select(date_time)
    
    # geo_dataviz <- geo_merge %>%
    #   dplyr::select(id, date_time, 
    #                 latitude, longitude, 
    #                 temp, specCond, 
    #                 pH, turb_FNU,
    #                 ODO_percent, ODO_mgL, 
    #                 chlor_RFU, chlor_ugL,
    #                 BGApc_RFU, BGApc_ugL, 
    #                 nn03_mg)
    
    # geo_chla <- geo_merge %>%
    #   dplyr::select(id, date_time, 
    #                 temp, ODO_percent, ODO_mgL, chlor_RFU)
    # 
    
    
    writeOGR(geo_merge, dsn = file.path(home_path, multi_merge_name, "Shapefiles"),
             layer = as.character(paste(multi_merge_name, "_", "Shapefile_AllData", sep="")),
             driver="ESRI Shapefile",  verbose = FALSE, overwrite = TRUE)
    
    writeOGR(geo_location, dsn = file.path(home_path, multi_merge_name, "Shapefiles"),
             layer = as.character(paste(multi_merge_name, "_", "Shapefile_DateTime", sep="")),
             driver="ESRI Shapefile",  verbose = FALSE, overwrite = TRUE)
    
    writeOGR(geo_chla, dsn = file.path(home_path, multi_merge_name, "Shapefiles"),
             layer = as.character(paste(multi_merge_name, "_", "Shapefile_Chlor", sep="")),
             driver="ESRI Shapefile",  verbose = FALSE, overwrite = TRUE)
    
    writeOGR(geo_dataviz, dsn = file.path(home_path, multi_merge_name, "Shapefiles"),
             layer = as.character(paste(multi_merge_name, "_", "Shapefile_DataViz", sep="")),
             driver="ESRI Shapefile",  verbose = FALSE, overwrite = TRUE)
    
    
    saveRDS(geo_merge, file.path(home_path, multi_merge_name, "Shapefiles",
                                 paste0(multi_merge_name, "_", "Shapefile_AllData.rds")))
    
    saveRDS(geo_location, file.path(home_path, multi_merge_name, "Shapefiles",
                                    paste0(multi_merge_name, "_", "Shapefile_DateTime.rds")))
    
    saveRDS(geo_chla, file.path(home_path, multi_merge_name, "Shapefiles",
                                paste0(multi_merge_name, "_", "Shapefile_Chlor.rds")))
    
    saveRDS(geo_dataviz, file.path(home_path, multi_merge_name, "Shapefiles",
                                   paste0(multi_merge_name, "_", "Shapefile_DataViz.rds")))
    
    
    
    
    # color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98),
    #                                    rev(magma(5, begin=.25, end=.98))),
    #                                  bias=1)
    
    color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), 
                                       rev(magma(5, begin=.4, end=.98))), 
                                     bias=1)
    
    # Choose plot variables as of Jan 2022
    # Chosen for Flamebodia, updated for Illinois (Nov 2022)
    plotvars <- c("CH4_Dry", "CO2_Dry", 
                  "CH4uM", "CH4Sat", 
                  "CO2uM", "CO2Sat",
                  "H2O", "barom_mmHg",
                  # "no3_uM", "nn03_mg", 
                  "NO3_uM", "NO3_mgL", 
                  "abs254", "abs350",
                  "water_temp", "depth",
                  "temp", "specCond", 
                  "pH", "pressure",
                  "chlor_RFU", "chlor_ugL",
                  "ODO_percent", "ODO_mgL",
                  "BGApc_RFU", "BGApc_ugL", 
                  "turb_FNU", 
                  "fDOM_RFU", "fDOM_QSU",
                  "cdom_volt", "peakT_volt", 
                  "Turb_C6P", "CDOM_C6P", 
                  "CHL_a_C6P","Brightners",
                  "Fluorescein","Ref_Fuel",
                  "Temp_C6P",  
                  "CDOM_C6P_wt", "CDOM_C6P_turb",
                  "CHL_a_C6P_wt", 
                  "Brightners_wt", "Brightners_turb",
                  "Fluorescein_wt", 
                  "Ref_Fuel_wt", "Ref_Fuel_turb",
                  "FP_Trans", "FP_GreenAlgae",
                  "FP_BlueGreen", "FP_Diatoms",
                  "FP_Cryptophyta", "FP_YellowSubs")
    
    
    #Identify variables in dataset to plot  
    plotvars_i <- intersect(plotvars, names(geo_merge))
    
    unique_id <- length(unique(geo_merge@data$id))
    panel_rows = ceiling(unique_id/3)
    panel_cols = min(c(unique_id, 3))
    
    
    locationTheme_map<-list(
      facet_wrap(~id, ncol = 1, drop = FALSE, strip.position = "right"),
      theme(strip.background = element_rect(fill = NA), 
            strip.text = element_text(size = 20)),
      theme(
        # axis.text.x=element_blank(), 
        #     axis.text.y=element_blank(), 
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(), 
        # axis.ticks=element_blank(), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(hjust = 0.5)),
      theme(panel.border=element_rect(fill = NA, colour = 'black'))
      
    )
    bigmap <- maps[[1]]
    bb <- attr(bigmap, "bb")
    
    smallmap <- maps[[5]]
    bb_small<- attr(smallmap, "bb")
    
    if(class(smallmap)[1] != "ggmap"){
      message("ggmap plotting only accepts ggmaps objects")
    } else if (class(basemap)[1] == "ggmap"){
      
      geo_location_sub <- geo_location[(sample(1:nrow(geo_location), 
                                               nrow(geo_location)*0.1)),]
      
      
      
      #Big map
      map_location_big <- ggmap(bigmap) +
        geom_point(aes_string(x = geo_location_sub$longitude, 
                              y = geo_location_sub$latitude),
                   data = geo_location_sub@data, 
                   alpha = 1, size = 2, color = "red") +
        locationTheme_map +
        theme(strip.text = element_blank(), 
              strip.background = element_blank())
      
      
      # print(map_location_big)
      
      map_location_small <- ggmap(smallmap) +
        geom_point(aes_string(x = geo_location_sub$longitude, 
                              y = geo_location_sub$latitude),
                   data = geo_location_sub@data, 
                   alpha = 1, size = 2, color = "red") +
        locationTheme_map
      
      # print(map_location_small)
      
      map_location <- ggpubr::ggarrange(map_location_big, 
                                        map_location_small, 
                                        ncol = 2, 
                                        nrow = 1,
                                        align = "v"
                                        )
      
      # print(map_location)
      
      ggsave(file.path(home_path, multi_merge_name, 
                       paste0(multi_merge_name, '_location', ".png")), 
             map_location, 
             width = 2*3 +1.5, height = (5*3),
             units = "in", dpi = dpi)
      
      
      # 
      # geo_location_sf <- st_as_sf(geo_location_sub) %>%
      #   arrange(desc(date_time))
      # 
      # map_location_big_all <- ggmap(bigmap) +
      #   geom_sf(aes(color = id),
      #              data = geo_location_sf[1:10,])
      # 
      # print(map_location_big_all)
      # 
    }
    
    var_i = 1
    #Loop through geodata and plot each variable
    for (var_i in 1:length(plotvars_i)){
      name <- plotvars_i[var_i]
      if (is.numeric(as.data.frame(geo_merge@data)[,name]) == TRUE){
        
        a <- geo_merge[!is.na(as.data.frame(geo_merge@data)[,name]),]
        # a <- a[sample(x = nrow(a), size = nrow(a)/10),] #sample if needed
        a_range <- range(as.data.frame(a@data)[,name], na.rm = TRUE)
        a_1_99tile <- quantile(as.data.frame(a@data)[,name], probs = c(.1,.99))
        # unique_id <- length(unique(a@data$id))
        # panel_rows = ceiling(unique_id/3)
        # panel_cols = min(c(unique_id, 3))
        
        
        if (legend == "bottomleft"){
          loc <- c(.01, .04/panel_rows)
          just <- c(0, 0)
        } else if (legend == "topleft"){
          loc <- c(.01, 1 - (.04/panel_rows))
          just <- c(0, 1)
        } else if (legend == "topright"){
          loc <- c(.99, 1 - (.04/panel_rows))
          just <- c(1, 1)
        } else if (legend == "bottomright"){
          loc <- c(.98, .04/panel_rows)
          just <- c(1, 0)
        }
        
        if (nrow(a)>0){
          commonTheme_map<-list(
            facet_wrap(~id, nrow = panel_rows, drop = FALSE),
            theme(strip.background = element_rect(fill = NA)),
            theme(axis.text.x=element_blank(), 
                  axis.text.y=element_blank(), 
                  axis.title.y=element_blank(), 
                  axis.title.x=element_blank(), 
                  axis.ticks=element_blank(), 
                  plot.margin = unit(c(0.2, 0, 0, 0), "cm"), 
                  plot.title = element_text(hjust = 0.5)),
            ggtitle(plot_title),
            # scale_colour_gradientn(colours = color.palette(n=100), 
            #                        limits=range(a@data[,name], na.rm=T)),
            scale_colour_gradientn(colours = color.palette(n=100), 
                                   limits=a_1_99tile, oob = scales::squish),
            theme(legend.position = loc, 
                  legend.justification = just, 
                  legend.background = element_rect(fill = 'white', colour='black'),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10), 
                  legend.key.height = unit(.4, "cm"),
                  legend.key.width = unit(1.2, "cm"), 
                  panel.border=element_rect(fill=NA, colour='black'), 
                  legend.direction="horizontal"),
            guides(colour=guide_colorbar(title.position = 'bottom', 
                                         title.hjust=0.5, 
                                         title=name, 
                                         ticks.colour = "black", 
                                         ticks.linewidth = 1))
          )
          
          #loop through list of maps and plot
          map_i <- 1
          for (map_i in seq_along(maps)){
            basemap <- maps[[map_i]]
            bb <- attr(basemap, "bb")
            
            if(class(basemap)[1] != "ggmap"){
              message("ggmap plotting only accepts ggmaps objects")
            } else if (class(basemap)[1] == "ggmap"){
              
              #Big map
              map <- ggmap(basemap) +
                geom_point(aes_string(x = a$longitude, y = a$latitude, 
                                      colour = as.character(name)), data = a@data, 
                           alpha = .2, size=3) +
                # coord_equal() +
                # ggsn::north(x.min = bb$ll.lon, x.max = bb$ur.lon,
                #             y.min = bb$ur.lat, y.max = bb$ll.lat,
                #             symbol = 3) +
                # scalebar(x.min = bb$ll.lon, x.max = bb$ur.lon,
                #          y.min = bb$ur.lat, y.max = bb$ll.lat,
                #          dist = 20, dist_unit = "km",
                #          location = "topright",
                #          st.bottom = TRUE, st.color = "red",
                #          transform = TRUE, model = "WGS84") +
                commonTheme_map 
              
              if (grepl("CH4", name) | 
                  grepl("CO2", name) | 
                  grepl("chlor", name) | 
                  grepl("turb", name)){
                map <- map +
                  scale_colour_gradientn(colours = color.palette(n=100), 
                                         # limits=range(a@data[,name], na.rm=T), 
                                         limits=a_1_99tile, oob = scales::squish,
                                         trans = "log10")
              }
              
              # print(map)
              
              ggsave(file.path(home_path, multi_merge_name, seq_maps[map_i],
                               paste0(multi_merge_name, '_', 
                                      name, ".png")), 
                     map, 
                     width = 5*panel_cols, height = (5 * panel_rows + .5), 
                     units = "in", dpi = dpi)
              
            }
          }
          
        }
        
        
        print(name)
      }
    }
  }
}



