
#Make merged maps of multiple flame days

MergeMap <- function(home_path, 
                     directories_merge,
                     merge_name,
                     maps){
  
  library(rgdal)
  library(sp)
  library(RODBC)
  library(RgoogleMaps)
  library(ggmap)
  library(riverdist)
  library(viridis)
  library(maptools)
  library(ggsn)
  
  #How many maps are there?
  seq_maps <- paste0("Maps", seq_along(maps)+1)
  
  
  if(length(seq_maps) == 0){
    message("No ggmap basemap provided")
  } else if(length(seq_maps) > 0){
    
    #Create a directory for each basemap
    sapply(seq_maps, function(x) dir.create(file.path(home_path, merge_name, x),
                                            showWarnings = FALSE))
    
    #Read and merge shapefiles
    geo_list <- list()
    geoclean_i <- 1
    for (geoclean_i in seq_along(directories_merge)){
      files_i <- list.files(file.path(home_path, directories_merge[geoclean_i], "ProcessedData"), 
                            full.names = TRUE)
      file_load <- files_i[grepl("geoclean.rds", files_i)]
      geo_list[[geoclean_i]] <- readRDS(file_load)
    }
    
    # geo_merge <-  rbind(geo_list[[1]], geo_list[[2]])
    # 
    geo_merge <- do.call(rbind, geo_list) 
    
    
    color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), 
                                       rev(magma(5, begin=.25, end=.98))), 
                                     bias=1)
    
    # Choose plot variables as of Jan 2022
    # Chosen for Flamebodia
    plotvars <- c("CH4_Dry", "CO2_Dry", "H2O",
                  "CH4uM", "CH4Sat", "CO2uM", "CO2Sat",
                  "no3_uM", "abs254", "abs350",
                  "temp", "specCond", "pH", "pressure",
                  "chlor_RFU", "ODO_percent", "ODO_mgL",
                  "BGApc_RFU", "turb_FNU",
                  "fDOM_RFU", "tds", 
                  "depth", "cdom_volt", "peakT_volt" )
    
    
    #Identify variables in dataset to plot  
    plotvars_i<-intersect(plotvars, names(geo_merge))
    
    
    var_i=1
    #Loop through geodata and plot each variable
    for (var_i in 1:length(plotvars_i)){
      name<-plotvars_i[var_i]
      if (is.numeric(geo_merge@data[,name])==TRUE){
        a <- geo_merge[!is.na(geo_merge@data[,name]),]
        a_range <- range(a@data[,name], na.rm = TRUE)
        a_1_99tile <- quantile(a@data[,name], probs = c(.1,.99))
        if (nrow(a)>0){
          
          commonTheme_map<-list(
            theme(axis.text.x=element_blank(), 
                  axis.text.y=element_blank(), 
                  axis.title.y=element_blank(), 
                  axis.title.x=element_blank(), 
                  axis.ticks=element_blank(), 
                  plot.margin = unit(c(0, 0, 0, 0), "cm")),
            # scale_colour_gradientn(colours = color.palette(n=100), 
            #                        limits=range(a@data[,name], na.rm=T)),
            scale_colour_gradientn(colours = color.palette(n=100), 
                                   limits=a_1_99tile, oob = scales::squish),
            theme(legend.position = c(.02, .08), 
                  legend.justification = c(0,0), 
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
              
              if (grepl("CH4", name) | grepl("chlor", name) | grepl("turb", name)){
                map <- map +
                  scale_colour_gradientn(colours = color.palette(n=100), 
                                         # limits=range(a@data[,name], na.rm=T), 
                                         limits=a_1_99tile, oob = scales::squish,
                                         trans = "log10")
              }
              
              # print(map)
              
              ggsave(file.path(home_path, merge_name, seq_maps[map_i],
                               paste0(merge_name, '_', min(dates_merge),
                                      "_to_", max(dates_merge),
                                      "_", name, ".png")), 
                     map, width = 6, height = 6, units = "in")
              
            }
          }
          
        }
        
        
        print(name)
      }
    }
  }
}



