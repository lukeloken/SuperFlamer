

# ##############################################################
# Load ggmaps and map your data
# Input is a spatial points data frame
# and a list of ggmaps maps
# Output are several .png images 
# ##############################################################


PlotSuperFlameGGmap<-function(geodata, dir, Date, Site, meta, maps, 
                              plotdiag = FALSE, legend = "bottomleft"){
  
  library(rgdal)
  library(sp)
  library(RODBC)
  library(RgoogleMaps)
  library(ggmap)
  library(riverdist)
  library(viridis)
  
  #How many maps are there?
  seq_maps <- paste0("Maps", seq_along(maps)+1)
  
  if(length(seq_maps) == 0){
    message("No ggmap basemap provided")
  } else if(length(seq_maps) > 0){
    
    #Create a directory for each basemap
    sapply(seq_maps, function(x) dir.create(file.path(dir, x),
                                            showWarnings = FALSE))
    
    # dir.create(file.path(dir, "Maps2"), showWarnings = F)
    # dir.create(file.path(dir, "Maps3"), showWarnings = F)
    

    color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), 
                                       rev(magma(5, begin=.25, end=.98))), 
                                     bias=1)

    # Choose plot variables as of Jan 2022
    # Chosen for Flamebodia
    plotvars <- c("CH4_Dry", "CO2_Dry", "H2O",
                  "CH4uM", "CH4Sat", "CO2uM", "CO2Sat",
                  "no3_uM", "abs254", "abs350",
                  "temp", "specCond", "pH", "pressure",
                  "ODO_percent", "ODO_mgL",
                  "chlor_RFU", "BGApc_RFU", "turb_FNU",
                  "fDOM_RFU", "tds", 
                  "depth", "cdom_volt", "peakT_volt" )
    
    #Identify variables in dataset to plot  
    plotvars_i<-intersect(plotvars, names(geodata))
    
    var_i=1
    #Loop through geodata and plot each variable
    for (var_i in 1:length(plotvars_i)){
      name<-plotvars_i[var_i]
      if (is.numeric(geodata@data[,name])==TRUE){
        a <- geodata[!is.na(geodata@data[,name]),]
        
        if (nrow(a)>0){
          
          if (legend == "lowerleft"){
            loc <- c(.02, .04)
          } else if (legend == "topleft"){
            loc <- c(.02, .85)
          }
          
          commonTheme_map<-list(
            theme(axis.text.x=element_blank(), 
                  axis.text.y=element_blank(), 
                  axis.title.y=element_blank(), 
                  axis.title.x=element_blank(), 
                  axis.ticks=element_blank(), 
                  plot.margin = unit(c(0, 0, 0, 0), "cm")),
            scale_colour_gradientn(colours = color.palette(n=100), 
                                   limits=range(a@data[,name], na.rm=T)),
            theme(legend.position = loc, 
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
          for (map_i in seq_along(maps)){
            basemap <- maps[[map_i]]
            
            if(class(basemap)[1] != "ggmap"){
              message("ggmap plotting only accepts ggmaps objects")
            } else if (class(basemap)[1] == "ggmap"){
              
              #Big map
              map <- ggmap(basemap) +
                geom_point(aes_string(x = a$longitude, y = a$latitude, 
                                      colour = as.character(name)), data = a@data, 
                           alpha = .2, size=3) +
                commonTheme_map 
              
              if (grepl("CH4", name) | grepl("chlor", name) | grepl("turb", name)){
                map <- map +
                  scale_colour_gradientn(colours = color.palette(n=100), 
                                         limits=range(a@data[,name], na.rm=T), trans = "log10")
              }
              
              # print(map)
              
              ggsave(file.path(dir, seq_maps[map_i],
                               paste(Date, '_', Site, "_", name, ".png", sep="")), 
                     map, width = 6, height = 6, units = "in")
              
            }
          }
          
        }
        
        
        print(name)
      }
    }
  }
}
