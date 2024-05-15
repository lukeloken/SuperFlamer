
#not sure which of these are necessary
library(ggsn)
library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(rgdal)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(riverdist)
library(viridis)
library(maptools)
library(spdplyr)

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMeIllinois'

home_path <- "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data"


#Illinois River basemap (May 2022)
map_big <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                    'IllinoisRiver_big_ggmap.rds'))

maps <- list(map_big)

#where to put the legend
legend <- "topleft"
#what is the name of the plot
plot_title = ""

#name of dataset to plot
merge_name <- "Merged_Illinois_May_2022"
dates_merge <- seq.Date(as.Date("2022-05-02"), as.Date("2022-05-07"), by = "day")

#where the maps are created
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)


geo_merge <- readRDS(file.path(home_path, merge_name, "Shapefiles",
                             paste0(merge_name, "_", "Shapefile_AllData.rds")))


color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), 
                                   rev(magma(5, begin=.4, end=.98))), 
                                 bias=1)

# Choose plot variables as of Jan 2022
# Chosen for Flamebodia
plotvars <- c("CH4_Dry", "CO2_Dry", "H2O",
              "CH4uM", "CH4Sat", "CO2uM", "CO2Sat",
              "no3_uM", "nn03_mg", "abs254", "abs350",
              "temp", "specCond", "pH", "pressure",
              "chlor_RFU", "ODO_percent", "ODO_mgL",
              "BGApc_RFU", "turb_FNU",
              "fDOM_RFU", "tds", 
              "depth", "cdom_volt", "peakT_volt" )


#Identify variables in dataset to plot  
plotvars_i<-intersect(plotvars, names(geo_merge))

if (legend == "bottomleft"){
  loc <- c(.02, .04)
  just <- c(0,0)
} else if (legend == "topleft"){
  loc <- c(.02, .87)
  just <- c(0,0)
} else if (legend == "topright"){
  loc <- c(.98, .87)
  just <- c(0,0)
} else if (legend == "bottomright"){
  loc <- c(.98, .04)
  just <- c(0,0)
}

var_i=12
#Loop through geodata and plot each variable
for (var_i in 1:length(plotvars_i)){
  name <- plotvars_i[var_i]
  if (is.numeric(as.data.frame(geo_merge@data)[,name]) == TRUE){
    
    a <- geo_merge[!is.na(as.data.frame(geo_merge@data)[,name]),]
    a_range <- range(as.data.frame(a@data)[,name], na.rm = TRUE)
    a_1_99tile <- quantile(as.data.frame(a@data)[,name], probs = c(.1,.99))
    if (nrow(a)>0){
      
      commonTheme_map<-list(
        theme(axis.text.x=element_blank(), 
              axis.text.y=element_blank(), 
              axis.title.y=element_blank(), 
              axis.title.x=element_blank(), 
              axis.ticks=element_blank(), 
              plot.margin = unit(c(0, 0, 0, 0), "cm"), 
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
          
          ggsave(file.path(home_path, merge_name, seq_maps[map_i],
                           paste0(merge_name, '_', min(dates_merge),
                                  "_to_", max(dates_merge),
                                  "_", name, ".png")), 
                 map, width = 6, height = 6.1, units = "in")
          
        }
      }
      
    }
    
    
    print(name)
  }
}


