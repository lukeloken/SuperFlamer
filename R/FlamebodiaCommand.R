# ##########################################################################
# Flame Command
# Set data diretory and run all flame scripts
# 1) Data files (.dat) from superflame computer (e.g., Site_Date_GPS.dat)
# 2) meta tables (.csv) from access (e.g., FlameMetaDate.csv)
# ##########################################################################

#Clean environment and connections
rm(list=ls())
closeAllConnections()

library(ggsn)

# If you need to install sensorQC use this command
# install.packages("sensorQC", repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"), dependencies = TRUE)


# dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'
# dir<-'C:/Dropbox/FLAMe_2018/Data/2018-06-27_WhiteBirchLake'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-14_TonleSapNorthShore'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-15_TonleSapNorthBasin'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-16_TonleSapNorthBasin2'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-17_TonleSapNorthBasin3'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-18_TonleSapCentral'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-19_TonleSapLower'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-21_TonleSapLowertoPP'

list.files('C:/Users/lloken/OneDrive - DOI/Flamebodia/Data')

# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-18_BasaacRver'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-19_MekongRiverPhnomPenhDown'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-20_MekongRiverToKampongCham'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-21_MekongRiverToKratie'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-22_MekongRiverToSambur'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-23_MekongRiverSamburToStungTrung'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-24_MekongRiverStrungTrengTo3S'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-25_MekongRiverStrungTrengToRonkel'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-25_MekongRiverStrungTrengToRonkel2'

dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-04_MekongRiverPhnomPehn'
dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-05_TonleSapRiver'
dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-06_TonleSapLakeTonleChamar'
dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-07_TonleSapLakeLower'
dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-08_TonleSapCenter'
dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-09_TonleSapUpper'

list.files(dir)

source('R/RunAllSuperFlameScripts.R')

#Choose gg basemap
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

map_big <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
                                     'TonleSap2_ggmap.rds'))
map_upper <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
                                      'TonleSap_upper_ggmap.rds'))
map_confluence <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                           'TonleSap_confluence_ggmap.rds'))
map_lower <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
                                      'TonleSap_lower_ggmap.rds'))

#Mekong River (Feb 2022)
# map_Mekong_big <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                       'MekongRiver_big_ggmap.rds'))
# map_Mekong_south <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                            'MekongRiver_south_ggmap.rds'))
# map_Mekong_middle1 <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                            'MekongRiver_middle1_ggmap.rds'))
# map_Mekong_middle2 <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                            'MekongRiver_middle2_ggmap.rds'))
# map_Mekong_north <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                            'MekongRiver_north_ggmap.rds'))


# median_long <- median(coordinates(a)[,1], na.rm = TRUE)
# median_lat <- median(coordinates(a)[,2], na.rm = TRUE)
# 
# if(median_lat > 12.75){map_small <- map_upper}
# if(median_lat <= 12.75 & median_lat > 12.3){map_small <- map_lower}
# if(median_lat <= 12.3){map_small <- map_confluence}

#Select which maps to use
# maps <- list(map_Mekong_big, map_Mekong_south)
maps <- list(map_big, map_upper, map_lower, map_confluence)


# plotdiag=F will turn off sensorQC plotting (outlier flags)
RunSuperFlame(dir, maps = maps, plotdiag = TRUE)

# #######################################
# Merge shapefiles and make a single plot
# #######################################
home_path <- "C:/Users/lloken/OneDrive - DOI/Flamebodia/Data"

dates_merge <- seq.Date(as.Date("2022-04-04"), as.Date("2022-04-09"), by = "day")

directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_merge <- directories_all[directories_dates]

# maps <- list(map_big, map_upper, map_lower, map_confluence, 
#              map_Mekong_big, map_Mekong_south, map_Mekong_middle1, 
#              map_Mekong_middle2, map_Mekong_north)

maps <- list(map_big, map_upper, map_lower, map_confluence)

merge_name <- "MergedMaps_2022April"
# merge_name <- "MergedMaps_2022Jan"
MergeMapTonleSapAll(home_path, directories_merge, merge_name, maps)

# copy of ggmap function. Should make own function


MergeMapTonleSapAll <- function(home_path, 
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
    dir.create(file.path(home_path, merge_name), showWarnings = FALSE)
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



