library(dplyr)
library(sf)
library(ggmap)
library(ggplot2)
library(viridis)

home_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"

#read in aquatic area shapefile and look at it
aqa_path <- "C:/Users/slafond-hudson/OneDrive - DOI/SLL/flame/aqa_2010_lag_new"
aqa <- st_read(dsn=aqa_path, layer="aqa_2010_lag_new")
head(aqa)
st_crs(aqa)
aqa_df <- data.frame(aqa)
head(aqa_df)

aqa_df %>%
  select(AQUA_CODE)%>%
  distinct()

#transform into WGS84 to match flame data
aqa84 <- st_transform(aqa, 4326)
st_crs(aqa84)
head(aqa84)
st_bbox(aqa84)
# -90.5729 ymin: 39.92974 xmax: -90.41426 ymax: 40.00327
# -90.57300  39.89686 -89.62102  40.63415 

#read in flame data and check it out
 flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/Shapefiles"
# flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/2023-07-18_IllinoisRiver/ProcessedData"
# geodata <- read_sf(dsn=flame_path, layer="2023-07-18_IllinoisRiver_07_ShapefileCleaned")
 # geodata <- read_sf(dsn=flame_path, layer="Merged_Illinois_Jul_2023_Shapefile_AllData")
geodata <- readRDS(file.path(flame_path, "Merged_Illinois_Jul_2023_Shapefile_AllData.rds"))
head(geodata) 
class(geodata)
st_bbox(geodata)

geodata <- st_as_sf(geodata, coords=c("latitude", "longitude"))
class(geodata)
head(geodata)

names(geodata)


# crop flame to within LaGrange pool
# xmin, ymin, xmax, ymax from from st_bbox(aqa84), 
# not from bounding box in head(aqa84)
# don't know why these differ but here we are
bb <- st_bbox(aqa84)
geodata_crop <- st_crop(geodata, bb)
st_bbox(geodata_crop)

# manually plot flame data on aquatic areas
column <- "CH4_Dry"
names <- names(geodata_crop)
column <- names[5]

ggplot() +
  geom_sf(data = aqa84, aes(fill = AQUA_DESC), alpha=0.5)+
  scale_fill_brewer("Aquatic area type", palette="Dark2")+
  geom_sf(data = geodata_crop, aes(color = .data[[column]]), alpha=0.5)+
  scale_color_continuous(type="viridis")+
  labs(title="LaGrange Pool July 2023")+
  theme_void()+
  theme(plot.title = element_text(hjust=0.5))
ggsave("./figures/LaGrange_aa_ph.png", dpi=300)

#eventually get a loop that will create maps for all (relevant/corrected) variables
######################################################################



###############
# following code is modified from VisualizeSpatialDataGGmap.R
###############
maps <- aqa84
dir <- 'C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/2023-07-18_IllinoisRiver/ProcessedData'
output_path <- 'C:/workflows/SuperFlamer/figures'  

#just pick a subset of geodata so it's not making 117 figures
names(geodata_crop)
geodata_crop <- geodata_crop %>%
  select("CH4_Dry", "CO2_Dry", 
         "CH4uM", "CH4Sat", 
         "CO2uM", "CO2Sat",
         "H2O", "barom_mmHg",
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
         "Turb_C6P", "CDOM_C6P", 
         "CHL_a_C6P","Brightners",
         "Fluorescein","Ref_Fuel",
         "Temp_C6P",  
         "CDOM_C6P_wt", "CDOM_C6P_turb",
         "CHL_a_C6P_wt", "CHL_a_C6P_turb",
         "Brightners_wt", "Brightners_turb",
         "Fluorescein_wt", "Fluorescein_turb",
         "Ref_Fuel_wt", "Ref_Fuel_turb",
         "FP_Trans", "FP_GreenAlgae",
         "FP_BlueGreen", "FP_Diatoms",
         "FP_Cryptophyta", "FP_YellowSubs",
         "latitude", "longitude")


PlotSuperFlameAquaticAreas <- function(geodata,
                                dir,
                                maps,
                                bad_data = NULL,
                                plotdiag = FALSE,
                                legend = "lowerleft") {
  library(rgdal)
  library(sp)
  library(RODBC)
  library(RgoogleMaps)
  library(ggmap)
  library(riverdist)
  library(viridis)
  
  
  color.palette = colorRampPalette(c(viridis(6, begin = .1, end = .98),
                                     rev(magma(
                                       5, begin = .25, end = .98
                                     ))),
                                   bias = 1)
  
  #Identify variables in dataset to plot
  plotvars_i <- names(geodata)
  
  var_i = 1
  #Loop through geodata and plot each variable
  
  for (var_i in 1:length(plotvars_i)) {
    name <- plotvars_i[var_i]
    print(name)
  #   if (is.numeric(geodata[, name]) == TRUE) {
  #     a <- geodata[!is.na(geodata[, name]), ]
  #     a_range <- range(as.data.frame(a)[, name], na.rm = TRUE)
  #     a_1_99tile <-
  #       quantile(as.data.frame(a)[, name], probs = c(.1, .99))
  # 
  #     print(a_range)
  #     print(a_1_99tile)
  # 
  #   }
  # 
  # 
  # if (nrow(a) > 0) {
  #   if (legend == "lowerleft") {
  #     loc <- c(.02, .04)
  #   } else if (legend == "topleft") {
  #     loc <- c(.02, .85)
  #   }
  #   
  #   commonTheme_map <- list(
  #     theme(
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.title.y = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.ticks = element_blank(),
  #       plot.margin = unit(c(0, 0, 0, 0), "cm")
  #     ),
  #     scale_colour_gradientn(
  #       colours = color.palette(n = 100),
  #       limits = a_1_99tile,
  #       oob = scales::squish
  #     ),
  #     scale_fill_brewer(
  #       "Aquatic area type",
  #       palette = "Dark2"
  #     ),
  #     theme(
  #       legend.position = loc,
  #       legend.justification = c(0, 0),
  #       legend.background = element_rect(fill = 'white', colour =
  #                                          'black'),
  #       legend.text = element_text(size = 8),
  #       legend.title = element_text(size = 10),
  #       legend.key.height = unit(.4, "cm"),
  #       legend.key.width = unit(1.2, "cm"),
  #       panel.border = element_rect(fill = NA, colour = 'black'),
  #       legend.direction = "horizontal"
  #     ),
  #     guides(
  #       colour = guide_colorbar(
  #         title.position = 'bottom',
  #         title.hjust = 0.5,
  #         title = name,
  #         ticks.colour = "black",
  #         ticks.linewidth = 1
  #       )
  #     )
  #   )
  #   
  #   
    # when looping, specifying the scale_color results in error
  "discrete value supplied to continuous scale"
  but in manual plotting above, aqa can be specified with discrete fill
  and flame data specified with a continuous color gradient
  map <- ggplot() +
    geom_sf(data = maps, aes(fill = AQUA_DESC), alpha = 0.5) +
    scale_fill_brewer("Aquatic area type", palette = "Dark2") +
    geom_sf(data = geodata,
            aes(color = .data[[name]]),
            alpha = 0.5)+
    scale_color_continuous(type="viridis")+
    theme_void()#
    scale_color_gradientn(colors=color.palette(n=100), limits = a_1_99tile,
                          oob = scales::squish)
    # 
    # if (grepl("CH4", name) |
    #     grepl("chlor", name) |
    #     grepl("turb", name) | grepl("BlueGreen", name)) {
    #   if (min(a_1_99tile) > 0) {
    #     map <- map +
    #       scale_colour_gradientn(
    #         colours = color.palette(n = 100),
    #         # limits=range(a@data[,name], na.rm=T),
    #         limits = a_1_99tile,
    #         oob = scales::squish,
    #         trans = "log10"
    #       )
    #   }
    # }
    
     # print(map)
    
    # ggsave(file.path(dir,
    #                  paste(name, ".png", sep="")),
    #        map, width = 6, height = 6, units = "in")
    } 
  # }
}

        
        # print(name)

PlotSuperFlameAquaticAreas(geodata=geodata_crop, dir=output_path, maps = aqa84)



###################################################################
  

map <- ggmap(basemap) +
  geom_point(aes_string(x = a_clip$coords.x1, y = a_clip$coords.x2, 
                        colour = (a@data$CO2uM)), data = a@data, 
             alpha = .2, size=3) +
  commonTheme_map 
map

head(a@data)

bbox(aqa84)
bbox(a)
a_clip <- a[aqa84, ]

#i don't think this will work unless I join the data together

map2 <- ggmap(basemap) +
  geom_point(aes_string(x = a_clip$coords.x1, y = a_clip$coords.x2, 
                        colour = (a_clip@data$CO2uM)), data = a_clip@data, 
             alpha = .2, size=3) +
  geom_polygon(aes(x=aqa84$coords.x1, y=aqa84$coords.x2))+
  commonTheme_map 
map2
