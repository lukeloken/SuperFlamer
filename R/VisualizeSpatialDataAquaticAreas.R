library(dplyr)
library(sf)
library(ggmap)
library(ggplot2)
library(viridis)
library(riverdist)

home_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"

# read in aquatic area shapefile and check projection
aqa_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas/aqa_2010_lag_new"
aqa <- st_read(dsn=aqa_path, layer="aqa_2010_lag_new")
st_crs(aqa)

# transform into WGS84 to match flame data
aqa84 <- st_transform(aqa, 4326)
st_crs(aqa84)

# read in flame data and check it out
flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/Shapefiles"
geodata <- readRDS(file.path(flame_path, "Merged_Illinois_Jul_2023_Shapefile_AllData.rds"))

# transform into sf object and crop to same size as aquatic areas shapefile
geodata <- st_as_sf(geodata, coords=c("latitude", "longitude"))

bb <- st_bbox(aqa84)
geodata_crop <- st_crop(geodata, bb)
st_bbox(geodata_crop)
# note: st_bbox(aqa84) and bbox printed in head(aqa84) differ
# st_bbox(aqa84) is what works for cropping

#save objects to reload later, skip time-intensive steps every time
saveRDS(aqa84, file.path(home_path, "ProcessedObjects", "lag_aqa84.RDS"))
saveRDS(geodata_crop, file.path(home_path, "ProcessedObjects", "lag_geodata.RDS"))
# manually plot flame data on aquatic areas

ggplot() +
  geom_sf(data = aqa84, aes(fill = AQUA_DESC), alpha=0.5)+
  scale_fill_brewer("Aquatic area type", palette="Dark2")+
  geom_sf(data = geodata_crop, aes(color = CH4_Dry), alpha=0.5)+
  scale_color_continuous(type="viridis")+
  labs(title="LaGrange Pool July 2023")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.ticks=element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  theme(text = element_text(size = 8))
ggsave("./figures/LaGrange_aa_chlor.png", dpi=300, width = 6, height = 6, units = "in")

#eventually get a loop that will create maps for all (relevant/corrected) variables
######################################################################


###############
# the following code is modified from VisualizeSpatialDataGGmap.R
# but does not work
###############

maps <- readRDS(file.path(home_path, "ProcessedObjects", "lag_aqa84.RDS"))
geodata <- readRDS(file.path(home_path, "ProcessedObjects", "lag_geodata.RDS"))
output_path <- 'C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas/lag_aqa_flame_points'

names(geodata)
geodata <- geodata %>%
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
         "CHL_a_C6P_wt", 
         # "CHL_a_C6P_turb", "Fluorescein_turb",
         "Brightners_wt", "Brightners_turb",
         "Fluorescein_wt",
         "Ref_Fuel_wt", "Ref_Fuel_turb",
         "FP_Trans", "FP_GreenAlgae",
         "FP_BlueGreen", "FP_Diatoms",
         "FP_Cryptophyta", "FP_YellowSubs",
         "latitude", "longitude")

commonTheme_map <- list(
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.ticks=element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white")),
  
  # scale_colour_gradientn(colours = color.palette(n=100), 
                         # limits=a_1_99tile, oob = scales::squish),
  theme(legend.position = c(.02, .48),
        legend.justification = c(0,0), 
        legend.background = element_rect(fill = NA, colour=NA),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10), 
        legend.key.height = unit(.4, "cm"),
        legend.key.width = unit(1.2, "cm"), 
        panel.border=element_rect(fill=NA, colour="black"), 
        legend.direction="vertical"),
  guides(colour=guide_colorbar(title.position = 'top', 
                               title.hjust=0, 
                               ticks.colour = "black", 
                               ticks.linewidth = 1),
                               label.position = 'top')
)

PlotSuperFlameAquaticAreas <- function(geodata,
                                       dir,
                                       maps,
                                       bad_data = NULL,
                                       plotdiag = FALSE,
                                       legend = "lowerleft") {
  
  color.palette = colorRampPalette(c(viridis(6, begin = .1, end = .98),
                                     rev(magma(
                                       5, begin = .25, end = .98
                                     ))),
                                   bias = 1)
  
  #Identify variables in dataset to plot
  plotvars_i <- names(geodata)
  
  var_i = "specCond"
  
  for (var_i in plotvars_i) {
    
    data_i <- geodata %>% 
      select(geometry, any_of(var_i)) %>% 
      filter(!is.na(var_i))
    
    map <- ggplot() +
      geom_sf(data = maps, aes(fill = AQUA_DESC), alpha = 0.3) +
      scale_fill_brewer("Aquatic area type", palette = "Dark2")+  
      geom_sf(data = data_i, aes(color = .data[[var_i]]))+
      scale_color_continuous(type="viridis")+
      commonTheme_map
    
    print(map)
    
    ggsave(file.path(dir,
                     paste("lag_", var_i, ".png", sep="")),
           map, width = 6, height = 6, units = "in")
    
  }
}

PlotSuperFlameAquaticAreas(geodata, dir=output_path, maps)


##### Next steps: remove non-aquatic polygons
##### Isolate just the main channel polygons and flame data that falls within
##### ideally bind aquatic areas and flame datasets for easier manipulation

aqa_water <- aqa84 %>%
  filter(LAND_WATER=="Water")

main_channel <- aqa84 %>%
  filter(AQUA_CODE=='MNC')

ggplot() +
  geom_sf(data=main_channel)+
  theme_void()

outside_main <- aqa_water %>%
  filter(AQUA_CODE!="MNC")

ggplot() +
  geom_sf(data=outside_main, aes(fill=AQUA_DESC))+
  theme_void()

aqa_ifl <- aqa_water %>%
  filter(AQUA_CODE=="IFL")

ggplot() +
  geom_sf(data=aqa_ifl, aes(fill=AQUA_DESC))+
  theme_void()

aqa_trib <- aqa_water %>% 
  filter(AQUA_CODE=="TRC")

ggplot()+
  geom_sf(data=aqa_trib)+
  theme_void()

# bb_main <- st_bbox(main_channel)
# geodata_main <- st_crop(geodata, bb_main)
#this might not be the way to do this,
#crops via a rectangle rather than whether
#points fall within the polygon

flame_main <- st_intersection(geodata_crop, main_channel)
head(flame_main)

flame_outside_main <- st_intersection(geodata_crop, outside_main)
flame_outside_main <- flame_outside_main %>%
  filter(!is.na(NO3_uM))

#plot flame data in main channel
flame_main <- flame_main %>%
  filter(!is.na(NO3_uM))

ggplot() +
  geom_sf(data = aqa_trib)+
  geom_sf(data = flame_main, aes(color = NO3_uM), alpha=0.5)+
  scale_color_continuous(type="viridis")+
  labs(title="LaGrange Pool July 2023")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.ticks=element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  theme(text = element_text(size = 8))
ggsave("./figures/LaGrange_aa_NO3tribs.png", dpi=300, width = 6, height = 6, units = "in")

#plot flame data outside main channel
ggplot() +
  geom_sf(data = outside_main, aes(fill = AQUA_DESC), alpha=0.5)+
  scale_fill_brewer("Aquatic area type", palette="Dark2")+
  geom_sf(data = flame_outside_main, aes(color = NO3_uM), alpha=0.5)+
  scale_color_continuous(type="viridis")+
  labs(title="LaGrange Pool July 2023")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.ticks=element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  theme(text = element_text(size = 8))





