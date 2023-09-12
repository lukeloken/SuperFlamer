library(dplyr)
library(sf)
library(ggmap)
library(ggplot2)
library(viridis)

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

# manually plot flame data on aquatic areas

ggplot() +
  geom_sf(data = aqa84, aes(fill = AQUA_DESC), alpha=0.5)+
  scale_fill_brewer("Aquatic area type", palette="Dark2")+
  geom_sf(data = geodata_crop, aes(color = CH4_Dry), alpha=0.5)+
  scale_color_continuous(type="viridis")+
  labs(title="LaGrange Pool July 2023")+
  theme_void()+
  theme(plot.title = element_text(hjust=0.5))
ggsave("./figures/LaGrange_aa_ph.png", dpi=300)

#eventually get a loop that will create maps for all (relevant/corrected) variables
######################################################################


###############
# the following code is modified from VisualizeSpatialDataGGmap.R
# but does not work
###############

maps <- aqa84
geodata <- geodata_crop
output_path <- 'C:/workflows/SuperFlamer/figures'

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
  
  color.palette = colorRampPalette(c(viridis(6, begin = .1, end = .98),
                                     rev(magma(
                                       5, begin = .25, end = .98
                                     ))),
                                   bias = 1)
  
  #Identify variables in dataset to plot
  plotvars_i <- names(geodata)
  
  # var_i = "CH4_Dry"
  
  #Loop through geodata and plot each variable
  
  for (var_i in plotvars_i) {
    # name <- var_i
    # when looping, specifying the scale_color results in error
    # "discrete value supplied to continuous scale"
    # but in manual plotting above, aqa can be specified with discrete fill
    # and flame data specified with a continuous color gradient
    map <- ggplot() +
      geom_sf(data = maps, aes(fill = AQUA_DESC), alpha = 0.5) +
      scale_fill_brewer("Aquatic area type", palette = "Dark2") +
      geom_sf(data = geodata,
              aes(color = .data[[var_i]]),
              alpha = 0.5)+
      # scale_color_continuous(type="viridis")+
      theme_void()
    
    print(map)
    
    # ggsave(file.path(dir,
    #                  paste(name, ".png", sep="")),
    #        map, width = 6, height = 6, units = "in")
    
  }
}

PlotSuperFlameAquaticAreas(geodata, dir=output_path, maps)
