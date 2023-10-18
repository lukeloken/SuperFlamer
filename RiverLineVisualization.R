# 
# library(sf)
# library(riverdist)
# library(dplyr)
# library(tidyverse)
# 
# spatial_dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/SpatialData"
# home_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"
# flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/Shapefiles"
# output_path <- "C:/workflows/SuperFlamer/figures"
# 
# # Read in snapped flame data 
# points <- readRDS(file.path(home_path, "ProcessedObjects", 'Merged_Illinois_Jul_2023_all_snapped.RDS'))
# 
# points_mc <- readRDS(file.path(home_path, "ProcessedObjects", "Merged_Illinois_Jul_2023_mc_snapped.RDS"))
# 
# # network_clean <- readRDS(file.path(spatial_dir, "IL_network.rds"))
# 
# names(points_mc)
# geodata <- points_mc %>%
#   select("CH4_Dry", "CO2_Dry", 
#          "CH4uM", "CH4Sat", 
#          "CO2uM", "CO2Sat",
#          "H2O", "barom_mmHg",
#          "NO3_uM", "NO3_mgL", 
#          "abs254", "abs350",
#          "water_temp", "depth",
#          "temp", "specCond", 
#          "pH", "pressure",
#          "chlor_RFU", "chlor_ugL",
#          "ODO_percent", "ODO_mgL",
#          "BGApc_RFU", "BGApc_ugL", 
#          "turb_FNU", 
#          "fDOM_RFU", "fDOM_QSU",
#          "Turb_C6P", "CDOM_C6P", 
#          "CHL_a_C6P","Brightners",
#          "Fluorescein","Ref_Fuel",
#          "Temp_C6P",  
#          "CDOM_C6P_wt", "CDOM_C6P_turb",
#          "CHL_a_C6P_wt", 
#          # "CHL_a_C6P_turb", "Fluorescein_turb",
#          "Brightners_wt", "Brightners_turb",
#          "Fluorescein_wt",
#          "Ref_Fuel_wt", "Ref_Fuel_turb",
#          "FP_Trans", "FP_GreenAlgae",
#          "FP_BlueGreen", "FP_Diatoms",
#          "FP_Cryptophyta", "FP_YellowSubs",
#          "latitude", "longitude", "Dist")

PlotSuperFlameRiverDist <- function(geodata, dir) {

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
  theme(legend.position = c(.02, .45),
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
                               barwidth = 1,
                               barheight = 5,
                               ticks.colour = "black", 
                               ticks.linewidth = 0.5,
                               order = 1),
         label.position = 'top')
  )

  #Identify variables in dataset to plot
  plotvars_i <- names(geodata)

  # var_i = "NO3_mgL"

  for (var_i in plotvars_i) {
    
  data_i <- geodata %>% 
    select(Dist, any_of(var_i), AQUA_CODE) %>% 
    filter(!is.na(var_i))

  data_i$AQUA_CODE <- factor(data_i$AQUA_CODE, levels=c("MNC", "CB", "SC", "TRC", "CFL", "LM", "N"))
  
  fig <- ggplot(data_i, aes(x=Dist/1000, y=.data[[var_i]], color=AQUA_CODE))+
    geom_point(size = 1, alpha=0.7)+
    scale_color_brewer("Aquatic area type", palette="BuGn")+
    labs(x = "River distance (km)")+
    theme_classic()
  
  print(fig)
  
  ggsave(file.path(dir,
                   paste("riverdist_", var_i, ".png", sep="")),
         fig, width = 6, height = 6, units = "in")
  
  }  
}  

# PlotSuperFlameRiverDist(geodata, dir=output_path)  
  
  
  
  
  
  
  
  
  # fig <- ggplot(data_i, aes(Dist/1000, .data[[var_i]]))+
  # geom_point(size = 1, alpha=0.5, color="seagreen")+
  # geom_point(data=points_mc, aes((Dist/1000)+1, CH4_Dry), size=1, alpha=0.5, color="blue")+
  # labs(y = "CH4 Dry", x = "River distance (km)")+
  # theme_classic()
  # theme(legend.position = c(.9, .),
  #       legend.justification = c(0,0), 
  #       legend.background = element_rect(fill = NA, colour=NA),
  #       legend.text=element_text(size=8),
  #       legend.title=element_text(size=10), 
  #       legend.key.height = unit(.4, "cm"),
  #       legend.key.width = unit(1.2, "cm"), 
  #       panel.border=element_rect(fill=NA, colour="black"), 
  #       legend.direction="vertical"),
# print(fig)
