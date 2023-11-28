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

  pool_start <- c(0, 60.900, 69.075, 92.746, 136.185, 158.099, 279.011, 404.837)
  pool_end <- c(60.378, 68.220, 91.834, 135.715, 158.097, 279.007, 404.836, 528.604)
  pool_centers <- c(30.1890, 64.56, 80.454, 114.231, 147.141, 218.553, 341.923, 466.721)
  pool_names <- c("loc", "bra", "dre", "mar", "sta", "peo", "lag", "alt")
  pool_info <- data.frame(pool_names, pool_start, pool_end, pool_centers)
  
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
  theme(legend.position = c(.8, .8),
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

  #custom color palette
  colors_map = c("black", "#005f73", "#0a9396", '#bb3e03', '#e09f3e', '#606c38', "#a7c957")
  
  #Identify variables in dataset to plot
  plotvars_i <- names(geodata)

  var_i = "chlor_ugL"

  for (var_i in plotvars_i) {
    
  data_i <- geodata %>% 
    select(Dist_m, any_of(var_i), AQUA_DESC) %>% 
    filter(!is.na(var_i))
  

  # data_i$AQUA_CODE <- factor(data_i$AQUA_CODE, levels=c("MNC", "CB", "SC", "TRC", "CFL", "LM", "N"))
  data_i$AQUA_DESC <- factor(data_i$AQUA_DESC, levels=c("Main Navigation Channel", 
                                                        "Channel Border", 
                                                        "Side Channel", 
                                                        "Tributary Channel", 
                                                        "Contiguous Floodplain Lake", 
                                                        "Lake Michigan", 
                                                        "Non-aquatic"))
  data_i <- data_i %>%
    arrange(AQUA_DESC)
  
  #plot all data with colors by aquatic area
  fig <- ggplot(data_i, aes(x=Dist_m/1000, y=.data[[var_i]], color=AQUA_DESC))+
    geom_vline(xintercept=pool_end, linetype="longdash", color="lightgray")+
    geom_point(aes(size = AQUA_DESC), alpha=0.7)+
    # annotate(geom="text", x=15, y = Inf, label="Pool boundaries", vjust = 1.5, size=2.5, color="lightgray")+
    scale_size_manual("Aquatic areas", values=c(0.5, rep(1,10)))+
    scale_color_manual("Aquatic areas", values = colors_map)+
    labs(x = "River distance (km)")+
    theme_classic()+
    theme(legend.position = "bottom",
          legend.justification = c(0,0), 
          legend.background = element_rect(fill = NA, colour=NA),
          legend.text=element_text(size=8),
          legend.title=element_text(size=10),
          legend.title.align = 0.5,
          legend.key.height = unit(.4, "cm"),
          legend.key.width = unit(1.2, "cm"), 
          panel.border=element_rect(fill=NA, colour="black"), 
          legend.direction="vertical")+
    guides(color = guide_legend(ncol = 3), size = guide_legend(ncol=3))
  
  print(fig)
  
  ggsave(file.path(output_path, "dist_by_aqua",
                   paste("riverdist_", var_i, ".png", sep="")),
         fig, width = 6, height = 4, units = "in")
  
  
  #plot just main channel data
  data_i <- data_i %>% 
    filter(AQUA_DESC=="Main Navigation Channel")
  
  fig <- ggplot(data_i, aes(x=Dist_m/1000, y=.data[[var_i]]), color="black")+
    geom_vline(xintercept=pool_end, linetype="longdash", color="lightgray")+
    geom_point(aes(size = AQUA_DESC), alpha=0.7)+
    # annotate(geom="text", x=15, y = Inf, label="Pool boundaries", vjust = 1.5, size=2.5, color="lightgray")+
    scale_size_manual("Aquatic areas", values=c(0.5, rep(1,10)))+
    scale_color_manual("Aquatic areas", values = colors_map)+
    labs(x = "River distance (km)")+
    theme_classic()+
    theme(legend.position = "none")
  
  print(fig)
  
  ggsave(file.path(output_path, "main_channel",
                   paste("riverdist_", var_i, ".png", sep="")),
         fig, width = 6, height = 4, units = "in")
  
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
