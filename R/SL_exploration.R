library(rgdal)
library(ggmap)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
library(ggsn)
library(sf)
library(lubridate)
library(ggplot2)

home_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"

aqa_path <- "C:/Users/slafond-hudson/OneDrive - DOI/SLL/aqa_2010_lag_new"
aqa <- readOGR(dsn=aqa_path, layer="aqa_2010_lag_new")
head(aqa)

plot(aqa)
ncol(aqa)

aqa@data %>%
  select(AQUA_CODE)%>%
  distinct()

qtm(shp=aqa, fill = "AQUA_CODE", fill.palette = "Dark2")

geodata <- readRDS(file.path(home_path, "Merged_Illinois_Jul_2023/Shapefiles/Merged_Illinois_Jul_2023_Shapefile_AllData.rds"))
head(geodata)
head(aqa)

EPSG <- make_EPSG()
EPSG[grep("WGS 84$", EPSG$note), ]
aqa84 <- spTransform(aqa, CRS("+init=epsg:4326"))
saveRDS(aqa84, file="C:/Users/slafond-hudson/OneDrive - DOI/SLL/aqa84.Rds")

plot(d)


###############
# following code is from VisualizeSpatialDataGGmap.R
###############

color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), 
                                   rev(magma(5, begin=.25, end=.98))), 
                                 bias=1)

# Choose plot variables as of Jan 2022
# Chosen for Flamebodia and FLAMeIllinois
plotvars <- c("CH4_Dry", "CO2_Dry", 
              "CH4uM", "CH4Sat", 
              "CO2uM", "CO2Sat",
              "H2O", "barom_mmHg",
              "no3_uM", "nn03_mg", 
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
              "CHL_a_C6P_wt", "CHL_a_C6P_turb",
              "Brightners_wt", "Brightners_turb",
              "Fluorescein_wt", "Fluorescein_turb",
              "Ref_Fuel_wt", "Ref_Fuel_turb",
              "FP_Trans", "FP_GreenAlgae",
              "FP_BlueGreen", "FP_Diatoms",
              "FP_Cryptophyta", "FP_YellowSubs")

#Identify variables in dataset to plot  
plotvars_i<-intersect(plotvars, names(geodata))
var_i=1

for (var_i in 1:length(plotvars_i)){
  name<-plotvars_i[var_i]
  if (is.numeric(geodata@data[,name])==TRUE){
    a <- geodata[!is.na(geodata@data[,name]),]
    a_range <- range(as.data.frame(a@data)[,name], na.rm = TRUE)
    a_1_99tile <- quantile(as.data.frame(a@data)[,name], probs = c(.1,.99))
  }
}  

commonTheme_map<-list(
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(), 
        axis.title.x=element_blank(), 
        axis.ticks=element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm")),
  scale_colour_gradientn(colours = color.palette(n=100), 
                         limits=a_1_99tile, oob = scales::squish),
  theme(legend.justification = c(0,0), 
        legend.background = element_rect(fill = 'white', colour='black'),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10), 
        legend.key.height = unit(.4, "cm"),
        legend.key.width = unit(1.2, "cm"), 
        panel.border=element_rect(fill=NA, colour='black'), 
        legend.direction="horizontal"),
  guides(colour=guide_colorbar(title.position = 'bottom', 
                               title.hjust=0.5, 
                               title="CO2 uM", 
                               ticks.colour = "black", 
                               ticks.linewidth = 1))
)

basemap <- map_zoom10_3
  

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
