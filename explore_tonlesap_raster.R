

library(raster)
library(sf)
library(sp)
library(rdgal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(spdplyr)

#Load wetted perimeter raster
gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'


nir_raster <- raster(file.path(gis_dir, "TonleSap_FromAlan", 
                               "LC08_L1TP_126051_20220102_20220106_02_T1_B5_MERGED.tif"))
names(nir_raster) <- "layer"

nir_raster <- reclassify(nir_raster, cbind(-Inf, 0.000001, NA), right = FALSE)
# nir_raster <- clamp(nir_raster, lower = 0.0000001)

plot(nir_raster)
res(nir_raster)

#aggregate from 30x30 resolution to 300x300 (factor = 10)
nir_raster_300m <- aggregate(nir_raster, fact = 10)
res(nir_raster_300m)
plot(nir_raster_300m)

#aggregate from 30x30 resolution to 150x150 (factor = 10)
nir_raster_150m <- aggregate(nir_raster, fact = 5)
res(nir_raster_150m)
plot(nir_raster_150m)



# create classification matrix
reclass_df <- c(0, 7360, 1,
                7360, 13357, 2,
                13357, 14239, 3, 
                14239, 14925, 4,
                14925, 15824, 5,
                15824, Inf, 6)
reclass_df

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

reclass_m

nir_raster_classified <- reclassify(nir_raster,
                                    reclass_m)
nir_raster_300m_classified <- reclassify(nir_raster_300m,
                                         reclass_m)
nir_raster_150m_classified <- reclassify(nir_raster_150m,
                                         reclass_m)

nir_colors <- c("#0077b6", "#c9cba3", "#ffe1a8", "#e26d5c", "#723d46", "#472d30")



par(mar= c(2,2,2,2))
plot(nir_raster_classified, col = nir_colors, axes = FALSE)
plot(nir_raster_300m_classified, col = nir_colors, axes = FALSE)
plot(nir_raster_150m_classified, col = nir_colors, axes = FALSE)



#preparing raster object to plot with geom_tile in ggplot2
r_points = rasterToPoints(nir_raster_classified, spatial = TRUE)
r_df = data.frame(r_points) 
head(r_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

#preparing raster object to plot with geom_tile in ggplot2
r_300m_points = rasterToPoints(nir_raster_300m_classified, spatial = TRUE)
r_300m_df = data.frame(r_300m_points)
head(r_300m_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

#preparing raster object to plot with geom_tile in ggplot2
r_150m_points = rasterToPoints(nir_raster_150m_classified, spatial = TRUE)
r_150m_df = data.frame(r_150m_points)
head(r_150m_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks



# #Raw imagery
# #Very slow
# plot1 <- ggplot(data = r_df) + 
#   geom_tile(aes(x = x, y = y, fill = as.character(layer))) + 
#   scale_fill_manual(values = nir_colors) + 
#   coord_equal() +
#   theme_bw() +
#   theme(panel.grid.major = element_blank()) +
#   xlab("Longitude") + ylab("Latitude") +
#   labs(fill = "NIR group") +
#   theme(axis.text = element_blank()) +
#   scale_x_continuous(expand = expansion(mult = c(0,0))) +
#   scale_y_continuous(expand = expansion(mult = c(0,0))) +
#   ggtitle("30m resolution: Jan 2022")
# 
# print(plot1)
# 
# ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", "NIRimagery_Jan2022_30m.png"), plot1, dpi = 500)

#300m resolution
plot2 <- ggplot(data = r_300m_df) + 
  geom_tile(aes(x = x, y = y, fill = as.character(layer))) + 
  scale_fill_manual(values = nir_colors) + 
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  labs(fill = "NIR group") +
  theme(axis.text = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  ggtitle("300m resolution: Jan 2022")

print(plot2)

ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", "NIRimagery_Jan2022_300m.png"), plot2, dpi = 500)


#150m resolution
plot3 <- ggplot(data = r_150m_df) + 
  geom_tile(aes(x = x, y = y, fill = as.character(layer))) + 
  scale_fill_manual(values = nir_colors) + 
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  labs(fill = "NIR group") +
  theme(axis.text = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  ggtitle("150m resolution: Jan 2022")

print(plot3)

ggsave(file.path(onedrive_dir, "Figures", "Maps_TonleSap", "NIRimagery_Jan2022_150m.png"), plot3, dpi = 500)



#Make pixel layer to interpolate
str(r_300m_points)

#using sf package
# watergrid_300m_sf <- st_as_sf(r_300m_points) %>%
#   filter(layer == 1)
# ggplot(watergrid_300m_sf) +
#   geom_sf()
# watergrid_300m <- as(watergrid_300m_sf, Class = "Spatial") 


watergrid_300m_sp <- r_300m_points %>%
  filter(layer == 1)
  
watergrid_150m_sp <- r_150m_points %>%
  filter(layer == 1)

watergrid_30m_sp <- r_points %>%
  filter(layer == 1)


projection <- proj4string(watergrid_300m_sp)

gridded(watergrid_300m_sp) <- TRUE
gridded(watergrid_150m_sp) <- TRUE

plot(watergrid_300m_sp, col = "blue")
plot(watergrid_150m_sp, col = "blue")


saveRDS(watergrid_300m_sp, file.path(onedrive_dir, "GIS",
                                     "TonleSap_January2022_WaterGrid_300m.rds"))
saveRDS(watergrid_150m_sp, file.path(onedrive_dir, "GIS",
                                     "TonleSap_January2022_WaterGrid_150m.rds"))
saveRDS(watergrid_30m_sp, file.path(onedrive_dir, "GIS",
                                     "TonleSap_January2022_WaterGrid_30m.rds"))

