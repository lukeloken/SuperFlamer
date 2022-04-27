

library(raster)
library(sf)
library(sp)
library(rdgal)
library(rgeos)
library(dplyr)
library(ggplot2)

#Load wetted perimeter raster
gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"


nir_raster <- raster(file.path(gis_dir, "TonleSap_FromAlan", 
                               "LC08_L1TP_126051_20220102_20220106_02_T1_B5_MERGED.tif"))
nir_raster <- reclassify(nir_raster, cbind(-Inf, 0.000001, NA), right = FALSE)
# nir_raster <- clamp(nir_raster, lower = 0.0000001)

plot(nir_raster)
res(nir_raster)

#aggregate from 30x30 resolution to 300x300 (factor = 10)
nir_raster_300m <- aggregate(nir_raster, fact = 10)
res(nir_raster_300m)
plot(nir_raster_300m)


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

nir_colors <- c("#0077b6", "#c9cba3", "#ffe1a8", "#e26d5c", "#723d46", "#472d30")



par(mar= c(2,2,2,2))
plot(nir_raster_classified, col = nir_colors, axes = FALSE)
plot(nir_raster_300m_classified, col = nir_colors, axes = FALSE)



#preparing raster object to plot with geom_tile in ggplot2
r_points = rasterToPoints(nir_raster_300m_classified)
r_df = data.frame(r_points) %>%
  rename(layer =  LC08_L1TP_126051_20220102_20220106_02_T1_B5_MERGED)
head(r_df) #breaks will be set to column "layer"
# r_df$cuts = cut(r_df$layer, breaks=c(0:6)) #set breaks

# nir_colors <- c("#0077b6", "#c9cba3", "#ffe1a8", "#e26d5c", "#723d46", "#472d30", "#d9d9d9")

ggplot(data = r_df) + 
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
