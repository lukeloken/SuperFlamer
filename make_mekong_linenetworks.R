

library(spatstat)
library(gstat)
library(sp)
library(rgdal)
library(geoR)
library(raster)
library(rgeos)
library(maptools)
library(riverdist)
library(gstat)

gis_dir <- "C:/Users/lloken/OneDrive - DOI/GIS"
data_dir <- "C:/Users/lloken/OneDrive - DOI/FLAMebodia/Data"

#Code to create a route feature of the ship channel

Mekong_line <- readOGR(file.path(gis_dir, "Mekong"), "Mekong_SingleLine")
TonleSapRiver_line <- readOGR(file.path(gis_dir, "Mekong"), "TonleSapRiver_SingleLine")
BassacRiver_line <- readOGR(file.path(gis_dir, "Mekong"), "Bassac_SingleLine")

#load the NIR raster
nir_raster <- raster(file.path(gis_dir, "TonleSap_FromAlan", 
                               "LC08_L1TP_126051_20220102_20220106_02_T1_B5_MERGED.tif"))

projection <- crs(nir_raster)


geo_mekong <- readRDS(file.path(data_dir, "Merged_Mekong_Feb_2022", "Shapefiles", 
                                "Merged_Mekong_Feb_2022_Shapefile_AllData.rds"))

geo_mekong <- spTransform(geo_mekong, projection)


bb_data <- bbox(geo_mekong)
bb_data[,1] <- bb_data[,1] - 5000
bb_data[,2] <- bb_data[,2] + 5000 

geo_mekong_subset <- geo_mekong[sample(1:nrow(geo_mekong), size = nrow(geo_mekong)/100),]



gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

Mekong_line_clip <- gClip(Mekong_line, bb_data)
df <- data.frame(id = as.character(1:length(Mekong_line_clip)), 
                 fake_name = letters[1:length(Mekong_line_clip)])
Mekong_line_clip <- SpatialLinesDataFrame(Mekong_line_clip, 
                                           data = df, 
                                           match.ID = FALSE)

Bassac_line_clip <- gClip(BassacRiver_line, bb_data)
df2 <- data.frame(id = as.character(1:length(Bassac_line_clip)), 
                 fake_name = letters[1:length(Bassac_line_clip)])
Bassac_line_clip <- SpatialLinesDataFrame(Bassac_line_clip, 
                      data = df2, 
                      match.ID = FALSE)
## Warning: spgeom1 and spgeom2 have different proj4 strings

plot(Mekong_line_clip, col = "green", lwd = 5, axes = TRUE)
plot(Mekong_line, add = TRUE)

plot(Bassac_line_clip, col = "orange", lwd = 5, axes = TRUE, add = TRUE)
plot(BassacRiver_line, add = TRUE)


# plot(nir_raster, add = TRUE)
# plot(Mekong_line, add = TRUE)
plot(TonleSapRiver_line, add = TRUE, col = "red", lwd = 5)

plot(geo_mekong_subset, add = TRUE, col = "blue", cex = .1, pch = 16)


# Transform line into UTM's (zone 10, for California). This way distance is in meters (m)
Mekong_line_UTM <- spTransform(Mekong_line_clip, crs(nir_raster))
TonleSapRiver_line_UTM <- spTransform(TonleSapRiver_line, crs(nir_raster))
BassacRiver_line_UTM <- spTransform(Bassac_line_clip, crs(nir_raster))

plot(nir_raster)
plot(Mekong_line_UTM, add = TRUE, col = "blue")
plot(TonleSapRiver_line_UTM, add = TRUE, col = "red")
plot(BassacRiver_line_UTM, add = TRUE, col = "orange")


Mekong_network <- line2network(sp = Mekong_line_UTM)
TonleSapRiver_network <- line2network(sp = TonleSapRiver_line_UTM)
BassacRiver_network <- line2network(sp = BassacRiver_line_UTM)


Mekong_network_clean <- cleanup(Mekong_network)
TonleSapRiver_network_clean <- cleanup(TonleSapRiver_network)
BassacRiver_network_clean <- cleanup(BassacRiver_network)


Mekong_network_clean <- cleanup(Mekong_network_clean)
BassacRiver_network_clean <- cleanup(BassacRiver_network_clean)


plot(Mekong_network)
plot(Mekong_network_clean)
plot(TonleSapRiver_network_clean)
plot(BassacRiver_network_clean)

str(Mekong_network_clean)
str(TonleSapRiver_network_clean)


saveRDS(Mekong_network_clean, 
        file.path(gis_dir, "Mekong", "MekongNetwork.rds"))
saveRDS(TonleSapRiver_network_clean, 
        file.path(gis_dir, "Mekong", "TonleSapRiverNetwork.rds"))
saveRDS(BassacRiver_network_clean, 
        file.path(gis_dir, "Mekong", "BassacRiverNetwork.rds"))


#End
