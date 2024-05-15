library(sf)
library(riverdist)
library(sp)
library(rgdal)
library(raster)


dir <- "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/SpatialData"
file <- "Combined_flowline_Loken.shp"

old <- readRDS(file.path(dir, "IL_river_flowline.rds"))
old_network <- readRDS(file.path(dir, "IL_network.rds"))

project <- st_crs(old_network$sf)

IL_line_sf <- st_read(file.path(dir, file))
IL_line_sp <- readOGR(file.path(dir, file))


plot(st_geometry(IL_line_sf))
plot(IL_line_sp)

#project to UTM zone 15N
IL_line_sp_UTM <- sp::spTransform(IL_line_sp, crs("+init=epsg:32615"))

plot(IL_line_sp_UTM)

# Create linenetwork and compute cumulative distance. Only do this once
IL_network <- line2network(sp = IL_line_sp_UTM)
IL_network_clean <- cleanup(IL_network)
# IL_network_clean <- IL_singleflowline_clean

# IL_singleflowline_clean <- cleanup(IL_network_clean)
# IL_singleflowline_clean <- cleanup(IL_singleflowline_clean)


saveRDS(IL_network_clean, file = file.path(dir, 'Illinois_network_clean.rds'))

str(IL_network_clean)

plot(IL_network_clean$sp)

unique(IL_line_sf$Name)

river_names <- data.frame(River_name = unique(IL_line_sf$Name))

write.csv(river_names, file.path(dir, "RiverSegmentNames.csv"), row.names = F)
