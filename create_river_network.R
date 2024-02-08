# Create a river network object from flowline shapefiles
# Used in later steps to calculate river distance and snap points to river channel

# library(sf)
# library(riverdist)
# 
# 
# spatial_dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/SpatialData"

#read in river line shapefile, reproject to UTM, and convert to river network object
projection = "+init=epsg:26915"
IL_network <- line2network(path = file.path(spatial_dir),
                           layer = "IL_river_flowline",
                           tolerance = 200,
                           reproject = projection)

#clean up step: interactive, save output
IL_network_clean <- cleanup(IL_network)
#notes:
#dissolve: y
#Insert vertices: y
#Minimum distance to use: 1
#Please identify segment number: 1
#Please identify vertex number: 540421
#Accept mouth assignment: y
#Remove additional segments: n
#Build segment routes: y

plot(IL_network_clean)
str(IL_network_clean)

saveRDS(IL_network_clean, file=file.path(spatial_dir, 'IL_network.rds'))
