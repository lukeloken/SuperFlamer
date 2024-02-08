#Aquatic areas and river distance command file

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(riverdist)

#File paths
# aqa_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas"
aqa_path <- "C:/Users/slafond-hudson/DOI/WQP Prioritized Constituents Project - Task 2 Carbon and Nutrients in the IRB/GIS/Aquatic Areas Loken Edits"
processed_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/ProcessedObjects"
spatial_dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/SpatialData"

#more file paths, change date for each trip
output_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_May_2022/RiverDist_plots"
flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_May_2022/Shapefiles"
flame_file <- "Merged_Illinois_May_2022_Shapefile_AllData.rds"

date <- "May_2022"

projection = "+init=epsg:26915"

#functions
source("./merge_aquatic_area_pools.R")
source("./intersect_flame_aqa.R")
source("./snap_points_to_river.R")
source("./RiverLineVisualization.R")
source("./VisualizeAquaticAreasBoxplot.R")

###########################

# Step 1: merge aquatic areas and intersect flame data with aqa polygons
pattern <- "_new_loken.shp"
pool_names <- c("alt", "bra", "dre", "lag", "loc", "mar", "or1", "p26", "peo", "sta")

aqa <- merge_aquatic_area_pools(aqa_path, pattern, pool_names, processed_path)

polygon_check_list <- intersect_flame_aqa(flame_path, flame_file, projection, aqa, processed_path)

#do this if need to merge for the first time
points_aqa <- polygon_check_list[['points_aqa']]
saveRDS(points_aqa, file.path(processed_path, "1_flame_intersected", paste(date, "flame_intersected_aqa.rds", sep=""))) #give this a more descriptive name

#do this if already merged, just need to load points_aqa
# points_aqa <- readRDS(file.path(processed_path, "1_flame_intersected/all_tripsflame_intersected_aqa.rds"))
# points_aqa <- st_as_sf(points_aqa)
# points_aqa <- st_transform(points_aqa, crs=projection)

# Step 2: create river network
# if already created, skip this step and load river_network.rds

# IL_network <- line2network(path = file.path(spatial_dir),
#                            layer = "IL_river_flowline",
#                            tolerance = 200,
#                            reproject = projection)
# 
# network_clean <- cleanup(IL_network)
#notes:
#dissolve: y
#Insert vertices: y
#Minimum distance to use: 1
####network_clean10 uses 10
#Please identify segment number: 1
#Please identify vertex number: 540421
#Accept mouth assignment: y
#Remove additional segments: n
#Build segment routes: y

# saveRDS(network_clean, file=file.path(spatial_dir, 'river_network.rds'))

network_clean <- readRDS(file.path(spatial_dir, "river_network.rds"))

# Step 3: snap points to river network

#first read in flame data to snap
# points <- readRDS(file.path(processed_path, "1_flame_intersected", paste(date, "flame_intersected_aqa.rds", sep="")))

#then snap function
points <- snap_points_to_river(points_aqa, processed_path, flame_file)

#Step 4: plot flame data by river distance

#read in snapped flame data if skipped last step
# points <- readRDS(file=file.path(processed_path, "2_flame_snapped", paste(date, "_snapped.rds", sep="")))

#rename desired points object to geodata and select subset of flame variables
geodata <- points %>%
  select("CH4_Dry", "CO2_Dry", 
         "CH4uM", "CH4Sat", 
         "CO2uM", "CO2Sat",
         "H2O", "barom_mmHg",
         "NO3_uM", "NO3_mgL", 
         "abs254", "abs350",
         # "water_temp", 
         # "depth",
         "temp", "specCond", 
         "pH", "pressure",
         "chlor_RFU", "chlor_ugL",
         "ODO_percent", "ODO_mgL",
         "BGApc_RFU", "BGApc_ugL", 
         "turb_FNU", 
         "fDOM_RFU", "fDOM_QSU",
         # "Turb_C6P", 
         # "CDOM_C6P",
         # "CHL_a_C6P",
         # "Brightners",
         "Fluorescein",
         # "Ref_Fuel",
         # "Temp_C6P",
         # "CDOM_C6P_wt", "CDOM_C6P_turb",
         # "CHL_a_C6P_wt",
         # "CHL_a_C6P_turb", "Fluorescein_turb",
         # "Brightners_wt", "Brightners_turb",
         # "Fluorescein_wt",
         # "Ref_Fuel_wt", "Ref_Fuel_turb",
         # "FP_Trans", "FP_GreenAlgae",
         # "FP_BlueGreen", "FP_Diatoms",
         # "FP_Cryptophyta", "FP_YellowSubs",
         "latitude", "longitude",
         "Dist_m",
         "AQUA_CODE", "AQUA_DESC", "Pool")

PlotSuperFlameRiverDist(geodata, output_path)
plot_aqa_boxplots(points, geodata, output_path, date)
