#Aquatic areas and river distance command file

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(riverdist)

#File paths
aqa_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas"
processed_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/ProcessedObjects"
spatial_dir <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/SpatialData"
output_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/RiverDist_plots"
flame_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/Merged_Illinois_Jul_2023/Shapefiles"
flame_file <- "Merged_Illinois_Jul_2023_Shapefile_AllData"

#functions
source("./merge_aquatic_areas.R")
source("./snap_points_to_river.R")
source("./RiverLineVisualization.R")

###########################

# Step 1: merge aquatic areas
merge_aquatic_areas(aqa_path, processed_path)

# Step 2: create river network
# if already created, skip this step and load river_network.rds
projection = "+init=epsg:26915"
IL_network <- line2network(path = file.path(spatial_dir),
                           layer = "IL_river_flowline",
                           tolerance = 200,
                           reproject = projection)

network_clean <- cleanup(IL_network)
#notes:
#dissolve: y
#Insert vertices: y
#Minimum distance to use: 1
#Please identify segment number: 1
#Please identify vertex number: 540421
#Accept mouth assignment: y
#Remove additional segments: n
#Build segment routes: y

saveRDS(network_clean, file=file.path(spatial_dir, 'river_network.rds'))

network_clean <- readRDS(file.path(spatial_dir, "river_network.rds"))

# Step 3: snap points to river network

#first read in flame data to snap
flame_file <- "Merged_Illinois_Jul_2023_Shapefile_AllData"

points <- readRDS(file.path(flame_path, 
                            paste(flame_file, ".rds", sep="")))
#then snap function
snap_points_to_river(points, projection, processed_path, flame_file)

#Step 4: plot flame data by river distance

#read in snapped flame data
points <- readRDS(file=file.path(processed_path, paste(flame_file, "_all_snapped", ".rds", sep="")))
# points_mc <- readRDS(file=file.path(processed_path, paste(flame_file, "_mc_snapped", ".rds", sep="")))
# points_ao <- readRDS(file=file.path(processed_path, paste(flame_file, "_ao_snapped", ".rds", sep="")))  
# points_fl <- readRDS(file=file.path(processed_path, paste(flame_file, "_fl_snapped", ".rds", sep="")))
# points_tribs <- readRDS(file=file.path(processed_path, paste(flame_file, "_tribs_snapped", ".rds", sep="")))

#rename desired points object to geodata and select subset of flame variables
geodata <- points %>%
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
         "CHL_a_C6P_wt", 
         # "CHL_a_C6P_turb", "Fluorescein_turb",
         "Brightners_wt", "Brightners_turb",
         "Fluorescein_wt",
         "Ref_Fuel_wt", "Ref_Fuel_turb",
         "FP_Trans", "FP_GreenAlgae",
         "FP_BlueGreen", "FP_Diatoms",
         "FP_Cryptophyta", "FP_YellowSubs",
         "latitude", "longitude", "Dist",
         "AQUA_CODE", "AQUA_DESC", "Pool")

output_path = file.path(output_path, "dist_by_aqa")
PlotSuperFlameRiverDist(geodata, output_path)

#next, need to somehow map flame data onto aquatic areas so that flame dataframe has a column for aquatic area
#merge?
