# ##########################################################################
# Flame Command
# Set data diretory and run all flame scripts
# 1) Data files (.dat) from superflame computer (e.g., Site_Date_GPS.dat)
# 2) meta tables (.csv) from access (e.g., FlameMetaDate.csv)
# ##########################################################################

#Clean environment and connections
rm(list=ls())
closeAllConnections()

library(ggsn)
library(dplyr)
library(sf)
library(lubridate)

#Load superflame scripts
source('R/RunAllSuperFlameScripts.R')
source('R/MergeSampleTables.R')
source('R/MergeMap.R')
source('remove_bad_data_Illinois.R')

# If you need to install sensorQC use this command
# install.packages("sensorQC", repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"), dependencies = TRUE)

#Choose gg basemaps

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMeFox'

#Load the flame directories
#fix this dupliation
home_path <- "C:/Users/lloken/OneDrive - DOI/FLAMeFox/Data"
map_lake <- readRDS(file.path(onedrive_dir, 'SpatialData', 'FoxRiver_lake_ggmap.rds'))
map_lower <- readRDS(file.path(onedrive_dir, 'SpatialData', 'FoxRiver_lower_ggmap.rds'))
map_upper <- readRDS(file.path(onedrive_dir, 'SpatialData', 'FoxRiver_upper_ggmap.rds'))
map_bay <- readRDS(file.path(onedrive_dir, 'SpatialData', 'FoxRiver_bay_ggmap.rds'))
map_big <- readRDS(file.path(onedrive_dir, 'SpatialData', 'FoxRiver_big_ggmap.rds'))

# bad_data <- remove_bad_data_Illinois()


#Aug 2023 (Fox River)
dates_merge <- seq.Date(as.Date("2023-08-07"), as.Date("2023-08-10"), by = "day")
maps <- list(map_big, map_bay,  map_upper, map_lower, map_lake)



# Load directories
# Select directories by date are run.
directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_torun <- file.path(home_path, directories_all[directories_dates])
directories_torun <- directories_torun[length(directories_torun)]
dir = directories_torun[1]
for(dir in directories_torun){
 
  
  list.files(dir)
  
  RunSuperFlame(dir, 
                maps = maps, 
                plotdiag = TRUE, 
                legend = "topleft", 
                # bad_data = bad_data
                )
  
}

# #######################################
# Merge shapefiles and sample tables
# Make multiple maps spanning mulptiple days
# #######################################

#load merging scripts/functions
source('R/MergeSampleTables.R')
source('R/MergeMap.R')
source('R/MergeMapMulti.R')

#Aug 2023
merge_name <- "Merged_Fox_2023"
dates_merge <- seq.Date(as.Date("2023-08-07"), as.Date("2023-08-10"), by = "day")
maps <- list(map_big, map_bay,  map_upper, map_lower, map_lake)

dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

# after loading campaign details, merge directories by campaign
directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_merge <- directories_all[directories_dates]


#merge samples
samples_merged <- MergeSampleTables(home_path, directories_merge, merge_name)

write.table(samples_merged, file = file.path(home_path, merge_name,
                                             paste0(merge_name, "_Samples.csv")), 
            col.names = TRUE, 
            row.names = FALSE, sep=",")

#merge maps
MergeMap(home_path, directories_merge, merge_name, maps, legend = "topleft", 
         plot_title = paste0(merge_name, ": Preliminary"))


# ################################################
# Make multi-panel maps across seasons
# ################################################

#not needed for single sample campaign. 

multi_merge_name <- "Merged_Illinois_May_2022_Jul_2023"
names_to_merge <- c("Merged_Illinois_May_2022", 
                    "Merged_Illinois_Aug_2022", 
                    "Merged_Illinois_Nov_2022", 
                    "Merged_Illinois_Mar_2023",
                    "Merged_Illinois_Jul_2023")
maps <- list(map_big, map_upper, map_middle, map_lower, 
             map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)
dir.create(file.path(home_path, multi_merge_name), showWarnings = FALSE)

MergeMapMulti(home_path, 
              multi_merge_name, 
              names_to_merge, 
              name_labels = gsub("Merged_Illinois_", "", names_to_merge),
              maps, 
              legend = "topleft", 
              plot_title = paste0(multi_merge_name, ": Preliminary"))


