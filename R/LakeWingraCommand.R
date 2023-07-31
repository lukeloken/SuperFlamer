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

# If you need to install sensorQC use this command
# install.packages("sensorQC", repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"), dependencies = TRUE)

#Choose gg basemaps

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMeWingra'

map_wingra <- readRDS(file.path(onedrive_dir, 
                                'SpatialData', 
                                'LakeWingra_ggmap.rds'))

#Load the flame directories
home_path <- "C:/Users/lloken/OneDrive - DOI/FLAMeWingra/Data"

#May 2022 (Illinois)
dates_merge <- seq.Date(as.Date("2022-06-29"), as.Date("2022-06-29"), by = "day")
maps <- list(map_wingra)

# Load directories
# Select directories by date are run.
directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & 
                             as.Date(directories_all) %in% dates_merge)
directories_torun <- file.path(home_path, directories_all[directories_dates])

dir = directories_torun[1]
for(dir in directories_torun){

  list.files(dir)
  
  
  # plotdiag=F will turn off sensorQC plotting (outlier flags)
  RunSuperFlame(dir, maps = maps, plotdiag = TRUE, legend = "topleft")
  
}

