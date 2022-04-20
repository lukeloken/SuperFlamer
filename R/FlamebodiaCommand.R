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

#Load superflame scripts
source('R/RunAllSuperFlameScripts.R')

# If you need to install sensorQC use this command
# install.packages("sensorQC", repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"), dependencies = TRUE)

#Choose gg basemaps

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

#Tonle Sap (Jan 2022)
map_big <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                    'TonleSap2_ggmap.rds'))
map_upper <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                      'TonleSap_upper_ggmap.rds'))
map_confluence <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                           'TonleSap_confluence_ggmap.rds'))
map_lower <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                      'TonleSap_lower_ggmap.rds'))

#Mekong River (Feb 2022)
map_Mekong_big <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                           'MekongRiver_big_ggmap.rds'))
map_Mekong_south <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                             'MekongRiver_south_ggmap.rds'))
map_Mekong_middle1 <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                               'MekongRiver_middle1_ggmap.rds'))
map_Mekong_middle2 <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                               'MekongRiver_middle2_ggmap.rds'))
map_Mekong_north <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                             'MekongRiver_north_ggmap.rds'))


#Load the flame directories
home_path <- "C:/Users/lloken/OneDrive - DOI/Flamebodia/Data"

#Jan 2022 (Tonle Sap)
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-01-22"), by = "day")
maps <- list(map_big, map_upper, map_lower, map_confluence)

#Feb 2022 (Mekong River)
dates_merge <- seq.Date(as.Date("2022-02-18"), as.Date("2022-02-25"), by = "day")
maps <- list(map_Mekong_big, map_Mekong_south, map_Mekong_middle1, map_Mekong_middle2, map_Mekong_north)

#April 2022 (Tonle Sap)
dates_merge <- seq.Date(as.Date("2022-04-04"), as.Date("2022-04-09"), by = "day")
maps <- list(map_big, map_upper, map_lower, map_confluence)



# Load directories
# Select directories by date are run.
directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_torun <- file.path(home_path, directories_all[directories_dates])

for(dir in directories_torun){
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-14_TonleSapNorthShore'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-15_TonleSapNorthBasin'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-16_TonleSapNorthBasin2'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-17_TonleSapNorthBasin3'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-18_TonleSapCentral'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-19_TonleSapLower'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-21_TonleSapLowertoPP'

#Feb 2022
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-18_BasaacRver'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-19_MekongRiverPhnomPenhDown'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-20_MekongRiverToKampongCham'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-21_MekongRiverToKratie'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-22_MekongRiverToSambur'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-23_MekongRiverSamburToStungTrung'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-24_MekongRiverStrungTrengTo3S'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-25_MekongRiverStrungTrengToRonkel'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-02-25_MekongRiverStrungTrengToRonkel2'

#April 2022
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-04_MekongRiverPhnomPehn'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-05_TonleSapRiver'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-06_TonleSapLakeTonleChamar'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-07_TonleSapLakeLower'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-08_TonleSapCenter'
# dir<-'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-04-09_TonleSapUpper'

list.files(dir)



# plotdiag=F will turn off sensorQC plotting (outlier flags)
RunSuperFlame(dir, maps = maps, plotdiag = TRUE)

}

# #######################################
# Merge shapefiles and sample tables
# Make multiple maps spanning mulptiple days
# #######################################

#load merging scripts/functions
source('R/MergeSampleTables.R')
source('R/MergeMap.R')

home_path <- "C:/Users/lloken/OneDrive - DOI/Flamebodia/Data"

merge_name <- "Merged_TonleSap_April_2022"
dates_merge <- seq.Date(as.Date("2022-04-04"), as.Date("2022-04-09"), by = "day")
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

merge_name <- "Merged_Mekong_TonleSap_JanFeb_2022"
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-02-25"), by = "day")
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

merge_name <- "Merged_Mekong_Feb_2022"
dates_merge <- seq.Date(as.Date("2022-02-18"), as.Date("2022-02-25"), by = "day")
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

merge_name <- "Merged_TonleSap_January_2022"
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-01-22"), by = "day")
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

merge_name <- "Merged_TonleSap_JanuaryApril_2022"
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-04-09"), by = "day")
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)



directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_merge <- directories_all[directories_dates]

# maps <- list(map_big, map_upper, map_lower, map_confluence, 
#              map_Mekong_big, map_Mekong_south, map_Mekong_middle1, 
#              map_Mekong_middle2, map_Mekong_north)

maps <- list(map_big, map_upper, map_lower, map_confluence)


#merge samples
samples_merged <- MergeSampleTables(home_path, directories_merge, merge_name)

write.table(samples_merged, file = file.path(home_path, merge_name,
                                             paste0(merge_name, "_Samples.csv")), 
                                             col.names = TRUE, 
                                             row.names = FALSE, sep=",")

#merge maps
MergeMap(home_path, directories_merge, merge_name, maps)

