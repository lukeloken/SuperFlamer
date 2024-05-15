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

#Load superflame scripts
source('R/RunAllSuperFlameScripts.R')
source('R/MergeSampleTables.R')
source('R/MergeMap.R')
source('R/MergeChemistry.R')
source('remove_bad_data_FLAMebodia.R')

# If you need to install sensorQC use this command
# install.packages("sensorQC", repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"), dependencies = TRUE)

#Choose gg basemaps

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

#Load the flame directories
home_path <- "C:/Users/lloken/OneDrive - DOI/Flamebodia/Data"


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
map_Mekong_north2 <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                              'MekongRiver_north2_ggmap.rds'))

bad_data <- remove_bad_data_FLAMebodia()


#Jan 2022 (Tonle Sap)
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-01-22"), by = "day")
maps <- list(map_big, map_upper, 
             map_lower, map_confluence)

#Feb 2022 (Mekong River)
dates_merge <- seq.Date(as.Date("2022-02-18"), as.Date("2022-02-25"), by = "day")
maps <- list(map_Mekong_big, map_Mekong_south, 
             map_Mekong_middle1, map_Mekong_middle2, 
             map_Mekong_north)

#April 2022 (Tonle Sap)
dates_merge <- seq.Date(as.Date("2022-04-04"), as.Date("2022-04-09"), by = "day")
maps <- list(map_big, map_upper, 
             map_lower, map_confluence)


#Sept 2022 (Tonle Sap)
dates_merge <- seq.Date(as.Date("2022-09-27"), as.Date("2022-10-09"), by = "day")
maps <- list(map_big, map_upper, 
             map_lower, map_confluence)

#Jan 2023 (Tonle Sap)
dates_merge <- seq.Date(as.Date("2023-01-30"), as.Date("2023-02-06"), by = "day")
maps <- list(map_big, map_upper, 
             map_lower, map_confluence)


# Load directories
# Select directories by date are run.
directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_torun <- file.path(home_path, directories_all[directories_dates])
directories_torun <- directories_torun[grepl("TonleSap|Mekong", directories_torun)]
# directories_torun <- directories_torun[-grepl("DOT1", directories_torun)]


dir <- directories_torun[length(directories_torun)] #select last one
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
  RunSuperFlame(dir, maps = maps, plotdiag = TRUE, bad_data = bad_data)
  
  
}

# #######################################
# Merge shapefiles and sample tables
# Make multiple maps spanning mulptiple days
# #######################################

#load merging scripts/functions
source('R/MergeSampleTables.R')
source('R/MergeMap.R')
source('R/MergeChemistry.R')


#Individual campaigns
merge_name <- "Merged_TonleSap_Jan_2023"
plot_name = "Jan 2022"
dates_merge <- seq.Date(as.Date("2023-01-30"), as.Date("2023-02-06"), by = "day")
maps <- list(map_big, map_upper, map_lower, map_confluence)
legend = "bottomleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)


merge_name <- "Merged_TonleSap_Sep_2022"
plot_name = "Sep 2022"
dates_merge <- seq.Date(as.Date("2022-09-27"), as.Date("2022-10-06"), by = "day")
maps <- list(map_big, map_upper, map_lower, map_confluence)
legend = "bottomleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)


merge_name <- "Merged_TonleSap_Apr_2022"
plot_name = "Apr 2022"
dates_merge <- seq.Date(as.Date("2022-04-04"), as.Date("2022-04-09"), by = "day")
maps <- list(map_big, map_upper, map_lower, map_confluence)
legend = "bottomleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)


merge_name <- "Merged_Mekong_Feb_2022"
plot_name = "Feb 2022"
dates_merge <- seq.Date(as.Date("2022-02-18"), as.Date("2022-02-25"), by = "day")
maps <- list(map_Mekong_big, map_Mekong_south, 
             map_Mekong_middle1, map_Mekong_middle2, 
             map_Mekong_north, map_Mekong_north2)
legend = "topleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)


merge_name <- "Merged_TonleSap_Jan_2022"
plot_name = "Jan 2022"
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-01-22"), by = "day")
maps <- list(map_big, map_upper, map_lower, map_confluence)
legend = "bottomleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

#Combining campaigns
#Jan+Feb
merge_name <- "Merged_Mekong_TonleSap_JanFeb_2022"
plot_name = "Jan-Feb 2022"
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-02-25"), by = "day")
maps <- list(map_Mekong_big, map_Mekong_south, 
             map_Mekong_middle1, map_Mekong_middle2, 
             map_Mekong_north, map_Mekong_north2)
legend = "topleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

#Jan+Feb+Apr
merge_name <- "Merged_Mekong_TonleSap_JanApr_2022"
plot_name = "Jan-Apr 2022"
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2022-04-09"), by = "day")
maps <- list(map_Mekong_big, map_Mekong_south, 
             map_Mekong_middle1, map_Mekong_middle2, 
             map_Mekong_north, map_Mekong_north2)
legend = "topleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

#Everything
merge_name <- "Merged_TonleSap_Jan2022_Jan2023"
plot_name = "Jan 2022 - Jan 2023"
dates_merge <- seq.Date(as.Date("2022-01-14"), as.Date("2023-02-06"), by = "day")
maps <- list(map_Mekong_big, map_Mekong_south, 
             map_Mekong_middle1, map_Mekong_middle2, 
             map_Mekong_north, map_Mekong_north2)
legend = "topleft"
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)


# ############################################################
# After loading the specific merge_name, directories, and maps
# Run the merge scripts
# ############################################################
directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_merge <- directories_all[directories_dates]
directories_merge <- directories_merge[grepl("TonleSap|Mekong|Basaac", directories_merge)]
directories_merge <- setdiff(directories_merge, "2022-02-25_MekongRiverStrungTrengToRonkel2")

#merge samples
samples_merged <- MergeSampleTables(home_path, directories_merge, merge_name)

write.table(samples_merged, file = file.path(home_path, merge_name,
                                             paste0(merge_name, "_Samples.csv")), 
            col.names = TRUE, 
            row.names = FALSE, sep=",")

#merge maps
MergeMap(home_path, directories_merge, merge_name, maps = maps)



#merge water chemistry from UCD, Hg, and Cambodia lab
chem_path <- file.path(home_path, "WaterChemistry", "DataToMerge")
chem_geo <- MergeChemistry(home_path, chem_path, 
                           samples_merged, merge_name, 
                           map = maps[[1]], 
                           plot_title = plot_name, 
                           legend = "bottomleft",
                           title_location = "inside", 
                           color_scale = "samplechem")




# ################################################
# Make multi-panel maps across seasons
# ################################################

source('R/MergeMapMulti.R')

multi_merge_name <- "Merged_TonleSap_Jan_Apr_Sep_2022"
names_to_merge <- c("Merged_TonleSap_Jan_2022", 
                    "Merged_TonleSap_Apr_2022", 
                    "Merged_TonleSap_Sep_2022")
maps <- list(map_big, map_upper, map_lower, map_confluence)

dir.create(file.path(home_path, multi_merge_name), showWarnings = FALSE)

MergeMapMulti(home_path, 
              multi_merge_name, 
              names_to_merge, 
              name_labels = gsub("Merged_TonleSap_", "", names_to_merge),
              maps, 
              legend = "bottomleft", 
              plot_title = paste0(multi_merge_name, ": Preliminary"))



# ################################################
# Full dataset for data publication
# ################################################

source('R/MergeMapMulti.R')

multi_merge_name <- "Merged_TonleSap_Jan2022_Jan2023"
names_to_merge <- c("Merged_TonleSap_Jan_2022", 
                    "Merged_Mekong_Feb_2022",
                    "Merged_TonleSap_Apr_2022", 
                    "Merged_TonleSap_Sep_2022", 
                    "Merged_TonleSap_Jan_2023")
maps <- list(map_big, map_Mekong_big)

#merge samples
samples_merged <- MergeSampleTables(home_path, directories_merge, merge_name)

write.table(samples_merged, file = file.path(home_path, merge_name,
                                             paste0(merge_name, "_Samples.csv")), 
            col.names = TRUE, 
            row.names = FALSE, sep=",")


dir.create(file.path(home_path, multi_merge_name), showWarnings = FALSE)

MergeMapMulti(home_path, 
              multi_merge_name, 
              names_to_merge, 
              name_labels = gsub("Merged_", "", names_to_merge),
              maps, 
              legend = "bottomleft", 
              plot_title = paste0(multi_merge_name, ": Preliminary"))



