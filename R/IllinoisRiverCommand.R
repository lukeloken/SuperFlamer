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

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMeIllinois'

#Load the flame directories
home_path <- "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data"


#Illinois River (May 2022)
map_big <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                    'IllinoisRiver_big_ggmap.rds'))
map_upper <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                      'IllinoisRiver_upper_ggmap.rds'))
map_middle <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                       'IllinoisRiver_middle_ggmap.rds'))
map_lower <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
                                      'IllinoisRiver_lower_ggmap.rds'))

map_zoom10_1 <- readRDS(file.path(onedrive_dir, 'SpatialData',
                                  'IllinoisRiver_zoom10_1_ggmap.rds'))
map_zoom10_2 <- readRDS(file.path(onedrive_dir, 'SpatialData',
                                  'IllinoisRiver_zoom10_2_ggmap.rds'))
map_zoom10_3 <- readRDS(file.path(onedrive_dir, 'SpatialData',
                                  'IllinoisRiver_zoom10_3_ggmap.rds'))
map_zoom10_4 <- readRDS(file.path(onedrive_dir, 'SpatialData',
                                  'IllinoisRiver_zoom10_4_ggmap.rds'))
map_zoom10_5 <- readRDS(file.path(onedrive_dir, 'SpatialData',
                                  'IllinoisRiver_zoom10_5_ggmap.rds'))
map_zoom10_6 <- readRDS(file.path(onedrive_dir, 'SpatialData',
                                  'IllinoisRiver_zoom10_6_ggmap.rds'))
map_zoom10_7 <- readRDS(file.path(onedrive_dir, 'SpatialData',
                                  'IllinoisRiver_zoom10_7_ggmap.rds'))
# 

bad_data <- remove_bad_data_Illinois()


#May 2022 (Illinois)
dates_merge <- seq.Date(as.Date("2022-05-02"), as.Date("2022-05-07"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower)
maps <- list(map_big, map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)


#Aug 2022 (Illinois)
dates_merge <- seq.Date(as.Date("2022-08-08"), as.Date("2022-08-13"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower)
maps <- list(map_big, map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)

#Nov 2022 (Illinois)
dates_merge <- seq.Date(as.Date("2022-11-07"), as.Date("2022-11-14"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower)
maps <- list(map_big, map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)

#Mar 2023 (Illinois)
dates_merge <- seq.Date(as.Date("2023-03-29"), as.Date("2023-04-05"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower)
maps <- list(map_big, map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)

#July 2023 (Illinois)
dates_merge <- seq.Date(as.Date("2023-07-10"), as.Date("2023-07-20"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower)
maps <- list(map_big, map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)


# Load directories
# Select directories by date are run.
directories_all <- list.files(home_path)
directories_dates <- which(!is.na(as.Date(directories_all)) & as.Date(directories_all) %in% dates_merge)
directories_torun <- file.path(home_path, directories_all[directories_dates])
# directories_torun <- directories_torun[length(directories_torun)]
# dir = directories_torun[10]
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
  RunSuperFlame(dir, 
                maps = maps, 
                plotdiag = TRUE, 
                legend = "topleft", 
                bad_data = bad_data)
  
}

# #######################################
# Merge shapefiles and sample tables
# Make multiple maps spanning mulptiple days
# #######################################

#load merging scripts/functions
source('R/MergeSampleTables.R')
source('R/MergeMap.R')

#May 2022
merge_name <- "Merged_Illinois_May_2022"
dates_merge <- seq.Date(as.Date("2022-05-02"), as.Date("2022-05-07"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower, 
             map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

#Aug 2022
merge_name <- "Merged_Illinois_Aug_2022"
dates_merge <- seq.Date(as.Date("2022-08-08"), as.Date("2022-08-13"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower, 
             map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

#Nov 2022
merge_name <- "Merged_Illinois_Nov_2022"
dates_merge <- seq.Date(as.Date("2022-11-07"), as.Date("2022-11-14"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower, 
             map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

#Mar 2023
merge_name <- "Merged_Illinois_Mar_2023"
dates_merge <- seq.Date(as.Date("2023-03-29"), as.Date("2023-04-05"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower, 
             map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)
dir.create(file.path(home_path, merge_name), showWarnings = FALSE)

#Jul 2023
merge_name <- "Merged_Illinois_Jul_2023"
dates_merge <- seq.Date(as.Date("2023-07-10"), as.Date("2023-07-20"), by = "day")
maps <- list(map_big, map_upper, map_middle, map_lower, 
             map_zoom10_7, map_zoom10_6, map_zoom10_5, 
             map_zoom10_4, map_zoom10_3, map_zoom10_2, map_zoom10_1)
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

source('R/MergeMapMulti.R')

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


