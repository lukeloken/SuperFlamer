#assign discrete data to aquatic area class
#calculate river distance for each point

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(riverdist)
library(data.table)

#paths
data_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"
aqa_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/AquaticAreas"
processed_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data/ProcessedObjects"
# flame_file <- "Merged_Illinois_Nov_2022_Shapefile_AllData"

# discrete_files <- c("Merged_Illinois_May_2022/Merged_Illinois_May_2022_Samples.csv",
#                     "Merged_Illinois_Aug_2022/Merged_Illinois_Aug_2022_Samples.csv",
#                     "Merged_Illinois_Nov_2022/Merged_Illinois_Nov_2022_Samples.csv",
#                     "Merged_Illinois_Mar_2023/Merged_Illinois_Mar_2023_Samples.csv",
#                     "Merged_Illinois_Jul_2023/Merged_Illinois_Jul_2023_Samples.csv")

May <- read.csv(file.path(data_path, "Merged_Illinois_May_2022/Merged_Illinois_May_2022_Samples.csv"))%>%
  mutate_all(as.character)
Aug <- read.csv(file.path(data_path, "Merged_Illinois_Aug_2022/Merged_Illinois_Aug_2022_Samples.csv"))%>%
  mutate_all(as.character)
Nov <- read.csv(file.path(data_path, "Merged_Illinois_Nov_2022/Merged_Illinois_Nov_2022_Samples.csv"))%>%
  mutate_all(as.character)
Mar <- read.csv(file.path(data_path, "Merged_Illinois_Mar_2023/Merged_Illinois_Mar_2023_Samples.csv"))%>%
  mutate_all(as.character)
Jul <- read.csv(file.path(data_path, "Merged_Illinois_Jul_2023/Merged_Illinois_Jul_2023_Samples.csv"))%>%
  mutate_all(as.character)
discrete <- bind_rows(May, Aug, Nov, Mar, Jul, .id="Month")
discrete["Month"][discrete["Month"]==1] <- "May"
discrete["Month"][discrete["Month"]==2] <- "Aug"
discrete["Month"][discrete["Month"]==3] <- "Nov"
discrete["Month"][discrete["Month"]==4] <- "Mar"
discrete["Month"][discrete["Month"]==5] <- "Jul"


# # file_i = "Merged_Illinois_May_2022/Merged_Illinois_May_2022_Samples.csv"
# list_all <- lapply(file.path(data_path, discrete_files), read.csv)
# names(list_all) <- c("May", "Aug", "Nov", "Mar", "Jul")
# # list_all <- lapply(list_all, as.character)
# df_all <- bind_rows(list_all, .id="Month")
# # df_all <- rbindlist(list(list_all))
# 
# # df_all <- data.frame()

# 
# for (1:length(discrete_files)){
#   discrete <- read_csv(file_i)
#   df_all <- bind_rows(df_all, discrete)
# }
# 
# 
# discrete <- discrete_files %>% map_df(read_csv(.))

# for (file_i in discrete_files){
#   discrete_all <- data.frame()
#   
#   discrete <- read_csv(file.path(data_path, file_i))
#   discrete_all <- bind_rows(discrete_all, discrete)
#   return(discrete_all)
#   # 
#   #   st_as_sf(coords=c("longitude", "latitude"), crs = 4326)%>%
#   #   st_transform(crs=26915)
#   # 
#   # aqa <- readRDS(file.path(processed_path, "aquatic_areas_all.rds")) %>%
#   # st_transform(crs=26915) %>%
#   # st_as_sf()
#   # aqa_merge <-st_union(aqa)
#   # 
#   # p2 <- st_difference(discrete, aqa_merge)
#   
#   # discrete_all <- bind_rows(discrete_all, p2)
# }

#read in discrete data and set the crs
# discrete <- read_csv(file.path(data_path, "Merged_Illinois_Nov_2022_Samples.csv"))
discrete <- st_as_sf(discrete, coords=c("longitude", "latitude"), crs=4326)
discrete <- st_transform(discrete, crs=26915)
head(discrete$geometry)

#read in aquatic areas and merge all polygons into one polygon
aqa <- readRDS(file.path(processed_path, "aquatic_areas_all.rds"))
st_transform(aqa, crs=26915)
st_as_sf(aqa)
aqa_merge <-st_union(aqa)

#return discrete points in that aren't covered by aqa
p <- st_difference(discrete, aqa_merge)

#merge aquatic areas polygons with discrete data
p2 <- st_intersection(discrete, aqa)


discrete_map <- ggplot()+
  geom_sf(data=aqa)+
  geom_sf(data=discrete, aes(geometry = geometry, color=Month), size=1)+
  theme_classic()
print(discrete_map)
