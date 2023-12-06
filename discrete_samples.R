#assign discrete data to aquatic area class
#calculate river distance for each point

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(riverdist)
library(data.table)

source("./snap_points_to_river.R")

#paths
data_path <- "C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeIllinois/Data"
aqa_path <- "C:/Users/slafond-hudson/DOI/WQP Prioritized Constituents Project - Task 2 Carbon and Nutrients in the IRB/GIS/Aquatic Areas Loken Edits"
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

#read in discrete data and set the crs
# discrete <- read_csv(file.path(data_path, "Merged_Illinois_Nov_2022_Samples.csv"))
discrete <- st_as_sf(discrete, coords=c("longitude", "latitude"), crs=4326)
discrete <- st_transform(discrete, crs=26915)
head(discrete$geometry)

#read in aquatic areas and merge all polygons into one polygon
all_files <- list.files(path = aqa_path, pattern = "_new_loken.shp")
load_files <- all_files[!grepl(".xml", all_files)]
pools <- lapply(file.path(aqa_path, load_files), st_read)

names(pools) <- c("alt", "bra", "dre", "lag", "loc", "mar", "or1", "p26", "peo", "sta")

#merge all pools
df <- bind_rows(pools, .id='Pool') %>%
  select(-OBJECTID)
single_df <- st_union(df)

#return discrete points in that aren't covered by aqa
p <- st_difference(discrete, single_df)

#merge aquatic areas polygons with discrete data
p2 <- st_intersection(discrete, df)

projection = "+init=epsg:26915"

#commented out save line in snap_points_to_river.R first
points <- snap_points_to_river(p2, projection, processed_path, flame_file)

#this gets things in UTM, but I think I want to transform the lat/lon back 
points <- st_transform(points, crs=4326)
points <- points%>%
  mutate(long = unlist(map(points$geometry,1)),
         lat = unlist(map(points$geometry,2)))
write_csv(points, file.path(processed_path, '2_flame_snapped', 
                    'discrete_snapped_all.csv'))
# saveRDS(points, file.path(processed_path, '2_flame_snapped',
#                           'discrete_snapped_all.csv'))

discrete <- read_csv(file.path(processed_path, '2_flame_snapped', 'discrete_snapped_all.csv'))
class(discrete)
discrete <- st_as_sf(discrete, coords=c("long", "lat"), crs=4326) %>% st_transform(crs=26915)

discrete_map <- ggplot()+
  geom_sf(data=single_df)+
  geom_sf(data=discrete, aes(geometry = geometry, color=Month), size=2, alpha=0.8)+
  theme_classic()
print(discrete_map)

tribs <- discrete %>%
  filter(AQUA_CODE=="TRC")

discrete_tribs <- ggplot()+
  geom_sf(data=single_df)+
  geom_sf(data=tribs, aes(geometry = geometry, color=Month), size=2, alpha = 0.8)+
  theme_classic()
print(discrete_tribs)

discrete_tribs2 <- ggplot()+
  geom_point(data=tribs, aes(Dist_m/1000, 1, color=Month), size=2, alpha = 0.8)+
  geom_text(data=tribs, aes(x=Dist_m/1000, 1, label=Sample.Notes), hjust=-1, angle=45)+
  labs(xlab="River distance (km)")+
  theme_classic()
print(discrete_tribs2)
