

library(rgdal)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(riverdist)
library(viridis)

onedrive_dir <- 'C:/Users/slafond-hudson/DOI/Loken, Luke C - FLAMeFox'


# Google maps
# Commented code will load Loken API key on USGS computer
# Download maps from google server
# Updated Jan 2022, Each download costs money, so save these somewhere to use
###############################################################################################

# GoogleKey
GoogleAPIkey <- unlist(read.delim("C:/Users/slafond-hudson/keys/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))

register_google(key = as.character(GoogleAPIkey))

# Google background map tahoe
# map<-GetMap(center = c(39.09, -120.04), size=c(640,640), zoom=12, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)

#TonleSap old way
# map<-GetMap(center = c(12.45, 104.4),
#             size = c(640,640),
#             zoom = 9,
#             maptype = c("satellite"),
#             GRAYSCALE = F,
#             API_console_key = GoogleAPIkey)
# 
# PlotOnStaticMap(map)


# ggmap
# Tahoe
# map_test<-get_googlemap(center = c(-120.04, 39.09), size = c(640,640),
#                         zoom = 11, scale=2, maptype = "satellite", key=GoogleAPIkey )
# TonleSap
# map_test<-get_googlemap(center = c(104.45, 12.35),
#                         size = c(380,380),
#                         zoom = 8,
#                         scale = 2,
#                         maptype = "satellite",
#                         key=GoogleAPIkey )
# 
# ggmap(map_test)
# 
# ggmap(map_big)
# 
# 
# IllinoisRiver - upper
map_big<-get_googlemap(center = c(-88.25, 44.23),
                        size = c(500,500),
                        zoom = 9,
                        scale = 2,
                        maptype = "satellite",
                        key=GoogleAPIkey )
# 
ggmap(map_big)
# 
map_bay <-get_googlemap(center = c(-87.97, 44.57),
                         size = c(640,640),
                         zoom = 12,
                         scale = 2,
                         maptype = "satellite",
                         key=GoogleAPIkey )

ggmap(map_bay)
# 
map_lower <-get_googlemap(center = c(-88.07, 44.44),
                           size = c(640,640),
                           zoom = 11,
                           scale = 2,
                           maptype = "satellite",
                           key=GoogleAPIkey )

ggmap(map_lower)

map_upper <-get_googlemap(center = c(-88.44, 44.218),
                           size = c(640,640),
                           zoom = 13,
                           scale = 2,
                           maptype = "satellite",
                           key=GoogleAPIkey )

ggmap(map_upper)

map_lake <-get_googlemap(center = c(-88.40, 44.00),
                          size = c(500,500),
                          zoom = 10,
                          scale = 2,
                          maptype = "satellite",
                          key=GoogleAPIkey )

ggmap(map_lake)

map_lock <-get_googlemap(center = c(-88.125, 44.3795),
                         size = c(500,500),
                         zoom = 15,
                         scale = 2,
                         maptype = "satellite",
                         key=GoogleAPIkey )

ggmap(map_lock)

#save maps
# saveRDS(map, file=paste0(dropbox_dir, '/Data/SpatialData/UpperShipChannel_map1.rds'))
# saveRDS(map2, file=paste0(dropbox_dir, '/Data/SpatialData/UpperShipChannel_map2.rds'))
# saveRDS(map_test, file.path(onedrive_dir, 'SpatialData', 'Tahoe_ggmap.rds'))
# saveRDS(map_test, file.path(onedrive_dir, 'SpatialData', 'TonleSap_ggmap.rds'))
# saveRDS(map_test, file.path(onedrive_dir, 'SpatialData', 'TonleSap2_ggmap.rds'))
# saveRDS(map_upper, file.path(onedrive_dir, 'SpatialData', 'TonleSap_upper_ggmap.rds'))
# saveRDS(map_lower, file.path(onedrive_dir, 'SpatialData', 'TonleSap_lower_ggmap.rds'))
# saveRDS(map_confluence, file.path(onedrive_dir, 'SpatialData', 'TonleSap_confluence_ggmap.rds'))
dir.create(file.path(onedrive_dir, 'SpatialData'))
saveRDS(map_lake, file.path(onedrive_dir, 'SpatialData', 'FoxRiver_lake_ggmap.rds'))
saveRDS(map_lower, file.path(onedrive_dir, 'SpatialData', 'FoxRiver_lower_ggmap.rds'))
saveRDS(map_upper, file.path(onedrive_dir, 'SpatialData', 'FoxRiver_upper_ggmap.rds'))
saveRDS(map_bay, file.path(onedrive_dir, 'SpatialData', 'FoxRiver_bay_ggmap.rds'))
saveRDS(map_big, file.path(onedrive_dir, 'SpatialData', 'FoxRiver_big_ggmap.rds'))
saveRDS(map_lock, file.path(onedrive_dir, 'SpatialData', 'FoxRiver_lock_ggmap.rds'))
