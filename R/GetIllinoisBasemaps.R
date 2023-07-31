

library(rgdal)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(riverdist)
library(viridis)

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMeIllinois'


# Google maps
# Commented code will load Loken API key on USGS computer
# Download maps from google server
# Updated Jan 2022, Each download costs money, so save these somewhere to use
###############################################################################################

# GoogleKey
GoogleAPIkey<-unlist(read.delim("H:/Private/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))

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
map_big<-get_googlemap(center = c(-89.5, 40.3),
                        size = c(460,460),
                        zoom = 7,
                        scale = 2,
                        maptype = "satellite",
                        key=GoogleAPIkey )
# 
ggmap(map_big)
# 
map_middle <-get_googlemap(center = c(-89.7, 40.75),
                         size = c(640,640),
                         zoom = 9,
                         scale = 2,
                         maptype = "satellite",
                         key=GoogleAPIkey )

ggmap(map_middle)
# 
map_upper <-get_googlemap(center = c(-88.2, 41.6),
                           size = c(640,640),
                           zoom = 9,
                           scale = 2,
                           maptype = "satellite",
                           key=GoogleAPIkey )

ggmap(map_upper)

map_lower <-get_googlemap(center = c(-90.6, 39.4),
                           size = c(640,640),
                           zoom = 9,
                           scale = 2,
                           maptype = "satellite",
                           key=GoogleAPIkey )

ggmap(map_lower)


#Zoom in a bit more
map_zoom10_1 <-get_googlemap(center = c(-90.52, 39),
                          size = c(640,640),
                          zoom = 10,
                          scale = 2,
                          maptype = "satellite",
                          key=GoogleAPIkey )

ggmap(map_zoom10_1)

map_zoom10_2 <-get_googlemap(center = c(-90.7, 39.6),
                             size = c(640,640),
                             zoom = 10,
                             scale = 2,
                             maptype = "satellite",
                             key=GoogleAPIkey )

ggmap(map_zoom10_2)

map_zoom10_3 <-get_googlemap(center = c(-90.3, 40.1),
                             size = c(640,640),
                             zoom = 10,
                             scale = 2,
                             maptype = "satellite",
                             key=GoogleAPIkey )

ggmap(map_zoom10_3)

map_zoom10_4 <-get_googlemap(center = c(-89.8, 40.6),
                             size = c(640,640),
                             zoom = 10,
                             scale = 2,
                             maptype = "satellite",
                             key=GoogleAPIkey )

ggmap(map_zoom10_4)

map_zoom10_5 <-get_googlemap(center = c(-89.3, 41.1),
                             size = c(640,640),
                             zoom = 10,
                             scale = 2,
                             maptype = "satellite",
                             key=GoogleAPIkey )

ggmap(map_zoom10_5)

map_zoom10_6 <-get_googlemap(center = c(-88.5, 41.35),
                             size = c(640,640),
                             zoom = 10,
                             scale = 2,
                             maptype = "satellite",
                             key=GoogleAPIkey )

ggmap(map_zoom10_6)

map_zoom10_7 <-get_googlemap(center = c(-87.9, 41.8),
                             size = c(640,640),
                             zoom = 10,
                             scale = 2,
                             maptype = "satellite",
                             key=GoogleAPIkey )

ggmap(map_zoom10_7)

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
saveRDS(map_lower, file.path(onedrive_dir, 'SpatialData', 'IllinoisRiver_lower_ggmap.rds'))
saveRDS(map_upper, file.path(onedrive_dir, 'SpatialData', 'IllinoisRiver_upper_ggmap.rds'))
saveRDS(map_middle, file.path(onedrive_dir, 'SpatialData', 'IllinoisRiver_middle_ggmap.rds'))
saveRDS(map_big, file.path(onedrive_dir, 'SpatialData', 'IllinoisRiver_big_ggmap.rds'))

saveRDS(map_zoom10_1, file.path(onedrive_dir, 'SpatialData',
                                'IllinoisRiver_zoom10_1_ggmap.rds'))
saveRDS(map_zoom10_2, file.path(onedrive_dir, 'SpatialData',
                                'IllinoisRiver_zoom10_2_ggmap.rds'))
saveRDS(map_zoom10_3, file.path(onedrive_dir, 'SpatialData',
                                'IllinoisRiver_zoom10_3_ggmap.rds'))
saveRDS(map_zoom10_4, file.path(onedrive_dir, 'SpatialData',
                                'IllinoisRiver_zoom10_4_ggmap.rds'))
saveRDS(map_zoom10_5, file.path(onedrive_dir, 'SpatialData',
                                'IllinoisRiver_zoom10_5_ggmap.rds'))
saveRDS(map_zoom10_6, file.path(onedrive_dir, 'SpatialData',
                                'IllinoisRiver_zoom10_6_ggmap.rds'))
saveRDS(map_zoom10_7, file.path(onedrive_dir, 'SpatialData',
                                'IllinoisRiver_zoom10_7_ggmap.rds'))
# 
# 
#load maps
# map_test <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                      'TonleSap_ggmap.rds'))
# map_upper <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                       'TonleSap_upper_ggmap.rds'))
# map_confluence <- readRDS(file = file.path(onedrive_dir, 'SpatialData',
#                                            'TonleSap_confluence_ggmap.rds'))
# map_lower <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 
#                                       'TonleSap_lower_ggmap.rds'))
# 