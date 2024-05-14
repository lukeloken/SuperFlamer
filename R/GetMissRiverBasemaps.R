

library(rgdal)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(riverdist)
library(viridis)

onedrive_dir <- "C:/Users/lloken/DOI/FLAMeM - General"


# Google maps
# Commented code will load Loken API key on USGS computer
# Download maps from google server
# Updated Jan 2022, Each download costs money, so save these somewhere to use
###############################################################################################

# GoogleKey
GoogleAPIkey <- unlist(read.delim("H:/Private/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))

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
# Pool 8 - upper
map_pool8_upper<-get_googlemap(center = c(-91.27, 43.84),
                        size = c(300,300),
                        zoom = 12,
                        scale = 2,
                        maptype = "satellite",
                        key=GoogleAPIkey )
# 
ggmap(map_pool8_upper)

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
saveRDS(map_pool8_upper, file.path(onedrive_dir, 'SpatialData', 'Pool8_upper_ggmap.rds'))
