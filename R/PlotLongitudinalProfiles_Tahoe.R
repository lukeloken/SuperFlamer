
#Warning this script uses your API key. Do not use this a lot as you can get charged if you go over your monthly allotment. 

library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(RColorBrewer)
library(RcppRoll)
library(ggpubr)

# source('R/read_excel_allsheets.R')
# source('R/g_legend.R')
# source('R/lightmodel.R')
# source('R/ImageScale.R')

#As of Oct 2021
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMebodia'

data_dir <- file.path(onedrive_dir, "Data", "2021-10-03_LakeTahoe")
data_dir1 <- file.path(onedrive_dir, "Data", "2021-10-02_LakeTahoe")

list.files(data_dir)



#Load spatial libraries
library(rgdal)
# library(gtools)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(riverdist)


# ##################################################################################################
# Google maps
# Commented code will load API key on Loken-UC-Davis laptop
# Download 3 maps from google server
# As of Oct 2018, this costs money
# Instead, I saved these maps to the dropbox spatial folder and load them individually
# If needed you can uncomment and redownload maps, but shouldn't need to do so with ship channel
# ##################################################################################################

#GoogleKey
# GoogleAPIkey<-unlist(read.delim("H:/Private/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))
# 
# register_google(key = as.character(GoogleAPIkey))
# 
# # Google background map
# map<-GetMap(center = c(39.09, -120.04), size=c(640,640), zoom=12, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)


# ggmap
# map_test<-get_googlemap(center = c(-120.04, 39.09), size = c(640,640), 
#                         zoom = 11, scale=2, maptype = "satellite", key=GoogleAPIkey )
# 
# ggmap(map_test)

#stamenmap
# map_stamen<-get_stamenmap(bbox=c(left=-121.61, right=-121.53, bottom=38.45, top=38.57), zoom = 12, scale=2, maptype = "terrain-background")
# ggmap(map_stamen)

#save maps
# saveRDS(map, file=paste0(dropbox_dir, '/Data/SpatialData/UpperShipChannel_map1.rds'))
# saveRDS(map2, file=paste0(dropbox_dir, '/Data/SpatialData/UpperShipChannel_map2.rds'))
# saveRDS(map_test, file.path(onedrive_dir, 'SpatialData', 'Tahoe_ggmap.rds'))

# As of Nov 2019, load maps from file instead
#load maps
map_test <- readRDS(file = file.path(onedrive_dir, 'SpatialData', 'Tahoe_ggmap.rds'))

#In Feb 2020 this map has an error. Two images plot side by side. 
ggmap(map_test)


#UTM zone 10 for linear reference
projection = "+init=epsg:26910"

geographic = "+init = epsg:4269"

#Cambodia Zone
projection = "+int=epsg:32648"

start_i <- as.POSIXct("2021-10-03 17:08:00", tz = "UTC")
end_i <-   as.POSIXct("2021-10-03 21:28:00", tz = "UTC")

start_1 <- as.POSIXct("2021-10-02 20:06:00", tz = "UTC")
end_1 <-   as.POSIXct("2021-10-02 22:02:00", tz = "UTC")


color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), rev(magma(5, begin=.25, end=.98))), bias=1)
colours = color.palette(12)



#List of spatial files in google drive folder
spatialfiles <- list.files(data_dir)
spatialfiles1 <- list.files(data_dir1)

# SUNAfiles<-spatialfiles[grep('SUNA', spatialfiles)]
RTMCfiles2 <- spatialfiles[grep('Public', spatialfiles)]
RTMCfiles1 <- spatialfiles[grep('Public', spatialfiles1)]


#Process RTMC (FLAME) data
#Make one big file with everything. 
RTMCfileslong2 <- file.path(file.path(data_dir, RTMCfiles2))
RTMC_list2 <- lapply(RTMCfileslong2, function (l) read.csv(l, header=F, skip=4, stringsAsFactors = F))

RTMC_names2 <- names(read.csv(RTMCfileslong2[1], header=T, skip=1))

RTMC_df2<-ldply(RTMC_list2, data.frame)

names(RTMC_df2)<-RTMC_names2

#Subset data
# RTMC_df <- RTMC_df[,c(1,4,5, 9:10, 17:33)]
# RTMC_df[,2:ncol(RTMC_df)] <- sapply(RTMC_df[,2:ncol(RTMC_df)], as.numeric)

RTMC_df2$Date <- as.Date(RTMC_df2$TIMESTAMP)
RTMC_df2$TIMESTAMP <- as.POSIXct(RTMC_df2$TIMESTAMP, "UTC", format="%Y-%m-%d %H:%M:%S")

RTMC_df2 <- RTMC_df2 %>%
  filter(TIMESTAMP > start_i, TIMESTAMP < end_i)



RTMCfileslong <- file.path(file.path(data_dir1, RTMCfiles1))
RTMC_list <- lapply(RTMCfileslong, function (l) read.csv(l, header=F, skip=4, stringsAsFactors = F))

RTMC_names <- names(read.csv(RTMCfileslong[1], header=T, skip=1))

RTMC_df<-ldply(RTMC_list, data.frame)

names(RTMC_df)<-RTMC_names

RTMC_df$Date <- as.Date(RTMC_df$TIMESTAMP)
RTMC_df$TIMESTAMP <- as.POSIXct(RTMC_df$TIMESTAMP, "UTC", format="%Y-%m-%d %H:%M:%S")

RTMC_df <- RTMC_df %>%
  filter(TIMESTAMP > start_1, TIMESTAMP < end_1)

RTMC_df <- full_join(RTMC_df,RTMC_df2)


plotvars <- c("CH4_Dry", "CO2_Dry", "H2O",
              "no3_uM", 
              "temp", "specCond", "pH", "pressure",
              "chlor_RFU", "ODO_percent", "BGApc_RFU", "turb_FNU",
              "fDOM_RFU", "tds")


#Data limits
datalimits = data.frame(var=plotvars, min=NA, max=NA)
datalimits$min = c(2, 50, 5000, 0, 0, 0, 1, -10, -3, 0, -10, -10, -10, -10)
datalimits$max = c(5000, 10000, 100000, 500, 100, 1000, 14, 20, 100, 300, 100, 30, 30, 30)
#Omit gps data that are

# plotvars<-names(RTMC_df)[-which(names(RTMC_df) %in% c("TIMESTAMP", "Latitude", "Longitude", "EXOpHmV"))]

#Convert to NA if outside datalimits
var_no<-2
for (var_no in 1:nrow(datalimits)){
  var_name<-as.character(datalimits$var[var_no])
  min<-datalimits[var_no, c('min')]
  max<-datalimits[var_no, c('max')]
  RTMC_df[which(RTMC_df[,var_name]>max | RTMC_df[,var_name]<min),var_name]<-NA
}

bubble_start <- as.POSIXct("2021-10-03 17:33", tz= "UTC") 
bubble_end <- as.POSIXct("2021-10-03 17:45", tz= "UTC")
RTMC_df$CH4_Wet[which(RTMC_df$TIMESTAMP < bubble_end & RTMC_df$TIMESTAMP > bubble_start)] <- NA
RTMC_df$CO2_Wet[which(RTMC_df$TIMESTAMP < bubble_end & RTMC_df$TIMESTAMP > bubble_start)] <- NA
RTMC_df$CH4_Dry[which(RTMC_df$TIMESTAMP < bubble_end & RTMC_df$TIMESTAMP > bubble_start)] <- NA
RTMC_df$CO2_Dry[which(RTMC_df$TIMESTAMP < bubble_end & RTMC_df$TIMESTAMP > bubble_start)] <- NA
RTMC_df$turb_FNU[which(RTMC_df$TIMESTAMP < bubble_end & RTMC_df$TIMESTAMP > bubble_start)] <- NA
RTMC_df$ODO_percent[which(RTMC_df$TIMESTAMP < bubble_end & RTMC_df$TIMESTAMP > bubble_start)] <- NA

RTMC_df$CH4_Dry[which(is.na(RTMC_df$H2O))] <- NA
RTMC_df$CO2_Dry[which(is.na(RTMC_df$H2O))] <- NA

#Get rid of entire rows when geo data are NA
RTMC_df_geo <- RTMC_df[is.finite(rowMeans(RTMC_df[,c("latitude", "longitude")])),]

summary(RTMC_df_geo)

#convert dataframe into a spatial object
geo<-RTMC_df_geo
coordinates(geo)<- ~ longitude + latitude

proj4string(geo) <- CRS("+init=epsg:4326")


# #Linear Reference (This takes a lot of computation!)
# geo_UTM<-spTransform(geo, CRS(projection))
# geo_snapped<-xy2segvert(x=coordinates(geo_UTM)[,1], y=coordinates(geo_UTM)[,2], rivers=SSCNetwork_clean)
# geo$LinearDist<-unlist(SSCNetwork_clean$cumuldist)[geo_snapped$vert]
# geo$LinearDist_km<-geo$LinearDist/1000
# attributes(geo$TIMESTAMP)$tzone <- 'America/Los_Angeles'
# 
# #Plot two variables to visualize linear reference
# plot(geo$LinearDist, geo$EXOSpCn, type='l')
# plot(geo$LinearDist_km, geo$NO3_uM, pch=16)
# 
# row=15
# median_list <- list()
# geo_points <- list()
# geo_datacoords <- bind_cols (geo@data, data.frame(geo@coords))
# for (row in 1:nrow(site_df)){
#   
#   median_list[[row]] <- filter(geo_datacoords, TIMESTAMP > site_df$DateTime_start[row] &
#                     TIMESTAMP < site_df$DateTime_end[row]) %>%
#     summarize_all(.funs=median, na.rm=T)
#   
# }
# 
# site_medians_df<-ldply(median_list, data.frame) %>%
#   dplyr::select(-Date, -Latitude, -Longitude) 
# names(site_medians_df)<-paste0("FLAMe_", names(site_medians_df))
# 
# 
# site_points_df <- ldply(median_list, data.frame) %>%
#   dplyr::select(Latitude, Longitude) 
# 
# site_df_withFlame<-bind_cols(site_df, site_points_df) %>%
#   bind_cols(site_medians_df)   
# 
# SiteLocations<- site_df_withFlame %>% 
#   group_by(Site) %>%
#   summarize(LinearDist = median(FLAMe_LinearDist, na.rm=T),
#             Latitude = median(Latitude, na.rm=T),
#             Longitude = median(Longitude, na.rm=T))
# 
# #Save
# write.table(site_df_withFlame, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'FlameSiteData.csv'), row.names=F, sep=',')
# saveRDS(site_df_withFlame , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'FlameSiteData.rds'))
# 
# write.table(SiteLocations, file =file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'SiteLocations.csv'), row.names=F, sep=',')
# saveRDS(SiteLocations , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SiteLocations.rds'))



#Plotting parameters
B<-100 #Number of color breaks
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

dates<-unique(geo$Date)
# dates<-dates[9]

# dates<-dates[length(dates)] #Just use last date if processing newest file
event_i<-1
plot=TRUE

dir.create(file.path(onedrive_dir, "Figures", "ggmap"), showWarnings = F)

dir.create(file.path(onedrive_dir, "Rdata", "LakeTahoe"), showWarnings = F)

dir.create(file.path(onedrive_dir, "OutputData", "LakeTahoe"), showWarnings = F)

event_i <- 1
for (event_i in 1:length(dates)){
  # date<-as.Date(unique(field_df$Date)[event_i])
  date = dates[event_i]

  geo_i <- geo
    
  # geo_i<-geo[geo$Date==date,]

  
  #Identify variables to plot  
  plotvars_i<-intersect(plotvars, names(geo_i))
  
  var_i=1
  #Loop through geo_i and plot each variable
  for (var_i in 1:length(plotvars_i)){
    name<-plotvars_i[var_i]
    if (is.numeric(geo_i@data[,name])==TRUE){
      a<-geo_i[!is.na(geo_i@data[,name]),]
      
      
      if (nrow(a)>0){
        # a$Col <- as.numeric(cut(a@data[,name],breaks = B))
        # a$Color<-colors[a$Col]
        
        # am$col_values<-col_values[1:nrow(am)]
        # am$color2<-color.palette(n=B)[am$col_values]

        if (plot==TRUE){

        #Plot single image of all data
       

        #GGMAP side by side, better quality

        commonTheme_map<-list(
          theme(axis.text.x=element_blank(), 
                axis.text.y=element_blank(), 
                axis.title.y=element_blank(), 
                axis.title.x=element_blank(), 
                axis.ticks=element_blank(), 
                plot.margin = unit(c(0, 0, 0, 0), "cm")),
          scale_colour_gradientn(colours = color.palette(n=B), 
                                 limits=range(a@data[,name], na.rm=T)),
          theme(legend.position = c(.02, .98), 
                legend.justification = c(0,1), 
                legend.background = element_rect(fill = 'white', colour='black'),
                legend.text=element_text(size=8),
                legend.title=element_text(size=10), 
                legend.key.height = unit(.4, "cm"),
                legend.key.width = unit(.8, "cm"), 
                panel.border=element_rect(fill=NA, colour='black'), 
                legend.direction="horizontal"),
          guides(colour=guide_colorbar(title.position = 'bottom', 
                                       title.hjust=0.5, 
                                       title=name, 
                                       ticks.colour = "black", 
                                       ticks.linewidth = 1))
        )


        map <- ggmap(map_test) +
          geom_point(aes_string(x = a$longitude, y = a$latitude, 
                                colour = as.character(name)), data = a@data, 
                     alpha = .2, size=3) +
          commonTheme_map 

        if (name == "CH4_Dry"){
          map <- map +
        scale_colour_gradientn(colours = color.palette(n=B), 
                               limits=range(a@data[,name], na.rm=T), trans = "log10")
        }
        
        print(map)
        
ggsave(file.path(onedrive_dir, "Figures", "Maps",
                 paste(date, '_', name, ".png", sep="")), 
       map, width = 6, height = 6, units = "in")
        
        }

        # 
        # #Stadler style figure
        # #GGMAP side by side, better quality
        # png(paste0(dropbox_dir, "/Figures/NutrientExperiment/LongitudinalProfilesStadlerStyle/", date, '_', name, ".png", sep=""), res=300, width=6,height=8, units="in")
        # 
        # image1<-ggmap(map_test) +
        #   geom_point(aes_string(x = am$Longitude, y = am$Latitude, colour = as.character(name)), data = am@data, alpha = .2, size=4) +
        #   commonTheme_map
        # 
        # image2<-ggplot(aes_string(y = "LinearDist_km", x = name, fill = "col_values"), data =am@data ) + 
        #   labs(x=name, y="Distance (km)") + 
        #   theme_classic2() + 
        #   theme(plot.margin=unit(c(0,.1,.1,.05), "in")) +
        #   theme(panel.border=element_rect(fill=NA, colour='black')) + 
        #   # theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
        #   scale_y_reverse(position = 'right', limits=c(16,2.8)) +
        #   theme(legend.position='none') + 
        #   geom_point(color=am@data$color2, size=2)
        #   # scale_colour_manual(values = color.palette(n=B))
        # # scale_colour_gradientn(colours = color.palette(n=B), limits=range(c(am@data[,name], pm@data[,name]), na.rm=T))
        #   
        # grid.arrange(image1, image2, ncol=2, widths=c(2,1))
        # 
        # dev.off()

      }
      }
    }
  
  print(date)
}




