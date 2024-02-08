# Extract river network coordinates
library(plyr)

network_clean <- readRDS(file.path(spatial_dir, "river_network.rds"))

IR <- ldply(network_clean[['lines']], rbind) %>%
  setNames(c("lat", "lon"))
IR <- st_as_sf(IR, coords=c("lat", "lon"), crs=projection)
# proj4string(IR) <- st_crs("+init=epsg:26915")
IR <- st_transform(IR, crs=4326)
st_crs(IR)
head(IR)

IR <- IR%>%
  mutate(long = unlist(map(IR$geometry,1)),
         lat = unlist(map(IR$geometry,2)))

write_csv(IR, file.path(processed_path, "IL_river_coordinates_1m.csv"))

####test that it works
# IR1 <- read_csv(file.path(processed_path, "IL_river_coordinates_1m.csv"))
# IR1 <- st_as_sf(IR1, coords=c("long", "lat"), crs=4326)
# 
# IR10 <- read_csv(file.path(processed_path, "IL_river_coordinates_10m.csv"))
# IR10 <- st_as_sf(IR10, coords=c("long", "lat"), crs=4326)
# 
# fig <- ggplot()+
#   geom_sf(data=IR1, aes(geometry = geometry), color="black", size=1)+
#   geom_sf(data=IR10, aes(geometry = geometry), color="red", size=1)+
#   theme_classic()
# print(fig)

