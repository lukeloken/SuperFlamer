library(sbtools)
library(dplyr)
library(sf)
library(ggplot2)
library(terra)
library(maptiles)

aqa_path <- "C:/Users/slafond-hudson/DOI/FLAMeM - General/Data/AquaticAreas"

output <- processed_path <- "C:/Users/slafond-hudson/OneDrive - DOI/SLL/flame/Processed_objects"

aqa10 <- st_read(file.path(aqa_path, "level1", "aqa_2010_p10_new"))
unique(aqa10$AQUA_CODE)

aqa10 <- aqa10 %>% 
  filter(!AQUA_CODE %in% c("N", "MNC", "CB"))

p10bth75 <- "C:/Users/slafond-hudson/OneDrive - DOI/SLL/GIS/p10bth75/p10bth75.tif"
bath <- rast(p10bth75)
basename(p10bth75)
sources(bath)
hasValues(bath)
plot(bath, main="Pool 10")

tmpfilter <- bath < 100
boat <- mask(bath, tmpfilter)
plot(boat)
#note: elevation & bathymetry is in cm. 

# base <- get_tiles(bath, project=TRUE)
# plotRGB(base)
aqa10 <- vect(aqa10)
p10 <- mask(bath, aqa10, inverse=FALSE)
plot(p10)
writeRaster(p10,file.path(output, "p10.grd"))
