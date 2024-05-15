library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(riverdist)

#path
dir <- "C:/Users/slafond-hudson/DOI/WQP Prioritized Constituents Project - Task 2 Carbon and Nutrients in the IRB/Data"

#files
#Merged_Illinois_*/FLowline_interpolated*.shp
all_files <- list.files(path = dir, pattern = "Flowline_interpolated", recursive=TRUE)
load_files <- all_files[grepl(".shp", all_files)]
interp <- lapply(file.path(dir, load_files), st_read)
names(interp) <- c("Aug_2022", "Jul_2023", "Mar_2023", "May_2022", "Nov_2022")

df <- interp[["Jul_2023"]]
# head(df)

ggplot()+geom_sf(data=df, aes(geometry=geometry, color=CH4_Dry))+
  scale_color_continuous(type="viridis")+
  labs(title="Jul 2023")+
  theme_classic()
