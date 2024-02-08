#Aquatic areas and river distance command file

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(riverdist)

input_path <- "C:/Users/slafond-hudson/DOI/WQP Prioritized Constituents Project - Task 2 Carbon and Nutrients in the IRB/Data/Merged_Illinois_Jul_2023"
output_path <- "./figures"

date <- "Jul_2023"

source("./VisualizeAquaticAreasBoxplot.R")

points <- read.csv(file.path(input_path, "Flowline_interpolated_Jul_2023.csv"))

points['AQUA_DESC'][points['AQUA_DESC']==""] <- "Other"

geodata <- points %>%
  select("NO3_mgL",
         "temp_tau",
         "specCond_tau",
         "pH_tau",
         "turb_FNU_tau",
         "ODO_mgL_tau",
         "chlor_ugL_tau", 
         "BGApc_ugL_tau",
         "fDOM_QSU_tau",
         "CH4uM_tau", 
         "CO2uM_tau")

plot_aqa_boxplots(points, geodata, output_path, date)         
        