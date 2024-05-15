
library(openxlsx)
library(ggplot2)
library(dplyr)
library(lubridate)
source('R/RunAllSuperFlameScripts.R')

home_dir <- "C:/Users/lloken/OneDrive - DOI/GlobalRiverMethane"

file_yukon <- "qry_Yukon 2006-2008 serum CH4 data for Luke.xlsx"
file_all <- "qry_All Yukon query for Luke, 06-24-22.xlsx"
list.files(home_dir)
meth_all <- openxlsx::read.xlsx(file.path(home_dir, file_all), rows = 3:5000, colNames = FALSE)
meth_names <- names(openxlsx::read.xlsx(file.path(home_dir, file_all), rows = 1, colNames = TRUE))
meth_units <- openxlsx::read.xlsx(file.path(home_dir, file_all), rows = 2, skipEmptyCols = FALSE, colNames = FALSE)

meth_names_final <- paste(meth_names, meth_units, sep = "_")
meth_names_final <- gsub(" ", "_", meth_names_final)
meth_names_final <- gsub("\\.", "_", meth_names_final)
meth_names_final <- gsub("\\.", "_", meth_names_final)

meth_names_final

names(meth_all) <-meth_names_final
head(meth_all)
dim((meth_all))

meth_all <- meth_all %>%
  mutate(across(c(contains("CH4"), contains("CO2")), ~as.numeric(.x))) %>%
  mutate(Date = as.Date(SiteDate_NA, origin = "1899-12-30"), 
         year = year(Date))

ggplot(meth_all, aes(x = `CH4_umoles/L_30_mL_serum`, 
                     y = `CH4_ppm_30_mL_serum`, 
                     color = Project_NA)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~Project_NA) +
  theme(legend.position = "none")

ggplot(meth_all, aes(x = `CH4_umoles/L_evacuated_serum`, 
                     y = `CH4_ppm_evacuated_serum`, 
                     color = Project_NA)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10()


meth_all_luke <- meth_all %>%
  mutate(CH4kh = ifelse(is.na(WaterTemp_C_NA), getKh(10+273.15, "CH4"), getKh(WaterTemp_C_NA+273.15, "CH4")),
         CO2kh = ifelse(is.na(WaterTemp_C_NA), getKh(10+273.15, "CO2"), getKh(WaterTemp_C_NA+273.15, "CO2")),
         Pressure = ifelse(is.na(`Elevation(m)_NA`), 
                           1, 
                           (1-(.0000225577*`Elevation(m)_NA`))^5.25588)) %>%
  mutate(CH4uM_evacuatedserum_luke = as.numeric(CH4_ppm_evacuated_serum*CH4kh*Pressure), 
         CH4uM_30mLserum_luke = as.numeric(CH4_ppm_30_mL_serum*CH4kh*Pressure), 
         CH4ppm_evacuatedserum_luke = as.numeric(`CH4_umoles/L_evacuated_serum`/CH4kh/Pressure), 
         CH4ppm_30mLserum_luke = as.numeric(`CH4_umoles/L_30_mL_serum`/CH4kh/Pressure)
  ) %>%
  mutate(CO2uM_evacuatedserum_luke = as.numeric(CO2_ppm_evacuated_serum*CO2kh*Pressure), 
         CO2uM_30mLserum_luke = as.numeric(CO2_ppm_30_mL_serum*CO2kh*Pressure), 
         CO2ppm_evacuatedserum_luke = as.numeric(`CO2_umoles/L_evacuated_serum`/CO2kh/Pressure), 
         CO2ppm_30mLserum_luke = as.numeric(`CO2_umoles/L_30_mL_serum`/CO2kh/Pressure)
  )



head(meth_all_luke)

#original
ggplot(meth_all_luke, aes(x = `CH4_umoles/L_30_mL_serum`, 
                          y = `CH4_ppm_30_mL_serum`)) + 
  facet_wrap(~Project_NA, scales = "free") +
  geom_point()

ggplot(meth_all_luke, aes(x = `CH4_umoles/L_evacuated_serum`, 
                          y = `CH4_ppm_evacuated_serum`)) + 
  facet_wrap(~Project_NA, scales = "free") +
  geom_point()

ggplot(meth_all_luke, aes(x = `CO2_umoles/L_30_mL_serum`, 
                          y = `CO2_ppm_30_mL_serum`)) + 
  facet_wrap(~Project_NA, scales = "free") +
  geom_point()

ggplot(meth_all_luke, aes(x = `CO2_umoles/L_evacuated_serum`, 
                          y = `CO2_ppm_evacuated_serum`)) + 
  facet_wrap(~Project_NA, scales = "free") +
  geom_point()

#luke calculated um
ch4_um_limits = c(0.00001, max(c(meth_all_luke$CH4uM_30mLserum_luke, 
                           meth_all_luke$`CH4_umoles/L_30_mL_serum`), na.rm = TRUE))
ggplot(meth_all_luke, aes(x = `CH4_umoles/L_30_mL_serum`, 
                          y = `CH4uM_30mLserum_luke`)) + 
  scale_y_log10(limits = ch4_um_limits) +
  scale_x_log10(limits = ch4_um_limits) +
  coord_fixed() +
  facet_wrap(~Project_NA) +
  geom_abline() + 
  geom_point()

ggplot(meth_all_luke, aes(x = `CH4_umoles/L_evacuated_serum`, 
                          y = CH4uM_evacuatedserum_luke)) + 
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed() + 
  facet_wrap(~Project_NA) +
  geom_abline() + 
  geom_point() 

ggplot(meth_all_luke, aes(x = `CO2_umoles/L_30_mL_serum`, 
                          y = `CO2uM_30mLserum_luke`)) + 
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed() +
  facet_wrap(~year) +
  geom_abline() + 
  geom_point()

ggplot(meth_all_luke, aes(x = `CO2_umoles/L_evacuated_serum`, 
                          y = CO2uM_evacuatedserum_luke)) + 
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed() + 
  facet_wrap(~Project_NA) +
  geom_abline() + 
  geom_point() 

#luke calculated ppm
ch4_ppm_limits = c(0.1, max(c(meth_all_luke$CH4ppm_30mLserum_luke, 
                               meth_all_luke$`CH4_ppm_30_mL_serum`), 
                               na.rm = TRUE))
ggplot(meth_all_luke, aes(x = `CH4_ppm_30_mL_serum`, 
                          y = `CH4ppm_30mLserum_luke`)) + 
  geom_abline() + 
  scale_y_log10(limits = ch4_ppm_limits) +
  scale_x_log10(limits = ch4_ppm_limits) +
  coord_fixed() + 
  facet_wrap(~Project_NA, nrow = 5) + 
  geom_point() +
  labs(y = "CH4 (ppm) Luke calculated", 
       x = "CH4 (ppm) reported from 30 mL serums") +
  ggtitle("CH4 (ppm) 30 mL serums")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(meth_all_luke, aes(x = `CH4_ppm_30_mL_serum`, 
                          y = `CH4ppm_30mLserum_luke`)) + 
  geom_abline() + 
  scale_y_log10(limits = ch4_ppm_limits) +
  scale_x_log10(limits = ch4_ppm_limits) +
  facet_wrap(~year, nrow = 4) +  
  geom_point() +
  labs(y = "CH4 (ppm)- Luke calculated", 
       x = "CH4 (ppm)- reported from 30 mL serums") +
  ggtitle("CH4 (ppm) 30 mL serums") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

co2_ppm_limits = c(1, max(c(meth_all_luke$CO2ppm_30mLserum_luke, 
                            meth_all_luke$`CO2_ppm_30_mL_serum`), 
                            na.rm = TRUE))
ggplot(meth_all_luke, aes(x = `CO2_ppm_30_mL_serum`, 
                          y = `CO2ppm_30mLserum_luke`)) + 
  geom_abline() + 
  scale_y_log10(limits = co2_ppm_limits) +
  scale_x_log10(limits = co2_ppm_limits) +
  facet_wrap(~Project_NA, nrow = 5) + 
  geom_point() +
  labs(y = "CO2 (ppm)- Luke calculated", 
       x = "CO2 (ppm)- reported from 30 mL serums") +
  ggtitle("CO2 (ppm) 30 mL serums") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(meth_all_luke, aes(x = `CO2_ppm_30_mL_serum`, 
                          y = `CO2ppm_30mLserum_luke`)) + 
  geom_abline() + 
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~year, nrow = 4) + 
  geom_point() +
  labs(y = "CO2 (ppm)- Luke calculated", 
       x = "CO2 (ppm)- reported from 30 mL serums") +
  ggtitle("CO2 (ppm) 30 mL serums")

ggplot(meth_all_luke, aes(x = `CO2_ppm_evacuated_serum`, 
                          y = CO2ppm_evacuatedserum_luke)) + 
  geom_abline() + 
  facet_wrap(~Project_NA) + 
  geom_point() 

ggplot(meth_all_luke, aes(x = `CH4_ppm_evacuated_serum`, 
                          y = CH4ppm_evacuatedserum_luke)) + 
  geom_abline() + 
  # facet_wrap(~Project_NA) + 
  geom_point() 


summary(lm(CH4ppm_evacuatedserum_luke ~ CH4_ppm_evacuated_serum, data = meth_all_luke))
summary(lm(CH4ppm_30mLserum_luke ~ CH4_ppm_30_mL_serum, data = meth_all_luke))
