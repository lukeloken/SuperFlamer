

library(devtools)
# install_github("NEONScience/NEON-utilities/neonUtilities", force = TRUE, dependencies = TRUE)
# install_github("NEONScience/NEON-dissolved-gas/neonDissGas", force = TRUE, dependencies = TRUE)
library(neonDissGas)

source('R/CalculateKhPlummer.R')
source('R/CalculateKh.R')
source('R/ConvertGasesSuperFlame.R')
source('R/CalculateGasSaturation.R')

onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/FLAMeIllinois'

library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(egg)

raw_synoptic <- read_excel(file.path(onedrive_dir,
                                     "Data",
                                     "Syringe", 
                                     "qry_IRB_2022_gages_synoptics_CO2_CH4_11232022.xlsx"), 
                           sheet = 1, skip = 0)

synoptic <- raw_synoptic

#Equation to estimate pressure in Atm
# Pressure=(1-(.0000225577*Elevation))^5.25588

synoptic$Pressure_atm <- (1-(.0000225577*synoptic$Elevation_m))^5.25588

synoptic$Pressure_guess = "recorded"
synoptic$Pressure_guess[which(is.na(synoptic$Pressure_atm))] = "guessed"

synoptic$Temperature_guess = "recorded"
synoptic$Temperature_guess[which(is.na(synoptic$WaterTemp_C))] = "guessed"


#Guess pressure and temperature
synoptic$Pressure_atm[which(is.na(synoptic$Pressure_atm))] <- 
  mean(synoptic$Pressure_atm, na.rm = TRUE)

synoptic$WaterTemp_C[which(is.na(synoptic$WaterTemp_C))] <- 25

#convert CH4 units 
CH4kh = getKh(synoptic$WaterTemp_C+273.15,"CH4")
CH4atmsat = as.numeric(getSaturation(CH4kh, synoptic$Pressure_atm, "CH4"))

synoptic$CH4ppm_Loken <- as.numeric(synoptic$`CH4_umoles/L_oldserum`/CH4kh/synoptic$Pressure_atm)
synoptic$CH4Sat_Loken <- as.numeric(synoptic$`CH4_umoles/L_oldserum`/CH4atmsat*100)

#convert CO2 units
CO2kh=Kh_Plummer(synoptic$WaterTemp_C+273.15)
CO2atmsat=as.numeric(getSaturation(CO2kh, synoptic$Pressure_atm, "CO2"))

synoptic$CO2ppm_Loken <- as.numeric(synoptic$`CO2_umoles/L_oldserum`/CO2kh/synoptic$Pressure_atm)
synoptic$CO2Sat_Loken <- as.numeric(synoptic$`CO2_umoles/L_oldserum`/CO2atmsat*100)

names(synoptic)

plot1 <- ggplot(synoptic, aes(x = CO2_ppm_oldserum, y = CO2ppm_Loken)) +
  theme_bw() +
  geom_abline(size = 1) + 
  geom_point(aes(fill = Temperature_guess), shape = 21, size = 2, alpha = 0.7) +
  labs(fill = "Temperature") +
  theme(legend.position = c(0.02, 0.98), 
        legend.justification = c(0,1), 
        legend.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA)) +
  scale_x_continuous(limits = range(c(synoptic$CO2_ppm_oldserum,
                                      synoptic$CO2ppm_Loken), 
                                    na.rm = TRUE)) +
  scale_y_continuous(limits = range(c(synoptic$CO2_ppm_oldserum,
                                      synoptic$CO2ppm_Loken), 
                                    na.rm = TRUE)) +
  coord_equal()


plot2 <- ggplot(synoptic, aes(x = CH4_ppm_oldserum, y = CH4ppm_Loken)) +
  theme_bw() + 
  geom_abline(size = 1) + 
  geom_point(aes(fill = Temperature_guess), shape = 21, size = 2, alpha = 0.7) +
  labs(fill = "Temperature") +
  theme(legend.position = "none") +
  scale_x_continuous(limits = range(c(synoptic$CH4_ppm_oldserum,
                                      synoptic$CH4ppm_Loken), 
                                    na.rm = TRUE)) +
  scale_y_continuous(limits = range(c(synoptic$CH4_ppm_oldserum,
                                      synoptic$CH4ppm_Loken), 
                                    na.rm = TRUE)) +
  coord_equal()

plot_out <- egg::ggarrange(plots = list(plot1, plot2), nrow = 1)

print(plot_out)

ggsave(file.path(onedrive_dir, "Figures", "Synoptic_ppm.png"),
       plot_out, height = 3, width = 6, units = "in")

openxlsx::write.xlsx(synoptic, file.path(onedrive_dir,
                                         "Data",
                                         "Syringe", 
                                         "qry_IRB_2022_gages_synoptics_CO2_CH4_11232022_LCL.xlsx"))
