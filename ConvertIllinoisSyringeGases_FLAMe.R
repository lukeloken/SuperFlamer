

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


chem.files <- list.files(file.path(onedrive_dir,
                        "Data",
                        "WaterChemistry"), 
                        full.names = TRUE)

gas.files <- chem.files[grepl("CO2_CH4", chem.files)]

gas_list <- lapply(gas.files, read_excel, skip=0, sheet = 1)

gas_df <- bind_rows(gas_list)



#Equation to estimate pressure in Atm
# Pressure=(1-(.0000225577*Elevation))^5.25588

gas_df$Pressure_atm <- gas_df$AtmPressure_hPa/1013.25

#convert CH4 units 
CH4kh = getKh(gas_df$WaterTemp_C+273.15,"CH4")
CH4atmsat = as.numeric(getSaturation(CH4kh, gas_df$Pressure_atm , "CH4"))

gas_df$CH4_ppm_oldserum_Loken <- as.numeric(gas_df$`CH4_umoles/L_oldserum`/CH4kh/gas_df$Pressure_atm)
gas_df$CH4_Sat_oldserum_Loken <- as.numeric(gas_df$`CH4_umoles/L_oldserum`/CH4atmsat*100)

gas_df$CH4_ppm_newserum_Loken <- as.numeric(gas_df$`CH4_umoles/L_ newserum`/CH4kh/gas_df$Pressure_atm)
gas_df$CH4_Sat_newserum_Loken <- as.numeric(gas_df$`CH4_umoles/L_ newserum`/CH4atmsat*100)

#convert CO2 units
CO2kh=Kh_Plummer(gas_df$WaterTemp_C+273.15)
CO2atmsat=as.numeric(getSaturation(CO2kh, gas_df$Pressure_atm, "CO2"))

gas_df$CO2_ppm_oldserum_Loken <- as.numeric(gas_df$`CO2_umoles/L_oldserum`/CO2kh/gas_df$Pressure_atm)
gas_df$CO2_Sat_oldserum_Loken <- as.numeric(gas_df$`CO2_umoles/L_oldserum`/CO2atmsat*100)

gas_df$CO2_ppm_newserum_Loken <- as.numeric(gas_df$`CO2_umoles/L_newserum`/CO2kh/gas_df$Pressure_atm)
gas_df$CO2_Sat_newserum_Loken <- as.numeric(gas_df$`CO2_umoles/L_newserum`/CO2atmsat*100)


names(gas_df)

plot1 <- ggplot(gas_df, aes(x = CO2_ppm_newserum, y = CO2_ppm_newserum_Loken)) +
  theme_bw() +
  geom_abline(size = 1) + 
  geom_point(shape = 21, size = 2, alpha = 0.7)


plot2 <- ggplot(gas_df, aes(x = CO2_ppm_oldserum, y = CO2_ppm_oldserum_Loken)) +
  theme_bw() + 
  geom_abline(size = 1) + 
  geom_point(shape = 21, size = 2, alpha = 0.7) 


plot3 <- ggplot(gas_df, aes(x = CH4_ppm_newserum, y = CH4_ppm_newserum_Loken)) +
  theme_bw() +
  geom_abline(size = 1) + 
  geom_point(shape = 21, size = 2, alpha = 0.7) 


plot4 <- ggplot(gas_df, aes(x = CH4_ppm_oldserum, y = CH4_ppm_oldserum_Loken)) +
  theme_bw() + 
  geom_abline(size = 1) + 
  geom_point(shape = 21, size = 2, alpha = 0.7) 

plot_out <- egg::ggarrange(plots = list(plot1, plot2, plot3, plot4), 
                           nrow = 2, top = "FLAMe Aug 2022")

print(plot_out)

ggsave(file.path(onedrive_dir, "Figures", "FLAMe_Aug_ppm.png"),
       plot_out, height = 6, width = 6, units = "in")


write.csv(gas_df, file.path(onedrive_dir,
                            "Data",
                            "WaterChemistry",
                            "Serums_Aug_2022.csv"), 
          row.names = FALSE)

          