

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

raw_ch4 <- read_excel(file.path(onedrive_dir, 
                                "Methods", 
                                "IRB CH4 calcs by syringe equilibration_LL.xlsx"), 
                   sheet = 1, skip = 3)

raw_co2 <- read_excel(file.path(onedrive_dir, 
                                "Methods", 
                                "IRB Syringe Equilibration Calcs.xlsx"), 
                      sheet = 1, skip = 11)


select_ch4 <- raw_ch4 %>%
  select(Date, Location, Rep, PA = `Pa (Boulder)`, 
         headspaceTemp = `DEG C`, CH4_ppm = PPM) %>%
  mutate(barometricPressure = PA/1000)

air_CH4 <- select_ch4 %>% 
  filter(Rep == "Air") %>%
  group_by(Date, Location) %>%
  summarize(airCH4 = mean(CH4_ppm))

water_CH4 <- select_ch4 %>% 
  filter(Rep != "Air") %>%
  group_by(Date, Location, barometricPressure, headspaceTemp, Rep) %>%
  summarize(eqCH4 = mean(CH4_ppm)) %>%
  mutate(KPa = 100)
  

# join_CH4 <- left_join(water_CH4, air_CH4) %>%
#   mutate(volGas = 30, 
#          volH2O = 30, 
#          eqCO2 = 1000, 
#          airCO2 = 400, 
#          eqN2O = 1, 
#          airN2O = 0.01)
# 
# calculated_CH4 <- calc.syringe.conc.LL(inputFile = join_CH4) %>%
#   select(-contains("CO2"), -contains("N2O"))
# 
# summary(calculated_CH4$dissolvedCH4_M*1000000)




raw_syringe <- read_excel(file.path(onedrive_dir, 
                                    "Data", "Syringe", 
                                    "Neon Gas Calc Input File for Flame samples, May 2022.xlsx"), 
                          sheet = 1)


prep_syringe <- raw_syringe %>% 
  rename(headspaceTemp = EquilTempC, 
         waterTemp = WaterTempC, 
         volGas = gasVolume,
         volH2O = waterVolume, 
         eqCO2 = concentrationCO2Gas, 
         airCO2 = concentrationCO2Air, 
         eqCH4 = concentrationCH4Gas, 
         airCH4 = concentrationCH4Air) %>%
  mutate(eqN2O = 1, 
         airN2O = 0.01)
  
calc_syringe <- calc.syringe.conc.LL(inputFile = prep_syringe) %>%
  select(-contains("N2O")) %>%
  mutate(across(c("dissolvedCO2_M", "dissolvedCH4_M"), ~.x*1000000, 
                .names = "{.col}_micro")) %>%
  mutate(kh_CH4 = getKh(WaterTempK, "CH4")/101.325, 
         kh_CO2 = getKh(WaterTempK, "CO2")/101.325,
         CO2_ppm_corrected = dissolvedCO2_M/kh_CO2/barometricPressure*1000000,
         CH4_ppm_corrected = dissolvedCH4_M/kh_CH4/barometricPressure*1000000
  )

names(calc_syringe) <- gsub("_M_micro", "_uM", names(calc_syringe))

data.frame(calc_syringe)

openxlsx::write.xlsx(calc_syringe, file = file.path(onedrive_dir, 
                                                    "Data", "Syringe", 
                                                    "Calculated Neon Gas Calc Input File for Flame samples, May 2022.xlsx"))

plot_syringe <- calc_syringe %>% 
  select(`Master#`, Site, Date, 
         dissolvedCH4_uM, dissolvedCO2_uM, 
         CH4_ppm_corrected, CO2_ppm_corrected) %>%
  group_by(`Master#`, Site, Date) %>%
  summarize(mean_CH4umol_Neon = mean(dissolvedCH4_uM, na.rm = TRUE), 
            mean_CO2umol_Neon = mean(dissolvedCO2_uM, na.rm = TRUE),
            mean_CH4ppm_Neon = mean(CH4_ppm_corrected, na.rm = TRUE),
            mean_CO2ppm_Neon = mean(CO2_ppm_corrected, na.rm = TRUE)) 


plot_ch4 <- raw_ch4 %>%
  filter(Rep != "Air") %>%
  select(`Master#` = `Master #`, 
         Date, Site = Location, 
         CH4_umol = `umol/L CH4...13`, 
         CH4_ppm_orig = Water...15,
         CH4_ppm_new = `ppm`) %>%
  group_by(`Master#`, Date, Site) %>%
  summarize(mean_CH4umol_Boulder = mean(CH4_umol, na.rm = TRUE), 
            mean_CH4ppm_orig_Boulder = mean(CH4_ppm_orig, na.rm = TRUE), 
            mean_CH4ppm_new_Boulder = mean(CH4_ppm_new, na.rm = TRUE)
  )
  # mutate(type = "Boulder")
  # full_join(select(plot_syringe, -mean_CO2umol)) %>%
  # pivot_wider(names_from = type, values_from = mean_CH4umol)

# plot_ch4_ppm <- raw_ch4 %>%
#   filter(Rep != "Air") %>%
#   select(`Master#` = `Master #`, 
#          Date, Site = Location, 
#          CH4_ppm = ppm) %>%
#   group_by(`Master#`, Date, Site) %>%
#   summarize(mean_CH4ppm = mean(CH4_ppm, na.rm = TRUE)) %>%
#   mutate(type = "Boulder") %>%
#   pivot_wider(names_from = type, values_from = mean_CH4ppm)

plot_co2 <- raw_co2 %>%
  filter(Rep != "Air") %>%
  select(`Master#`, 
         Date, Site, 
         CO2_umol = `Corr. CO2 (umol/L) in lake water`, 
         CO2_ppm_orig = `Corr. CO2 (ppm)`) %>%
  group_by(`Master#`, Date, Site) %>%
  summarize(mean_CO2umol_Boulder = mean(CO2_umol, na.rm = TRUE), 
            mean_CO2ppm_orig_Boulder = mean(CO2_ppm_orig, na.rm = TRUE))
  # mutate(type = "Boulder") %>%
  # full_join(select(plot_syringe, -mean_CH4umol)) %>%
  # pivot_wider(names_from = type, values_from = mean_CO2umol)


sample_crosswalk <- read_excel(file.path(onedrive_dir, 
                                "Data", "Syringe",
                                "Flame_Boulder_SampleCrosswalk.xlsx"), 
                      sheet = 1) %>%
  select(`Master#` = `Master #`, sample_id)


samples_merged <- read.csv(file = file.path(onedrive_dir, "Data",
                                            "Merged_Illinois_May_2022",
                                             "Merged_Illinois_May_2022_Samples.csv")) 

flame_gas <- samples_merged %>%
  left_join(sample_crosswalk) %>%
  select(`Master#`, 
         CH4umol_FLAMe = CH4uM,
         CH4ppm_FLAMe = CH4_Dry,
         CO2umol_FLAMe = CO2uM,
         CO2ppm_FLAMe = CO2_Dry)

# flame_ch4_ppm <- samples_merged %>%
#   left_join(sample_crosswalk) %>%
#   select(`Master#`, FLAMe = CH4_Dry)
# 
# 
# flame_co2 <- samples_merged %>%
#   left_join(sample_crosswalk) %>%
#   select(`Master#`, FLAMe = CO2uM)
# 
# flame_co2_ppm <- samples_merged %>%
#   left_join(sample_crosswalk) %>%
  # select(`Master#`, FLAMe = CO2_Dry)

# plot_ch4 <- full_join(plot_ch4, flame_ch4)
# plot_co2 <- full_join(plot_co2, flame_co2)

# plot_ch4_ppm <- full_join(plot_ch4_ppm, flame_ch4_ppm)
# plot_co2_ppm <- full_join(plot_co2_ppm, flame_co2_ppm)

plot_all <-  full_join(plot_syringe, flame_gas) %>%
  full_join(plot_ch4) %>%
  full_join(plot_co2) %>%
  ungroup()

CH4umol_limits = range(select(plot_all, contains("CH4umol")), na.rm = TRUE)
CO2umol_limits = range(select(plot_all, contains("CO2umol")), na.rm = TRUE)
CH4ppm_limits = range(select(plot_all, contains("CH4ppm")), na.rm = TRUE)
CO2ppm_limits = range(select(plot_all, contains("CO2ppm")), na.rm = TRUE)

names(plot_all)

p1 <- ggplot(plot_all, aes( x = mean_CH4umol_Boulder, y = mean_CH4umol_Neon)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Boulder (uM)", 
       y = "CH4 Neon (uM)") +
  coord_fixed() +
  scale_x_log10(limits = CH4umol_limits) +
  scale_y_log10(limits = CH4umol_limits)

p2 <- ggplot(plot_all, aes( x = mean_CH4umol_Boulder, y = CH4umol_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Boulder (uM)", 
       y = "CH4 FLAMe (uM)") +
  coord_fixed() +
  scale_x_log10(limits = CH4umol_limits) +
  scale_y_log10(limits = CH4umol_limits)

p3 <- ggplot(plot_all, aes( x = mean_CH4umol_Neon, y = CH4umol_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Neon (uM)", 
       y = "CH4 FLAMe (uM)") +
  coord_fixed() +
  scale_x_log10(limits = CH4umol_limits) +
  scale_y_log10(limits = CH4umol_limits)

library(egg)
egg::ggarrange(plots = list(p1, p2, p3))


p4 <- ggplot(plot_all, aes( x = mean_CO2umol_Boulder, y = mean_CO2umol_Neon)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CO2 Boulder (uM)", 
       y = "CO2 Neon (uM)") +
  coord_fixed() +
  scale_x_log10(limits = CO2umol_limits) +
  scale_y_log10(limits = CO2umol_limits)

p5 <- ggplot(plot_all, aes( x = mean_CO2umol_Boulder, y = CO2umol_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CO2 Boulder (uM)", 
       y = "CO2 FLAMe (uM)") +
  coord_fixed() +
  scale_x_log10(limits = CO2umol_limits) +
  scale_y_log10(limits = CO2umol_limits)

p6 <- ggplot(plot_all, aes( x = mean_CO2umol_Neon, y = CO2umol_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CO2 Neon (uM)", 
       y = "CO2 FLAMe (uM)") +
  coord_fixed() +
  scale_x_log10(limits = CO2umol_limits) +
  scale_y_log10(limits = CO2umol_limits)



fullfig <- egg::ggarrange(plots = list(p1, p2, p3, p4, p5, p6), 
                          ncol = 2, byrow = FALSE, 
                          top = "Illinois River - May 2022")

ggsave(file.path(onedrive_dir, "Figures", "CO2CH4_uM_MethodCompare_6panel.png"), 
       fullfig)

#Samething ppm
p1_ppm <- ggplot(plot_all, aes( x = mean_CH4ppm_orig_Boulder, y = mean_CH4ppm_Neon)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Boulder original (ppm)", 
       y = "CH4 Neon (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CH4ppm_limits) +
  scale_y_log10(limits = CH4ppm_limits)

p2_ppm <- ggplot(plot_all, aes( x = mean_CH4ppm_orig_Boulder, y = CH4ppm_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Boulder original (ppm)", 
       y = "CH4 FLAMe (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CH4ppm_limits) +
  scale_y_log10(limits = CH4ppm_limits)

p3_ppm <- ggplot(plot_all, aes( x = mean_CH4ppm_Neon, y = CH4ppm_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Neon (ppm)", 
       y = "CH4 FLAMe (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CH4ppm_limits) +
  scale_y_log10(limits = CH4ppm_limits)

#Samething ppm
p4_ppm <- ggplot(plot_all, aes( x = mean_CH4ppm_orig_Boulder, y = mean_CH4ppm_new_Boulder)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Boulder original (ppm)", 
       y = "CH4 Boulder new (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CH4ppm_limits) +
  scale_y_log10(limits = CH4ppm_limits)

p5_ppm <- ggplot(plot_all, aes( x = mean_CH4ppm_new_Boulder, y = CH4ppm_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CH4 Boulder new (ppm)", 
       y = "CH4 FLAMe (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CH4ppm_limits) +
  scale_y_log10(limits = CH4ppm_limits)

p6_ppm <- ggplot(plot_all, aes( y = mean_CH4ppm_Neon, x = mean_CH4ppm_new_Boulder)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(y = "CH4 Neon (ppm)", 
       x = "CH4 Boulder new (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CH4ppm_limits) +
  scale_y_log10(limits = CH4ppm_limits)

library(egg)
egg::ggarrange(plots = list(p1_ppm, p2_ppm, p4_ppm, p6_ppm, p3_ppm,  p5_ppm), byrow = FALSE)


p7_ppm <- ggplot(plot_all, aes( x = mean_CO2ppm_orig_Boulder, y = mean_CO2ppm_Neon)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CO2 Boulder orig (ppm)", 
       y = "CO2 Neon (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CO2ppm_limits) +
  scale_y_log10(limits = CO2ppm_limits)

p8_ppm <- ggplot(plot_all, aes( x = mean_CO2ppm_orig_Boulder, y = CO2ppm_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CO2 Boulder orig (ppm)", 
       y = "CO2 FLAMe (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CO2ppm_limits) +
  scale_y_log10(limits = CO2ppm_limits)

p9_ppm <- ggplot(plot_all, aes( x = mean_CO2ppm_Neon, y = CO2ppm_FLAMe)) +
  theme_bw() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "CO2 Neon (ppm)", 
       y = "CO2 FLAMe (ppm)") +
  coord_fixed() +
  scale_x_log10(limits = CO2ppm_limits) +
  scale_y_log10(limits = CO2ppm_limits)



fullfig_ppm <- egg::ggarrange(plots = list(p1_ppm, p2_ppm, p3_ppm,
                                       p4_ppm, p5_ppm, p6_ppm, 
                                       p7_ppm, p8_ppm, p9_ppm), 
                          ncol = 3, byrow = FALSE, 
                          top = "Illinois River - May 2022")

ggsave(file.path(onedrive_dir, "Figures", "CO2CH4_ppm_MethodCompare_9panel.png"), 
       fullfig_ppm)
