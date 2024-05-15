

geo_merge <- readRDS(file.path(home_path, multi_merge_name, "Shapefiles",
                             paste0(multi_merge_name, "_", "Shapefile_AllData.rds")))


# geo_merge <- geo_merge %>%
#   select(-wiperPos)
# geo_merge$water_temp[which(geo_merge$date_time > as.POSIXct("2022-10-01 05:47:00", tz = "UTC") & 
#                              geo_merge$date_time < as.POSIXct("2023-01-01 00:00:00", tz = "UTC"))] <- NA
# 
# geo_merge$depth[which(geo_merge$date_time > as.POSIXct("2022-10-01 05:47:00", tz = "UTC") & 
#                              geo_merge$date_time < as.POSIXct("2023-01-01 00:00:00", tz = "UTC"))] <- NA
# 
geo_merge$temp_logger[which(geo_merge$temp_logger > 100)] <- NA
geo_merge$temp_logger[which(geo_merge$temp_logger <0.1)] <- NA

geo_merge$no3_uM[which(geo_merge$no3_uM > 200)] <- NA
geo_merge$nn03_mg[which(geo_merge$nn03_mg > 10)] <- NA

geo_merge$temp_int[which(geo_merge$temp_int > 100)] <- NA
geo_merge$temp_int[which(geo_merge$temp_int <0.1)] <- NA

geo_merge$temp_lamp[which(geo_merge$temp_lamp > 100)] <- NA
geo_merge$temp_lamp[which(geo_merge$temp_lamp <0.1)] <- NA

geo_merge$temp_spect[which(geo_merge$temp_spect > 100)] <- NA
geo_merge$temp_spect[which(geo_merge$temp_spect < 0.1)] <- NA

geo_merge$temp_spect[which(geo_merge$temp_spect > 100)] <- NA
geo_merge$temp_spect[which(geo_merge$temp_spect < 0.1)] <- NA


ggplot(data = geo_merge@data, aes(x = date_time, y = temp_logger)) + 
  geom_point() + 
  facet_wrap(~id, scales = "free")


ggplot(data = geo_merge@data, aes(x = date_time, y = CO2_Dry)) + 
  geom_point() + 
  facet_wrap(~id, scales = "free")

summary(geo_merge)

saveRDS(geo_merge, file.path(home_path, multi_merge_name, "Shapefiles",
                             paste0(multi_merge_name, "_", "Shapefile_AllData.rds")))

write.csv(geo_merge@data, file.path(home_path, multi_merge_name,
                                    paste0(multi_merge_name, "_AllData.csv")))



geo_min <- geo_merge@data %>%
  summarize(date_time = as.character(min(date_time, na.rm = TRUE)), 
            across(where(is.numeric), ~as.character(signif(min(.x, na.rm = TRUE), 4)))) %>%
  pivot_longer(everything(), names_to = "Field", values_to = "Minimum")

geo_max <- geo_merge@data %>%
  summarize(date_time = as.character(max(date_time, na.rm = TRUE)), 
            across(where(is.numeric), ~as.character(signif(max(.x, na.rm = TRUE), 4)))) %>%
  pivot_longer(everything(), names_to = "Field", values_to = "Maximum")

geo_long <- full_join(geo_min, geo_max) %>%
  mutate(Field = recode(Field, 
                        "no3_uM" = "NO3_uM", 
                        "nn03_mg" = "NO3_mgL"))

data.frame(geo_long)

head(geo_long)
write.csv(geo_long, file.path(home_path, multi_merge_name, "DataMinMax.csv"), row.names = FALSE)
