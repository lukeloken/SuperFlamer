
library(tidyr)
library(dplyr)
library(lubridate)
multi_merge_name <- "Merged_TonleSap_Jan2022_Jan2023"

home_path <- "C:/Users/lloken/OneDrive - DOI/FLAMebodia"


geo_merge <- readRDS(file.path(home_path, "Data", multi_merge_name, "Shapefiles",
                             paste0(multi_merge_name, "_", "Shapefile_AllData.rds")))

str(geo_merge)

geo_min <- geo_merge@data %>% 
  summarize(across(where(is.numeric) | where(is.POSIXct),
                   \(x) min(x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), ~signif(.x, 4))) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "Field", values_to = "Minimum")

geo_max <- geo_merge@data %>% 
  summarize(across(where(is.numeric) | where(is.POSIXct),
                   \(x) max(x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), ~signif(.x, 4))) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "Field", values_to = "Maximum")

geo_minmax <- data.frame(Field = names(geo_merge)) %>%
  left_join(geo_min, by = "Field") %>%
  left_join(geo_max, by = "Field")

# write.csv(geo_minmax, file.path(home_path, "Data Publication", "geo_minmax.csv"), row.names = FALSE)


#spot to join the data.dictionary
#read()
#maniuplate
#join
#check somehow, are all columns in data dictionary in data and vice versa?
#export



# write.csv(flame_meta, file.path(home_path, "Data Publication", "flame_meta.csv"), row.names = FALSE)

