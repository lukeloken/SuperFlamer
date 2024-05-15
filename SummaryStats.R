
data_raw <- readRDS("C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/Merged_Illinois_May_2022_Jul_2023/Shapefiles/Merged_Illinois_May_2022_Jul_2023_Shapefile_AllData.rds")

library(dplyr)
library(tidyr)
library(ggplot2)

str(data_raw)


data_all <- data_raw@data %>%
  select(-contains("tau")) %>%
  select(-contains("hyd"))

data_datetime <- data_all %>%
  group_by(id) %>%
  summarize(min_datetime = min(date_time, na.rm = TRUE), 
            max_datetime = max(date_time, na.rm = TRUE), 
            n_obs = length(unique(date_time))) 

data_datetime$n_obs

data_median <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) median(x, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "median")

data_max <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) max(x, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "max")

data_min <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) min(x, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "min")

data_mean <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "mean")

data_q01 <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) quantile(x, 0.01, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "q01")

data_q99 <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) quantile(x, 0.99, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "q99")

data_q25 <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) quantile(x, 0.25, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "q25")

data_q75 <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) quantile(x, 0.75, na.rm = TRUE))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "q75")

data_n <- data_all %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), 
                   \(x) length(which(!is.na(x))))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(stat = "n")

data_join <- bind_rows(data_min, data_mean) %>%
  bind_rows(data_n) %>%
  bind_rows(data_max) %>%
  bind_rows(data_median) %>%
  bind_rows(data_q01) %>%
  bind_rows(data_q25) %>%
  bind_rows(data_q75) %>%
  bind_rows(data_q99) %>%
  left_join(data_datetime) %>%
  mutate(stat = factor(stat, c("datetime", "n", "mean", "median", "min", "q01", "q25", "q75", "q99", "max"))) %>%
  arrange(variable, stat) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(c('mean', 'median', 'min', 'q01', 'q25', 'q75', 'q99', 'max'), \(x) signif(x, 6))) %>%
  select(id, min_datetime, max_datetime, variable, everything())

filter(data_join, variable == "CH4uM") %>%
  data.frame()

write.csv(data_join, "flameillinois_stats_by_campagin.csv", row.names = FALSE)
