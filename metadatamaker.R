
home_path <- "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data"
geo_merge <- readRDS(file.path(home_path, multi_merge_name, "Shapefiles",
                             paste0(multi_merge_name, "_", "Shapefile_AllData.rds")))


flame_meta <- data.frame(column = names(geo_merge))

write.csv(flame_meta, file.path(home_path, "IllinoisRiver_meta.csv"), row.names = FALSE)


head(geo_merge)
