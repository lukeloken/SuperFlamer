dir = file.path(processed_path, "2_flame_snapped")

all_files <- list.files(path = dir, pattern = "all_snapped.rds")

trips <- lapply(file.path(dir, all_files), st_read)
names(trips) = c("")