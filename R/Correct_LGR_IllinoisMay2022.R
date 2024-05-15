

#merge lgr data for Illinois River May 2022
# issues with lgr data
# Using LGR1 for some of the days
# LGR3 did not log properly in datalogger for some of the days

library(data.table)

lgr1_dir <- "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files"
lgr3_dir <- "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR3_data/flow_files"

files1 <- list.files(lgr1_dir, full.names = TRUE)
files3 <- list.files(lgr3_dir, full.names = TRUE)

if (length(files1) == 0) {
  stop("No files in lgr1_dir")}
if (length(files3) == 0) {
  stop("No files in lgr3_dir")}

# Merge data files that contain these strings in the file name
# Load files in this order
loadfiles1 <- files1[grepl("_f000", files1)]
loadfiles3 <- files3[grepl("_f000", files3)]

lgr1_timetable <- data.frame(matrix(nrow = length(loadfiles1), ncol = 3))
names(lgr1_timetable) <- c("file", "t_lgr", "t_datalogger")
# lgr1_timetable$date <- seq.Date(as.Date("2022-05-02"), as.Date("2022-05-07"), "day")
lgr1_timetable$t_lgr <- as.POSIXct(NA, tz = "Etc/GMT+5")
lgr1_timetable$t_datalogger <- as.POSIXct(NA, tz = "Etc/GMT+5")

lgr3_timetable <- lgr1_timetable[1:length(loadfiles3),]

lgr1_timetable$file <- loadfiles1
lgr3_timetable$file <- loadfiles3

# lgr1_timetable[which(lgr1_timetable$date == as.Date("2022-05-03")),2] <-
#   as.POSIXct(x = c("1980-01-01 10:41:00"), tz = "America/Chicago")
# lgr1_timetable[which(lgr1_timetable$date == as.Date("2022-05-03")),3] <- 
# as.POSIXct(x = c("2022-05-03 00:51:00"), tz = "UTC")

lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_01Jan1980_f0003.txt"),2] <-
  as.POSIXct(x = c("1980-01-01 00:17:23"), tz="Etc/GMT+5")
lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_01Jan1980_f0003.txt"),3] <- 
  as.POSIXct(x = c("2022-05-03 10:05:00"), tz = "Etc/GMT+5")

lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_01Jan1980_f0004.txt"),2] <-
  as.POSIXct(x = c("1980-01-01 11:06:13"), tz = "Etc/GMT+5")
lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_01Jan1980_f0004.txt"),3] <- 
  as.POSIXct(x = c("2022-05-05 12:17:00"), tz = "Etc/GMT+5")

lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_01Jan1980_f0005.txt"),2] <-
  as.POSIXct(x = c("1980-01-01 14:20:43"), tz = "Etc/GMT+5")
lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_01Jan1980_f0005.txt"),3] <- 
  as.POSIXct(x = c("2022-05-06 07:41:00"), tz = "Etc/GMT+5")

lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_02Jan1980_f0000.txt"),2] <-
  as.POSIXct(x = c("1980-01-01 14:20:43"), tz = "Etc/GMT+5")
lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_02Jan1980_f0000.txt"),3] <- 
  as.POSIXct(x = c("2022-05-06 07:41:00"), tz = "Etc/GMT+5")


lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_02Jan1980_f0001.txt"),2] <-
  as.POSIXct(x = c("1980-01-02 01:11:51"), tz = "Etc/GMT+5")
lgr1_timetable[which(lgr1_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/flow_files/gga_02Jan1980_f0001.txt"),3] <- 
  as.POSIXct(x = c("2022-05-07 08:18:00"), tz = "Etc/GMT+5")

attr(lgr1_timetable$t_lgr, "tzone") <- "Etc/GMT+5"
attr(lgr1_timetable$t_datalogger, "tzone") <- "Etc/GMT+5"

lgr1_timetable <- lgr1_timetable %>%
  mutate(time_diff_s = as.numeric(difftime(t_datalogger, t_lgr, units = "secs")))

lgr3_timetable[which(lgr3_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR3_data/flow_files/gga_2022-05-02_f0000.txt"),2] <-
  as.POSIXct(x = c("2022-05-02 09:40:00"), tz = "America/Chicago")

lgr3_timetable[which(lgr3_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR3_data/flow_files/gga_2022-05-02_f0000.txt"),3] <- 
  as.POSIXct(x = c("2022-05-02 15:27:47"), tz = "UTC")


lgr3_timetable[which(lgr3_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR3_data/flow_files/gga_2022-05-04_f0001.txt"),2] <-
  as.POSIXct(x = c("2022-05-04 09:40:00"), tz = "America/Chicago")

lgr3_timetable[which(lgr3_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR3_data/flow_files/gga_2022-05-04_f0001.txt"),3] <- 
  as.POSIXct(x = c("2022-05-04 15:27:47"), tz = "UTC")


lgr3_timetable[which(lgr3_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR3_data/flow_files/gga_2022-05-05_f0000.txt"),2] <-
  as.POSIXct(x = c("2022-05-05 08:06:00"), tz = "America/Chicago")

lgr3_timetable[which(lgr3_timetable$file == "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR3_data/flow_files/gga_2022-05-05_f0000.txt"),3] <- 
  as.POSIXct(x = c("2022-05-05 13:53:46"), tz = "UTC")

# patterns <- c('Public')
# loadfiles <- c(files[grep(patterns[1], files)])


if (length(loadfiles1) == 0) {
  stop("No loadfiles in lgr1_dir")}

if (length(loadfiles3) == 0) {
  stop("No loadfiles in lgr3_dir")}

# Get data
lgr1_list <- lapply(loadfiles1, fread, sep=",", skip=1, 
                      header=TRUE, stringsAsFactors=F, 
                      na.strings=c('NA', 'NaN', '', 'null', '"NAN"'))
names(lgr1_list) <- loadfiles1

head(lgr1_list[[4]])
tail(lgr1_list[[4]])

i = 4
lgr1_list_corrected <- list()
for(i in seq_along(loadfiles1)){
  d_i <- lgr1_list[[i]]
  offset_s_i <- lgr1_timetable %>%
    filter(file == loadfiles1[i]) %>%
    pull(time_diff_s)
  
  d_out <- d_i %>%
    mutate(datetime_raw = as.POSIXct(Time, format = "%m/%d/%y %H:%M:%S", tz = "Etc/GMT+5")) %>%
    group_by(datetime_raw) %>%
    summarize(across(contains("_ppm"), ~mean(.x, na.rm = TRUE))) %>%
    arrange(datetime_raw) %>%
    ungroup() %>%
    mutate(TIMESTAMP = datetime_raw + offset_s_i) %>%
    select(TIMESTAMP, contains("datetime"), everything()) 
  
  lgr1_list_corrected[[i]] <- d_out
}

head(lgr1_list_corrected)

lgr1_df_corrected <- bind_rows(lgr1_list_corrected) %>%
  rename("CH4_Wet" = "[CH4]_ppm", 
         "CH4_Dry" = "[CH4_DRY]_ppm", 
         "CO2_Wet" = "[CO2]_ppm", 
         "CO2_Dry" = "[CO2_DRY]_ppm", 
         "H2O" = "[H2O]_ppm"
  )

lgr1_df_corrected$CH4_Dry[which(lgr1_df_corrected$CH4_Dry<0)] <- NA
lgr1_df_corrected$CH4_Wet[which(lgr1_df_corrected$CH4_Wet<0)] <- NA
lgr1_df_corrected$CO2_Dry[which(lgr1_df_corrected$CO2_Dry<0)] <- NA
lgr1_df_corrected$CO2_Wet[which(lgr1_df_corrected$CO2_Wet<0)] <- NA
lgr1_df_corrected$H2O[which(lgr1_df_corrected$H2O<0)] <- NA


names(lgr1_df_corrected)

head(lgr1_df_corrected)

ggplot(lgr1_df_corrected) +
  geom_point(aes(x = TIMESTAMP, y = CH4_Dry)) +
  scale_y_log10()

ggplot(lgr1_df_corrected) +
  geom_point(aes(x = TIMESTAMP, y = CO2_Dry)) +
  scale_y_log10()

saveRDS(lgr1_df_corrected, file = "C:/Users/lloken/OneDrive - DOI/FLAMeIllinois/Data/LGR1_data/CorrectedLGR1.rds")
