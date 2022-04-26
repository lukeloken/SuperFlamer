
# ##############################################################
# Read SuperFLAMe data, construct a single file
# Input is folder directory
# Output is a merged datatable of all SuperFlame Data
# ##############################################################

library(data.table)

ReadSuperFlame <- function(dir){
  rawdir <- paste(dir, "RawData", sep = "/")
  files <- list.files(rawdir)
  if (length(files) ==0) {
    stop("No files in 'dir/RawData' subdirectory")}
  
  # Merge data files that contain these strings in the file name
  # Load files in this order
  patterns <- c('GPS', 'Public', 'EXO', 'GGA', 'SUNA', 'Turner', 'EchoMap')
  loadfiles <- c(files[grep(patterns[1], files)],
                 files[grep(patterns[2], files)],
                 files[grep(patterns[3], files)],
                 files[grep(patterns[4], files)],
                 files[grep(patterns[5], files)],
                 files[grep(patterns[6], files)],
                 files[grep(patterns[7], files)])
  
  # patterns <- c('Public')
  # loadfiles <- c(files[grep(patterns[1], files)])
  
  
  if (length(loadfiles) == 0) {
    stop("'dir/RawData' does not contain correct files (e.g., '..._GPS.dat')")}
  
  # Get header names
  header.list <- lapply(paste(rawdir, loadfiles, sep="/"), fread, sep=",", skip=1, nrows=0)
  # Get data
  import.list <- lapply(paste(rawdir, loadfiles, sep="/"), fread, sep=",", skip=4, header=F, stringsAsFactors=F, na.strings=c('NA', 'NaN', '', 'null', '"NAN"'))
  
  # Put header names on correct data frame
  for (df in 1:length(import.list)){
    names(import.list[[df]])<- names(header.list[[df]])
  }
  
  # Omit 'Records' column
  import.list <- lapply(import.list, function(l) l[,-c('RECORD')])
  
  #Check for names of columns. Use public only if table is missing
  names(import.list)
  nonpublic.names <- lapply(import.list[-grep("Public", loadfiles)], 
                            function(l) names(l)) %>%
    unlist() %>%
    unique()
  
  #Drop TIMESTAMP as this is the merger name
  nonpublic.names <- nonpublic.names[-which(nonpublic.names == "TIMESTAMP")]
  
  public.df <-  import.list[[grep("Public", loadfiles)]] 
  
  public.names <- names(public.df)
  
  drop_names <- intersect(public.names, nonpublic.names)
  
  import.list[[grep("Public", loadfiles)]] <- import.list[[grep("Public", loadfiles)]]   %>%
    # select(TIMESTAMP, contains("String"), contains("status")) %>%
    # select(-data_status) %>%
    select(-drop_names) %>%
    mutate(TIMESTAMP = round_date(TIMESTAMP, "secs")) %>%
    group_by(TIMESTAMP) %>%
    mutate(across(everything(), ~unique(.x)[1])) %>%
    ungroup()
  
  
  
  #Merge datatables using TIMESTAMP
  my.df <- Reduce(function(x, y) full_join(x, y, by = "TIMESTAMP"), 
                  import.list, accumulate = FALSE) 
  
  if (nrow(my.df) == 0){
    stop("Merged data table has zero rows")}
  
  #Set Date_Time as time (need to check timezone)
  my.df$date_time <- as.POSIXct(my.df$TIMESTAMP, format='%Y-%m-%d %H:%M:%S', tz='UTC')
  
  # Set Lat/Long to decimal degrees rather than ddmm.mmmm (likely delete after July 5, 2017)
  # my.df[,c('longitude', 'latitude')]<-lapply(my.df[,c('longitude', 'latitude')], ConvertToDD)
  
  return(my.df)
  
}


