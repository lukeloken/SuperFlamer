# #######################################################################################
# Code to loop through 'Samples.csv' files and extract all sample Flame Values
# Output single file of samples (not aggregated)
# #######################################################################################



MergeSampleTables <- function(home_path, directories_merge, merge_name){
  
  # Load packages
  library(gtools)
  
  # home_path <- "C:/Users/lloken/OneDrive - DOI/Flamebodia/Data"
  # directories_merge
  # merge_name
  
  samples_out <- as.data.frame(matrix(nrow=0, ncol=0))
  
  dir <- directories_merge[1]
  for (dir in directories_merge){
    subdir <- file.path(home_path, dir, "ProcessedData")
    subdir_files <- list.files(subdir)
    
    file <- subdir_files[grep('_Samples.csv', subdir_files)]
    
    if (length(file)==1){
      data1 <- read.csv(file.path(subdir, file),
                        header=T, stringsAsFactors = F)
      
      data1 <- data1 %>%
        mutate(Sample.Number = as.character(Sample.Number))
      
      if (nrow(data1) == 0){
        next
      }
      if (nrow(samples_out) == 0){
        samples_out <- data1
      } else {
        samples_out <- bind_rows(samples_out, data1)
      }
    }
  }
  
  return(samples_out)
  
}
