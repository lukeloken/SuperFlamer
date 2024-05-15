options(scipen=999) #This disables scientific notation for values 

library(stringr)
#inputFile <- "test_good.csv"
inputFile <- "public.csv"
inputFile <- "FLAME_Public.dat"

inputFile <- 'C:/Users/lloken/OneDrive - DOI/Flamebodia/Data/2022-01-18_TonleSapCentral/RawData/FLAME_Public.dat'


outputFile <- "gga_good.csv"

is.blank <- function(var) {
  if (  is.na(var)||is.null(var)||is.nan(var)||(var=="NAN")||(var=="")||(var==" ") ) return (TRUE)
  else return (FALSE)
}


#Define an output dataframe
dfX <- data.frame(timeStamp=character(1),
                  commaCount=numeric(1),
                  ch4Wet=numeric(1),
                  h2o=numeric(1),
                  co2Wet=numeric(1),
                  dataGood=character(1),
                  stringsAsFactors=FALSE)
#Create a single row like dfX for temporary storage
tempX <- dfX  #Permanently empty template.
#Remove the empty row from dfX
dfX <- dfX[-1,] 


# #count lines
# f <- file(inputFile, open="rb")
# nlines <- 0L
# while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
#   nlines <- nlines + sum(chunk == as.raw(10L))
#   print(nlines)
# }
# close(f)


con <- file(inputFile, "rb")
lines = readLines(con)
close(con)
print(length(lines))
lines <- lines[1040:1060]
r = 1
for (r in 1:length(lines)) {
  str <- lines[r]
  numcommas <- str_count(str,pattern=",")
  tempX$commaCount <- numcommas
  str2 <- str_split(str,",")
  #message(str2[[1]][4]," ",str2[[1]][6]," ",str2[[1]][8])
  tempX$timeStamp <- str2[[1]][1]
  
  ch4Wet <- round(as.numeric(str2[[1]][4]),digits=1)
  if (is.blank(ch4Wet)) ch4Wet <- -1
  h2o <- round(as.numeric(str2[[1]][6]),digits=1)
  if (is.blank(h2o)) h2o <- -1
  co2Wet <- round(as.numeric(str2[[1]][8]),digits=1)
  if (is.blank(co2Wet)) co2Wet <- -1
  
  tempX$ch4Wet <- ch4Wet
  tempX$h2o<- h2o
  tempX$co2Wet <- co2Wet
  
  tempX$dataGood <- FALSE 
  
  if ((numcommas==396) && 
      (tempX$ch4Wet > 0.0) && (tempX$ch4Wet < 1000) &&
      (tempX$h2o > 20000) && (tempX$h2o < 100000) &&  
      (tempX$co2Wet > 400) && (tempX$co2Wet < 10000)){
    
      tempX$dataGood <- TRUE
  }

  dfX <- rbind(dfX,tempX)

}

#Write out CSV
write.table(dfX,file=outputFile,sep=",",col.names=TRUE,append=FALSE,quote=FALSE,row.names=FALSE,na="")