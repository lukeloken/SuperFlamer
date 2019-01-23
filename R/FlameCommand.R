# ##########################################################################
# Flame Command
# Set data diretory and run all flame scripts
# 1) Data files (.dat) from superflame computer (e.g., Site_Date_GPS.dat)
# 2) meta tables (.csv) from access (e.g., FlameMetaDate.csv)
# ##########################################################################

closeAllConnections()

# If you need to install sensorQC use this command
# install.packages("sensorQC", repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"), dependencies = TRUE)

# dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'
dir<-'C:/Dropbox/FLAMe_2018/Data/2018-06-27_WhiteBirchLake'

source('R/RunAllSuperFlameScripts.R')

RunSuperFlame(dir, plotdiag=T)
