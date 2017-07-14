# ##########################################################################
# Flame Command
# Set data diretory and run all flame scripts
# 1) Data files (.dat) from superflame computer (e.g., Site_Date_GPS.dat)
# 2) meta tables (.csv) from access (e.g., FlameMetaDate.csv)
# ##########################################################################

dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'
dir<-'E:/Dropbox/FLAME_Light/Data/2017-07-12_TauTrout'

source('R/RunAllSuperFlameScripts.R')

RunSuperFlame(dir, tauplot=T)
