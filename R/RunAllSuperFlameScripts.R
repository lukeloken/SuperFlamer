


#Set data directory
# 'RawData' folder needs to be within this folder and contain .dat files
dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'

#Load functions
source('R/MergeSuperFlameTables.R')

#Get and merge all raw data
data<-ReadSuperFlame(dir)
str(data)

