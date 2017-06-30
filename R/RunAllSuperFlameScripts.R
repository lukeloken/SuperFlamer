



dir<-'E:/Dropbox/FLAME_Light/Data/2017-06-27_LakeMendota'


source('R/MergeSuperFlameTables.R')
#Test run

data<-ReadSuperFlame(dir)

str(data)

ReadSuperFlame(dir2)
