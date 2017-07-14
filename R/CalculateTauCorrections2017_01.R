
# Code is used to determine hydro and tau corrections for FLAMe data.
# Input is merged data frame (data) and log file (log) indicating times of switching between sources. For this example 'lake' was first.
# Columns of 'data' need to be in the same order as manual tau/hydro entry (L50-70)

rm(list=ls(all=TRUE))
library(data.table)
library(caTools)

dir<-'E:/Dropbox/FLAME_Light/Data/2017-07-12_TauTrout'

data<-fread(paste(dir, "/ProcessedData/2017-07-12_TauTrout_03_Trimmed.csv", sep=""), header=TRUE)
data$date_time<-as.POSIXct(data$date_time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
data2<-data[,c(1,9:11, 13:21, 23:29)]

log<-fread(paste(dir, "/TauFieldLog.csv", sep=""), header=TRUE)
log$DateTime<-as.POSIXct(paste(log$Date, log$Time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz="UTC")

p=6 # number of plateaus (This should be = nrow(log)-2 )

# Plot simple figures of all variable timeseries

columns<-ncol(data2)

png(paste(dir, "/TauFigures/TauPlateaus2016.png", sep=""), res=600, width=20, height=20, units="in")

par(mar=c(1,4,0,0),bg=NA)
par(mfrow=c(ceiling(columns/4),4)) 
par( oma = c( 3,0,1,0 ))
par(tck=-.02)
par(mgp=c(0,.6,0))
par(las=0)

for (i in 2:ncol(data2)){
  name=colnames(data2)[i]
  if (is.numeric(data2[[name]])){
    plot(data2[[1]], data2[[name]], xaxt="n", ylab="", xlab="")
    mtext(name, 2, 2)
  }
}

dev.off()


# =====================================
# Manual change c(hydro, tau) (seconds)
# =====================================

temp<-c(7,6)
specCond<-c(7,8)
pH<-c(7,4)
chlor_ugL<-c(6,5)
chlor_RFU<-c(6,5)
ODO_percent<-c(7,8)
ODO_mgL<-c(7,8)
BGApc_RFU<-c(6,5)
BGApc_ugL<-c(6,5)
turb_FNU<-c(7,4)
fDOM_RFU<-c(8,2)
fDOM_QSU<-c(8,2)

CH4<-c(13,150)
H2O<-c(13,100)
CO2<-c(13,14)

no3_uM<-c(1,1)
nn03_mg<-c(1,1)
abs254<-c(1,1)
abs350<-c(1,1)

manual_hydro<-data.frame(temp,specCond,pH, chlor_ugL,
                         chlor_RFU,ODO_percent, ODO_mgL,BGApc_RFU,
                         BGApc_ugL,turb_FNU,fDOM_RFU,fDOM_QSU,
                         CH4,H2O,CO2,
                         no3_uM,nn03_mg,abs254,abs350)


manual_hydro$names<-c("hydro", "tau")
table_export<-manual_hydro[,c(ncol(manual_hydro), 1:(ncol(manual_hydro)-1))]

write.table(table_export, file=paste(dir, "/Manual_Hydros_Taus.csv", sep=""), sep=",", row.names=FALSE, col.names=TRUE)

# =====================================
# Recalculate each variable/plateau with assigned coefficeints.
# Code below follows same structure as above (calcualted), see above for comments
# =====================================

all_list_of_df_manual<-list()

var=2 #variable number 
for (var in 2:ncol(data2)){
  data_name<-names(data2)[var]
    hydro<-manual_hydro[1,data_name]
    tau<-manual_hydro[2,data_name]
    
    all_df_manual<-list()
    
    if(var==14){
      k<-15 #CH4
    } else {
      k<-5} #other variables
    
    j=3
    for (j in 3:p){
      
      #Create vectors of variable of interest and time for each variable/plateau
      plateau<-subset(data2, data2$date_time>log$DateTime[j+1] & data2$date_time<log$DateTime[j+2])
      vector<-plateau[[var]]
      name<-names(plateau)[var]
      time<-as.numeric(plateau[[1]] - min(plateau[[1]], na.rm = TRUE))
      vectorWindow<-  runmean(vector, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
      
      x_raw<-vector
      x_hydro<-c(tail(vector, n=(hydro*-1)), rep(NA, times=hydro))
      x_hydro_window<-  runmean(x_hydro, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
      diff<-c(0,diff(x_hydro_window, lag=2)/2,0)
      x_tau<-x_hydro+runmean(tau*diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
      x_time<-time
      
      x_df<-data.frame(x_time, x_raw, x_hydro, x_tau)
      all_df_manual[[length(all_df_manual)+1]]<-x_df
      names(all_df_manual)<-c(names(all_df_manual)[1:length(all_df_manual)-1], paste(names(data2)[var], "plateau#", j, sep=" "))
      
    }
    
    all_list_of_df_manual[[length(all_list_of_df_manual)+1]]<-all_df_manual
    names(all_list_of_df_manual)<-c(names(all_list_of_df_manual)[1:length(all_list_of_df_manual)-1], paste(names(data2)[var], sep=" "))
    
}

# ==================================================
# Plot Diagnositcs for manual tau, hydro
# ==================================================

list=1
for (list in 1:length(all_list_of_df_manual)){
  
  if(list<=12){
    xlim<-c(0,60)} #YSI
  if(list==13){
    xlim<-c(0,660)} #CH4
  if(list>=14){
    xlim<-c(0,120)} #other variables
  
  data_var<-all_list_of_df_manual[[list]]
  name<-names(all_list_of_df_manual)[[list]]
  
  all_x<-c()
  j<-1
  for (j in 1:4){
    all_x<-c(all_x, data_var[[j]][,2])
  }
  
  spread<-max(all_x, na.rm = TRUE)-min(all_x, na.rm = TRUE)
  ylim=c(min(all_x, na.rm = TRUE)-(spread*0.06), max(all_x, na.rm = TRUE)+(spread*0.06))
  
  png(paste(dir, "/TauFigures/plateaus_manual/", name , "_Tau.png", sep=""),res=200, width=8,height=8, units="in")
  
  par(mar=c(2,2,0.5,1),bg=NA)
  par(mfrow=c(2,2)) 
  par( oma = c( 2,2,0,0 ) )
  par(tck=-.02)
  par(mgp=c(0,.8,0))
  par(cex=.8)
  
  j=1
  for (j in 1:4){
    plot(data_var[[j]][[1]],data_var[[j]][[2]], type="l",ylim=ylim, xaxt="n", xlab="", ylab="", yaxt="n", xlim=xlim, lwd=2)
    points(data_var[[j]][[1]],data_var[[j]][[3]], type="l", col="red", lwd=2)
    points(data_var[[j]][[1]],data_var[[j]][[4]], col="blue", type="l", lwd=3)
    axis(2, las=1)
    axis(1)
    abline(v=0, lty=2)
    
  }
  
  mtext("Elapsed time since intake change (s)",1,0, outer=TRUE)
  mtext(paste(name, sep=""),2,0, outer=TRUE)
  
  dev.off()
  
}

#End manual plot code


