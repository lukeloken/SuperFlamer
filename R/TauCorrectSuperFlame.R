# ##################################################################################
# Code to loop through all Flame variables and apply tau corrections
# Output is a datatable with variables amended with _h and _t for hydraulic and tau corrected values
# ##################################################################################

require(caTools)

TauCorrectSuperFlame<-function(trimdata, tautable, plot=FALSE, ...){
  
  # Using Manual Fit Taus
  par(mfrow=c(1,1))
  kYSI=5 #Number of obs for running mean calculation
  kCH4<-10#Number of obs for running mean calculation for Methane
  colnames<-names(tautable)
  
  column=9 #variable number. var=9 is typically first for SuperFlame data 
  
  for (column in 9:ncol(trimdata)){
    X<-trimdata[[column]]
    name<-names(trimdata)[column]
    if (class(X)=='numeric' & is.element(name, names(tautable))){
      
      col_number<-which(names(tautable) == as.character(name))
      hydro<-unlist(round(tautable[1,col_number, with=FALSE]))
      tau<-unlist(tautable[2,col_number, with=FALSE])
      
      lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
      Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
      
      if (name=='CH4'){k=kCH4
      } else { k=kYSI}
      Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
      Tau_X<-rep(NA, times=length(X))
      
      for (i in 1:(length(X)-hydro)){
        Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
      }
      
      trimdata[,paste(name, "_hyd", sep="") := lag]
      trimdata[,paste(name, "_tau", sep="") := Tau_X]
      
      ylim=extendrange(X, f=0.05)
      
      # if(!is.null(plot)){
      if(plot==TRUE){
        plot(Tau_X, col="blue", type="l", ylim=ylim, main=paste(name), xlab="time (s)", ylab=name)
        points(lag, col="red", type="l")
        points(X, col="black", type="l")
      }
    }
  }
  return(trimdata)
}