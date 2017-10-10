# =====================================================
# CONVERSION CODE - Use getkH for CH4 and KH_Plummber for CO2
# Use getSaturation for both gases
# =====================================================


ConvertGasesSuperFlame=function(correctdata, Elevation){
  
  convertdata<-correctdata
  
  if(length(which(is.finite(c(correctdata$CO2, correctdata$CH4)))==T)>1){
    
    Pressure=(1-(.0000225577*Elevation))^5.25588# atmospheres
    
    #convert methane units 
    CH4kh=getKh(convertdata$temp+273.15,"CH4")
    CH4getsat=as.numeric(getSaturation(CH4kh, Pressure, "CH4"))
    
    convertdata$CH4uM<-as.numeric(convertdata$CH4*CH4kh*Pressure)
    convertdata$CH4Sat<-as.numeric(convertdata$CH4uM/CH4getsat*100)
    
    convertdata$CH4uM_hyd<-as.numeric(convertdata$CH4_hyd*CH4kh*Pressure)
    convertdata$CH4Sat_hyd<-as.numeric(convertdata$CH4uM_hyd/CH4getsat*100)
    
    convertdata$CH4uM_tau<-as.numeric(convertdata$CH4_tau*CH4kh*Pressure)
    convertdata$CH4Sat_tau<-as.numeric(convertdata$CH4uM_tau/CH4getsat*100)
    
    #convert CO2 units
    CO2kh=Kh_Plummer(convertdata$temp+273.15)
    CO2getsat=as.numeric(getSaturation(CO2kh, Pressure, "CO2"))
    
    convertdata$CO2uM<-as.numeric(convertdata$CO2*CO2kh*Pressure)
    convertdata$CO2Sat<-as.numeric(convertdata$CO2uM/CO2getsat*100)
    
    convertdata$CO2uM_hyd<-as.numeric(convertdata$CO2_hyd*CO2kh*Pressure)
    convertdata$CO2Sat_hyd<-as.numeric(convertdata$CO2uM_hyd/CO2getsat*100)
    
    convertdata$CO2uM_tau<-as.numeric(convertdata$CO2_tau*CO2kh*Pressure)
    convertdata$CO2Sat_tau<-as.numeric(convertdata$CO2uM_tau/CO2getsat*100)
    
  }
  return(convertdata)
}