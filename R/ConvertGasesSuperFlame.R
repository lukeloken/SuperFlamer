# =====================================================
# CONVERSION CODE - Use getkH for CH4 and KH_Plummber for CO2
# Use getSaturation for both gases
# =====================================================


ConvertGasesSuperFlame=function(correctdata, Elevation){
  
  convertdata<-correctdata
  
  #When water value is wonky, replace CH4 and CO2
  if("CH4_Dry" %in% names(convertdata)){
    convertdata$CH4_Dry[which(convertdata$H2O <= 500 | 
                              convertdata$H2O > 60000 |
                              !is.finite(convertdata$H2O))] <- NA
    
    convertdata$CO2_Dry[which(convertdata$H2O <= 500 | 
                                convertdata$H2O > 60000 |
                                !is.finite(convertdata$H2O))] <- NA
    
    convertdata$H2O[which(convertdata$H2O <= 500 | 
                                convertdata$H2O > 60000 |
                                !is.finite(convertdata$H2O))] <- NA
    
  }
  
  if(length(which(is.finite(c(correctdata$CO2_Dry, correctdata$CH4_Dry)))==T)>1){
    
    Pressure=(1-(.0000225577*Elevation))^5.25588# atmospheres
    
    #convert methane units 
    CH4kh=getKh(convertdata$temp+273.15,"CH4")
    CH4atmsat=as.numeric(getSaturation(CH4kh, Pressure, "CH4"))
    
    convertdata$CH4uM<-as.numeric(convertdata$CH4_Dry*CH4kh*Pressure)
    convertdata$CH4Sat<-as.numeric(convertdata$CH4uM/CH4atmsat*100)
    
    convertdata$CH4uM_hyd<-as.numeric(convertdata$CH4_Dry_hyd*CH4kh*Pressure)
    convertdata$CH4Sat_hyd<-as.numeric(convertdata$CH4uM_hyd/CH4atmsat*100)

    convertdata$CH4uM_tau<-as.numeric(convertdata$CH4_Dry_tau*CH4kh*Pressure)
    convertdata$CH4Sat_tau<-as.numeric(convertdata$CH4uM_tau/CH4atmsat*100)
    
    #convert CO2 units
    CO2kh=Kh_Plummer(convertdata$temp+273.15)
    CO2atmsat=as.numeric(getSaturation(CO2kh, Pressure, "CO2"))
    
    convertdata$CO2uM<-as.numeric(convertdata$CO2_Dry*CO2kh*Pressure)
    convertdata$CO2Sat<-as.numeric(convertdata$CO2uM/CO2atmsat*100)
    
    convertdata$CO2uM_hyd<-as.numeric(convertdata$CO2_Dry_hyd*CO2kh*Pressure)
    convertdata$CO2Sat_hyd<-as.numeric(convertdata$CO2uM_hyd/CO2atmsat*100)

    convertdata$CO2uM_tau<-as.numeric(convertdata$CO2_Dry_tau*CO2kh*Pressure)
    convertdata$CO2Sat_tau<-as.numeric(convertdata$CO2uM_tau/CO2atmsat*100)
    
  }
  return(convertdata)
}