#Loken modified function
# inputFile = prep_syringe
calc.syringe.conc.LL <- function (inputFile,  
                                  volGas = "volGas", volH2O = "volH2O", 
                                  barometricPressure = "barometricPressure", waterTemp = "waterTemp", 
                                  headspaceTemp = "headspaceTemp", 
                                  eqCO2 = "eqCO2", airCO2 = "airCO2",
                                  eqCH4 = "eqCH4", airCH4 = "airCH4", 
                                  eqN2O = "eqN2O", airN2O = "airN2O"){ 
  
  #kh table. should double check values to use. 
Kh  <-  data.frame("O2"=c(1.3*10^-3, 1700))
Kh <- cbind(Kh,"H2"=c(7.8*10^-4,500))
Kh <- cbind(Kh,"CO2"= c(3.4*10^-2, 2400 ))
Kh <- cbind(Kh,"N2"=c(6.1*10^-4, 1300))
Kh <- cbind(Kh,"He"=c(3.7*10^-4, 230))
Kh <- cbind(Kh,"Ne"=c(4.5*10^-4,490))
Kh <- cbind(Kh,"Ar"=c(1.4*10^-3, 1300))
Kh <- cbind(Kh,"CO"=c(9.5*10^-4,1300))
Kh <- cbind(Kh, "O3"=c(1.2*10^-2, 2300))
Kh <- cbind(Kh, "N2O"=c(2.5*10^-2, 2600))
Kh <- cbind(Kh, "SF6"=c(2.4*10^-4, 2400))
Kh <- cbind(Kh, "CH4"=c(1.4*10^-3, 1700))
Kh <- cbind(Kh, "C3H8"=c(1.4*10^-3, 2700))
Kh <- cbind(Kh, "NH3"=c(5.6*10^1, 4200))

cGas <- 8.3144598
cKelvin <- 273.15
cPresConv <- 1e-06
cT0 <- 298.15

# if (!is.character(gas)){stop(paste('gas must be a character. was given as',gas))}
# if (!any(names(Kh)==gas)){stop(paste(gas,'not found in list of coded gases'))}

# Khprime <-  unlist(Kh[gas])[1] / 101.325 #convert from atm to kpa
# C  <-  unlist(Kh[gas])[2]

# ckHCO2 <- 0.00033  # original NEON value
# ckHCH4 <- 1.4e-05 # original NEON value
# ckHN2O <- 0.00024 # original NEON value
# cdHdTCO2 <- 2400 # original NEON value
# cdHdTCH4 <- 1900 # original NEON value
# cdHdTN2O <- 2700 # original NEON value

ckHCO2 <- unlist(Kh["CO2"])[1] / 101.325 # Values from Loken convert from atm to kpa
ckHCH4 <- unlist(Kh["CH4"])[1] / 101.325 # Values from Loken convert from atm to kpa
ckHN2O <- unlist(Kh["N2O"])[1] / 101.325 # Values from Loken convert from atm to kpa
cdHdTCO2 <- unlist(Kh["CO2"])[2] # Values from Loken convert from atm to kpa
cdHdTCH4 <- unlist(Kh["CH4"])[2] # Values from Loken convert from atm to kpa
cdHdTN2O <- unlist(Kh["N2O"])[2] # Values from Loken convert from atm to kpa

airCO2_value = pull(inputFile[, airCO2])
airCO2_value[is.na(airCO2_value)] <- 405

airCH4_value = pull(inputFile[, airCH4])
airCH4_value[is.na(airCH4_value)] <- 1.85

airN2O_value = pull(inputFile[, airN2O])
airN2O_value[is.na(airN2O_value)] <- 0.33

barometricPressure_value = pull(inputFile[, barometricPressure])

volGas_value = pull(inputFile[, volGas])
volH2O_value = pull(inputFile[, volH2O])

eqCO2_value =  pull(inputFile[, eqCO2])
eqCH4_value =  pull(inputFile[, eqCH4])
eqN2O_value =  pull(inputFile[, eqN2O])

headspaceTemp_value = pull(inputFile[, headspaceTemp])

inputFile$dissolvedCO2_M <- barometricPressure_value * cPresConv * (
  (volGas_value * (eqCO2_value - airCO2_value)) / (cGas * volH2O_value * (headspaceTemp_value + cKelvin)) +
    eqCO2_value * ckHCO2 * exp(cdHdTCO2 * (1 / (headspaceTemp_value + cKelvin) - 1 / cT0)))

inputFile$dissolvedCH4_M <- barometricPressure_value * cPresConv * (
  (volGas_value * (eqCH4_value - airCH4_value)) / (cGas * volH2O_value * (headspaceTemp_value + cKelvin)) +
    eqCH4_value * ckHCH4 * exp(cdHdTCH4 * (1 / (headspaceTemp_value + cKelvin) - 1 / cT0)))

inputFile$dissolvedN2O_M <- barometricPressure_value * cPresConv * (
  (volGas_value * (eqN2O_value - airN2O_value)) / (cGas * volH2O_value * (headspaceTemp_value + cKelvin)) +
    eqN2O_value * ckHN2O * exp(cdHdTN2O * (1 / (headspaceTemp_value + cKelvin) - 1 / cT0)))

# inputFile$dissolvedCO2_M2 = baro_value * cPresConv *
#   (volGas_value * (eqCO2_value - sourceCO2_value)/(cGas * (headspaceTemp_value + cKelvin) * volH2O_value) + ckHCO2 * exp(cdHdTCO2 * (1/(headspaceTemp_value + cKelvin) - 1/cT0)) * eqCO2_value)
# 
# inputFile$dissolvedCH4_M = baro_value * cPresConv * 
#   (volGas_value * (eqCH4_value - sourceCH4_value)/(cGas * (headspaceTemp_value + cKelvin) * volH2O_value) + ckHCH4 * exp(cdHdTCH4 * (1/(headspaceTemp_value + cKelvin) - 1/cT0)) * eqCH4_value)
# 
# inputFile$dissolvedN2O_M = baro_value * cPresConv * 
#   (volGas_value * (eqN2O_value - sourceN2O_value)/(cGas * (headspaceTemp_value + cKelvin) * volH2O_value) + ckHN2O * exp(cdHdTN2O * (1/(headspaceTemp_value + cKelvin) - 1/cT0)) * eqN2O_value)


inputFile$dissolvedCO2_M <- signif(inputFile$dissolvedCO2_M, 
                                   digits = 3)
inputFile$dissolvedCH4_M <- signif(inputFile$dissolvedCH4_M, 
                                   digits = 3)
inputFile$dissolvedN2O_M <- signif(inputFile$dissolvedN2O_M, 
                                   digits = 3)
return(inputFile)
}
