
# #########################################################
# Calculate gas saturation in water
# Uses Kh function and list of atmmospheric concentrations
# #########################################################

getSaturation <- function(LakeKh, AtmP, gas){
  Atmosphere  <-  data.frame("O2"=209460)
  Atmosphere <- cbind(Atmosphere,"H2"=0.55)
  Atmosphere <- cbind(Atmosphere, "N2"=780840)
  Atmosphere <- cbind(Atmosphere, "Ar"=9340)
  Atmosphere <-cbind(Atmosphere, "CO2"=400)
  Atmosphere <-cbind(Atmosphere, "He"=5.24)
  Atmosphere <-cbind(Atmosphere, "Ne"=18.18)
  Atmosphere <- cbind(Atmosphere, "CH4"= 1.91)
  Atmosphere <- cbind(Atmosphere, "O3"=0.07)#potential max concentration
  Atmosphere <-cbind(Atmosphere, "N2O"= 0.325)
  Atmosphere <-cbind(Atmosphere, "CO"=0.1)
  Atmosphere <-cbind(Atmosphere, "NH3"=NA)
  
  if (!is.character(gas)){stop(paste('gas must be a character. was given as',gas))}
  if (!any(names(Atmosphere)==gas)){stop(paste(gas,'not found in list of coded gases'))}
  
  AtmosphereConc <-  unlist(Atmosphere[gas])[1]
  
  EquilSaturation=AtmosphereConc*LakeKh/AtmP #umol/L, mmol/m3
}