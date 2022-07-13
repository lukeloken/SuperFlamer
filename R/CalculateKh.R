# ###############################################################################
# Function to calculate Kh for a variety of gases and temperature
# Henry's Law constants from http://www.henrys-law.org/henry.pdf
# Temperature correction using van't Hoff equation , temperature is in (Kelvin)
# LakeKh is in ("Kh, cp") = (mol L-1 Atm-1) at STP
# ###############################################################################

getKh <- function(temperature, gas){
  Kh <- data.frame("O2" =c(1.3*10^-3, 1700))
  Kh <- cbind(Kh, "H2"  =c(7.8*10^-4, 500))
  Kh <- cbind(Kh, "CO2" =c(3.4*10^-2, 2400 ))
  Kh <- cbind(Kh, "N2"  =c(6.1*10^-4, 1300))
  Kh <- cbind(Kh, "He"  =c(3.7*10^-4, 230))
  Kh <- cbind(Kh, "Ne"  =c(4.5*10^-4, 490))
  Kh <- cbind(Kh, "Ar"  =c(1.4*10^-3, 1300))
  Kh <- cbind(Kh, "CO"  =c(9.5*10^-4, 1300))
  Kh <- cbind(Kh, "O3"  =c(1.2*10^-2, 2300))
  Kh <- cbind(Kh, "N2O" =c(2.5*10^-2, 2600))
  Kh <- cbind(Kh, "SF6" =c(2.4*10^-4, 2400))
  Kh <- cbind(Kh, "CH4" =c(1.4*10^-3, 1700))
  Kh <- cbind(Kh, "C3H8"=c(1.4*10^-3, 2700))
  Kh <- cbind(Kh, "NH3" =c(5.6*10^1,  4200))
  
  if (!is.character(gas)){stop(paste('gas must be a character. was given as',gas))}
  if (!any(names(Kh)==gas)){stop(paste(gas,'not found in list of coded gases'))}
  
  Khprime <-  unlist(Kh[gas])[1]
  C  <-  unlist(Kh[gas])[2]
  
  LakeKh= as.numeric(Khprime*exp(C*((1/temperature)-(1/298))))
}
