

# function to calculate equilibrium concentration of CO2 in water 
# Temperature dependence
# Plummer and Busenberg 1982; Geochimica et Cosmochimica Acta

Kh_Plummer <- function(Temperature){
  kh_Plummer=10^(108.3865 + 0.01985076*Temperature - 6919.53*(Temperature^-1) -40.45154*log10(Temperature)+669365*(Temperature^-2))
  return(kh_Plummer)
}     