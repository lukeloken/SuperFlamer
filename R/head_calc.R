################################################################### 
# The following function head.calc(input.table) back Calculats gas (CO2, CH4, and N2O) concentrations
# and isotopic signature in in situ water from a sample using the headspace technique.
# In order to run the function you need to import a csv or text file containing the input variables 
# used in the calculation. 
###################################################################
#Author : Cynthia Soued, email for questions: cynthia.soued@gmail.com
#updated: 09/05/2021 : include N2O headspace calculation
#updated: 09/05/2021 : change the solubility formula of CH4 to include salinity
#updated: 09/05/2021 : changed the output columns
#updated: 09/05/2021 : Added the references for solubility calculations
#updated: 26/11/2019 : Fixed a mistake on delta CO2 error calculation, formula was written with pCH4 instead of pCO2
#Updated: 04/11/2019 : Fixed a mistake on the Co2 error (the CV in % was not divided by 100 giving values 2 order of magnitude too high)
#Updated: 23/07/2019 : change the definition of error for concentration and isotopes. CV and SD are now a function of measured concentrations (greater error on low concentrations) 
#Updated: 23/07/2019 : added error on the headspace gas in the monte carlo simulations
#Updated: 21/05/2019 : add the correction for CO2 equilibration with bicabornate system.
#Updated: 21/05/2019 : add correction to avoid error if some columns are missing from input table (replace them by NA instead of NULL)
#Updated: 21/05/2019 : Minor change: remove the transformation of some output columns to character becasue they are particular to my database
#Updated: 25/03/2019
#################################


#Function start
##############################################################
head.calc.single = function(input.table) {

#Defining input table columns
    # If you import a table format with other column names than the ones below
    # then you must change them accordingly after the dollar sign in each row.
    # IMPORTANT!! Verify that your inputs variables are in the rignt units (indicated below for each column)
  
ID = as.character(input.table$ID) #Sample unique ID
ID.rep = as.character(input.table$ID.rep) #Sample replicate ID
pCO2.meas = ifelse(is.null(input.table$pCO2.meas),NA,input.table$pCO2.meas) #CO2 (ppm) measured in the headspace after shaking
pCH4.meas = ifelse(is.null(input.table$pCH4.meas),NA,input.table$pCH4.meas) #CH4 (ppm) measured in the headspace after shaking
pN2O.meas = ifelse(is.null(input.table$pN2O.meas),NA,input.table$pN2O.meas) #N2O (ppm) measured in the headspace after shaking
pCO2.head = ifelse(is.null(input.table$pCO2.head),NA,input.table$pCO2.head) #CO2 (ppm) in the headspace before shaking
pCH4.head = ifelse(is.null(input.table$pCH4.head),NA,input.table$pCH4.head) #CH4 (ppm) in the headspace before shaking
pN2O.head = ifelse(is.null(input.table$pN2O.head),NA,input.table$pN2O.head) #N2O (ppm) in the headspace before shaking
vol.tot = input.table$vol.tot #total volume of the bottle/syringe in L
vol.head = input.table$vol.head #Volume of the headspace in L
temp.in = input.table$temp.in #in situ temperature (before shaking) in degree celsius
temp.sam = input.table$temp.sam #sampling temperature (after shaking) in degree celsius
atm = input.table$atm #Atmospheric pressure in kPa
sal = input.table$sal #Salinity, in ppt, considered 0 in freshwater systems
alk = ifelse(is.null(input.table$alk),NA,input.table$alk)   #alkalinity of the water in situ in micro eq/L
DIC = ifelse(is.null(input.table$DIC),NA,input.table$DIC)   #DIC of the water in situ in mg/L (or ppm)
pH = ifelse(is.null(input.table$pH),NA,input.table$pH)  #pH of the water in situ


# Calculating intermediate terms
  # mol.vol = Molar volume
  # ratio = Headspace ratio (air/water)
  # sol.CO2.in = solubility of CO2 at in situ temperature, equation from Weiss (1977) DOI:https://doi.org/10.1016/0304-4203(74)90015-2
  # sol.CO2.sam = solubility of CO2 at sampling temperature, equation from Weiss (1977) DOI:https://doi.org/10.1016/0304-4203(74)90015-2
  # sol.CH4.in = solubility of CH4 at in situ temperature, equation derived from Yamamoto et al. 1976 DOI: 10.1021/je60068a029 
  # sol.CH4.sam = solubility of CH4 at sampling temperature, equation derived from Yamamoto et al. 1976 DOI: 10.1021/je60068a029 
  # sol.N2O.in = solubility of N2O at in situ temperature, equation from Weiss and Price (1980) DOI: 10.1016/0304-4203(80)90024-9
  # sol.N2O.sam = solubility of N2O at sampling temperature, equation from Weiss and Price (1980) DOI: 10.1016/0304-4203(80)90024-9
  # CO2.frac = CO2 fractionation number
  # alpha.CO2 = CO2 alpha number
  # alpha.CH4 = CH4 alpha number

mol.vol = (0.082057*(temp.sam+273.15))*(101.325/atm)
ratio =  vol.head / (vol.tot-vol.head)
sol.CO2.in = 10^(-(-((9345.17/(temp.in+273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10)))
sol.CO2.sam = 10^(-(-((9345.17/(temp.sam+273.15))-60.2409+23.3585*log((273.15+temp.sam)/100)+sal*(0.023517-0.023656*((273.15+temp.sam)/100)+0.0047036*((273.15+temp.sam)/100)^2))/log(10)))
sol.CH4.in = exp(-67.1962 + 99.1624 * (100/(temp.in+273.15)) + 27.9015 *log((temp.in+273.15)/100) + sal*(-0.072909 +0.041674 *((temp.in+273.15)/100) -0.0064603*((temp.in+273.15)/100)^2 ) )
sol.CH4.sam = exp(-67.1962 + 99.1624 * (100/(temp.sam+273.15)) + 27.9015 *log((temp.sam+273.15)/100) + sal*(-0.072909 +0.041674 *((temp.sam+273.15)/100) -0.0064603*((temp.sam+273.15)/100)^2 ) )
sol.N2O.in = exp(-62.7062+97.3066*(100/(temp.in+273.15))+24.1406*log((temp.in+273.15)/100)+sal*(-0.05842+0.033193*((temp.in+273.15)/100)+(-0.0051313)*((temp.in+273.15)/100)^2))
sol.N2O.sam = exp(-62.7062+97.3066*(100/(temp.sam+273.15))+24.1406*log((temp.sam+273.15)/100)+sal*(-0.05842+0.033193*((temp.sam+273.15)/100)+(-0.0051313)*((temp.sam+273.15)/100)^2))

# Calculating in situ C gas concentrations
# pCO2.insitu = in situ CO2 concentration in the water (in ppm)
# pCH4.insitu = in situ CH4 concentration in the water (in ppm)
# pN2O.insitu = in situ N2O concentration in the water (in ppm)
# conc.CO2.insitu = in situ CO2 concentration in the water (in umol/L)
# conc.CH4.insitu = in situ CH4 concentration in the water (in umol/L)
# conc.N2O.insitu = in situ N2O concentration in the water (in umol/L)
# dsat.CO2 = in situ CO2 saturation in the water (in umol/L)
# dsat.CH4 = in situ CH4 saturation in the water (in umol/L)
# dsat.N2O = in situ N2O saturation in the water (in umol/L)


pCO2.insitu = ((pCO2.meas-pCO2.head)*ratio/(mol.vol)+(pCO2.meas*sol.CO2.sam))/sol.CO2.in
pCH4.insitu = ((pCH4.meas-pCH4.head)*ratio/(mol.vol)+(pCH4.meas*sol.CH4.sam))/sol.CH4.in
pN2O.insitu = ((pN2O.meas-pN2O.head)*ratio/(mol.vol)+(pN2O.meas*sol.N2O.sam))/sol.N2O.in

conc.CO2.insitu = pCO2.insitu * sol.CO2.in
conc.CH4.insitu = pCH4.insitu * sol.CH4.in
conc.N2O.insitu = pN2O.insitu * sol.N2O.in

dsat.CO2 = conc.CO2.insitu - pCO2.head * sol.CO2.in
dsat.CH4 = conc.CH4.insitu - pCH4.head * sol.CH4.in
dsat.N2O = conc.N2O.insitu - pN2O.head * sol.N2O.in



###########################
#Calculate the error and its propagation
###########################

#Define the error for gas concentrations and isotopic signature
cv.pCO2=10^(-0.1342654*log10(pCO2.meas)+0.4946901+0.6)/100 #correlation derived from my samples : CV of replicates increases with lower pCO2.meas
cv.pCH4=10^(-0.06521389*log10(pCH4.meas)+0.3771357+0.8)/100 #correlation derived from my samples : CV of replicates increases with lower pCH4.meas

pCO2.error=ifelse(is.null(pCO2.meas),NA,abs(cv.pCO2*na.omit(pCO2.meas))) 
pCH4.error=ifelse(is.null(pCH4.meas),NA,abs(cv.pCH4*na.omit(pCH4.meas))) 
pN2O.error=ifelse(is.null(pN2O.meas),NA,abs(0.1*pN2O.meas)) #Typical CV of a replicate for N2O gas concentration (here it's set to 10%)

pCO2.head.error=ifelse(is.null(pCO2.head),NA,abs(cv.pCO2*na.omit(pCO2.head))) 
pCH4.head.error=ifelse(is.null(pCH4.head),NA,abs(cv.pCH4*na.omit(pCH4.head))) 
pN2O.head.error=ifelse(is.null(pN2O.head),NA,abs(0.1*pN2O.head)) #Typical CV of a replicate for N2O gas concentration (here it's set to 10%)


#Generate random vectors with a normal distribution based on error for gas concentration and isotopic signature for simulations
pCO2.random=rnorm(1000,mean=pCO2.meas,sd=pCO2.error)
pCH4.random=rnorm(1000,mean=pCH4.meas,sd=pCH4.error)
pN2O.random=rnorm(10000,mean=pN2O.meas,sd=pN2O.error)

pCO2.head.random=rnorm(1000,mean=pCO2.head,sd=pCO2.head.error)
pCH4.head.random=rnorm(1000,mean=pCH4.head,sd=pCH4.head.error)
pN2O.head.random=rnorm(1000,mean=pN2O.head,sd=pN2O.head.error)


#Calculating error on gas concentration using Monte Carlo simulation
pCO2.insitu.error = sd(unlist(lapply(1:length(pCO2.random),function(i){ ((pCO2.random[i]-pCO2.head.random)*ratio/(mol.vol)+(pCO2.random[i]*sol.CO2.sam))/sol.CO2.in})))
pCH4.insitu.error = sd(unlist(lapply(1:length(pCH4.random),function(i){ ((pCH4.random[i]-pCH4.head.random)*ratio/(mol.vol)+(pCH4.random[i]*sol.CH4.sam))/sol.CH4.in})))
pN2O.insitu.error = sd(unlist(lapply(1:length(pN2O.random),function(i){ ((pN2O.random[i]-pN2O.head.random)*ratio/(mol.vol)+(pN2O.random[i]*sol.N2O.sam))/sol.N2O.in})))

conc.CO2.insitu.error = pCO2.insitu.error * sol.CO2.in
conc.CH4.insitu.error = pCH4.insitu.error * sol.CH4.in
conc.N2O.insitu.error = pN2O.insitu.error * sol.N2O.in



#####################################
#Calculating the correction for CO2 equilibruim (this section is written by Jihyeon Kim)
#####################################

#Derivating alkalinity from DIC and pH 
pk1=0.000011*temp.in^2-0.012*temp.in+6.58
pk2=0.00009*temp.in^2-0.0137*temp.in+10.62
HCO3_prop=1/(10^(-pH+pk1)+1+10^(-pk2+pH))
CO3_prop=1/(10^(-2*pH+(pk1+pk2))+10^(-pH+pk2)+1)
HCO3=HCO3_prop*(DIC*1000/12.01)  #converting DIC from mg/L to umol/L
CO3=CO3_prop*(DIC*1000/12.01)
Kw = 10^(-(0.0002*((temp.sam)^2)-0.0444*temp.sam+14.953))
Hplus=10^-pH
OHminus=Kw/Hplus

if(is.na(alk)){alk=HCO3+2*CO3+OHminus-Hplus}  

# Derivation of the terms used in calculations
  # Kw = the dissociation constant of H2O into H+ and OH-
  # K1 = the equilibrium constant between CO2 and HCO3-
  # K2 = the equilibrium constant between HCO3- and CO3 2-
  # alpha0, alpha1, alpha2 = the ionization fractions of the DIC, i.e the fraction of the DIC comprised by CO2, HCO3-, and CO3 2-, respectively.

alk = alk*(1e-6)
K1 = 10^(((-(3404.71/(temp.sam+273.15)))+14.8435)-0.032786*(temp.sam+273.15))
K2 = 10^(((-(2902.39/(temp.sam+273.15)))+6.498)-0.02379*(temp.sam+273.15))

# Solve order 3 polynomial
a = 1
b = alk
CO2 = sol.CO2.sam*pCO2.meas*(1e-6)
c = -(CO2*K1+Kw)
d = -2*CO2*K1*K2
p = (3*a*c-b^2)/(3*a^2)
q = (2*(b^3)-9*a*b*c+27*d*a^2)/(27*a^3)
H.after = 2*sqrt(-p/3)*cos(acos(3*q*sqrt(-3/p)/(2*p))/3)-b/3
pH.after = -log10(H.after)
alpha0.after = 1/(1+K1/H.after+K1*K2/(H.after^2))
alpha1.after = 1/((H.after/K1)+1+K2/H.after)
alpha2.after = 1/(1+H.after/K2+(H.after^2)/(K1*K2))
DIC.after = CO2/alpha0.after
tDIC.after = (b+H.after-Kw/H.after)/((H.after*K1+2*K1*K2)/((H.after^2)+K1*H.after+K1*K2))
TC.after = pCO2.meas*ratio/mol.vol*(1e-6)+DIC.after
TC.before = TC.after-((ratio*pCO2.head*(1e-6))/mol.vol)

# Solve order 4 polynomial
a1 = 1
b1 = -(alk * K1 - Kw + K1 * K2 - TC.before * K1)
c1 = (K1*K2*alk - K1 * Kw - 2*TC.before * K1*K2)*(alk + K1)-4*(-K1*K2*Kw)
d1 = 4*(alk*K1 - Kw+K1*K2 - TC.before*K1)*(-K1*K2*Kw)-(K1*K2*alk-K1*Kw-2* TC.before * K1* K2)^2-(-K1*K2*Kw)*(alk+K1)^2
p2 = (3*a1*c1 - b1^2)/(3*a1^2)
q2 = (2 * b1^3 - 9*a1*b1*c1 + 27*d1*a1^2)/(27*a1^3)
r2 = 2*sqrt(-p2/3)*cos(acos(3*q2*sqrt(-3/p2)/(2*p2))/3)-b1/3
RR = sqrt((alk+K1)^2/4-(alk* K1 - Kw + K1 * K2- TC.before* K1) + r2)
SS = 3 * (alk + K1)^2 /4 - RR^2 - 2*(alk* K1 - Kw + K1 * K2 - TC.before * K1)
TT = (4 * (alk + K1) * (alk * K1 - Kw + K1* K2 - TC.before * K1) - 8 * (K1 * K2 * alk - K1 * Kw - 2 * TC.before * K1 * K2) - (alk + K1)^3) / (4 *RR)
H.before = -(alk + K1) / 4 + RR / 2 + sqrt( SS + TT ) / 2
pH.before = -log10(H.before)
alpha0.before = 1 / (1 + K1 /H.before + K1 * K2/(H.before^2))

# Calculating pCO2 insitu correced for the bicabonate equilibration  
pCO2.corrected = alpha0.before*TC.before*(1e6)/ sol.CO2.in
conc.CO2.corrected = pCO2.corrected * sol.CO2.in





#######################################
# Compiling the results in a data table
#######################################
# Here you can custumize the output of the function by adding/removing/changing the order
# of the output columns below in the parenthesis. 
output.single=data.frame(input.table,
                         pCO2.corrected,conc.CO2.corrected,pCO2.insitu,conc.CO2.insitu,dsat.CO2,pCO2.insitu.error,conc.CO2.insitu.error,
                         pCH4.insitu,conc.CH4.insitu,dsat.CH4,pCH4.insitu.error,conc.CH4.insitu.error,
                         pN2O.insitu, conc.N2O.insitu,dsat.N2O,pN2O.insitu.error,conc.N2O.insitu.error,
                         stringsAsFactors=F)

# Defining the object to return
return(output.single)

}

############################
#Defining a function to apply the calculations to a table (instead of a single line)
#############################

head.calc=function(input.table){
  output = lapply(1:dim(input.table)[1],function(i){head.calc.single(input.table[i,])})
  col.names=colnames(data.frame(output[1]))
  output=do.call(rbind.data.frame, output)
  colnames(output)=col.names
  return(output)
  }




#End of function
#######################################################

