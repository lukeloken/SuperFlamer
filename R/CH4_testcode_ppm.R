library(marelac)

#in mol ratio of CH4 to atm. This is what the package thinks CH4 was in the atm in 1998
CH4_atm_ppm <- atmComp("CH4")*1000000
print(CH4_atm_ppm)

# Example concentration from Stets 2015
CH4_ppm <- c(9.24)
CH4_molratio <- CH4_ppm / 1000000

CH4uM = gas_satconc(S = 0.01, t = 27.5, P = 1.004869, 
                    species = "CH4", atm = CH4_molratio)

print(CH4uM)
