# modify of nick's function calc_km_pa in the optimal-vcmax repository

calc_kc_pa <- function(temp, z) {
  R = 8.314   # in joules     
  Kc25 = 94.7 # Pa CO2 Boyd 2015
  Hkc = 64200 # in joules per mole
  temp_k = 273.15 + temp
  
  Kc_pa =Kc25 * exp(Hkc * ((temp_k - 298.15) / (298.15 * R * temp_k)))
  return(Kc_pa)
}