# modify of nick's function calc_km_pa in the optimal-vcmax repository

calc_ko_temp_pa <- function(temp, z) {
  temp_k <- 273.15 + temp
  
  patm <- calc_patm(z) 
  rat <- patm / calc_patm(0)
  O2 <- 2.09476e5
  O2_pa <- O2 * (1e-6) * patm 
  
  R <- 8.314   # in joules    
  
  Kc25 <- 94.7 # Pa CO2 Boyd 2015 
  Kc25 <- Kc25 * rat
  Hkc <-  64200 # in joules per mole
  
  Ko25_kpa <- 28.9 # Boyd 2015
  Ko25 <- Ko25_kpa * 1000
  Ko25 <- Ko25 * rat
  Hko <-  10500
  
  Kc_pa <- Kc25 * exp(Hkc * ((temp_k - 298.15) / (298.15 * R * temp_k)))
  Ko_pa <- Ko25 * exp(Hko * ((temp_k - 298.15) / (298.15 * R * temp_k)))
  
  Km_pa <- Kc_pa * (1 + O2_pa/Ko_pa)
  
  # Kc gives me closer unmbers to Nick's than Km, plus C4s shouldnt have any O2 in the bs, so it shouldnt have any Ko in the km anyway
  
  return(Ko_pa)
}