# calc ci

calc_chi_xi <- function(ca, temp, vpd, z){
  patm <- calc_patm(z)
  ca_pa <- ca * 1e-6 * patm
  
  R <- 8.3145 # Pa
  tempK <- temp + 273.15
  vpd_pa <- vpd * 1000 # is this right??? Ci would be higher if I didnt do this
  beta <- 30.83878 # New number, 03/06/2020
  
  Kr_25 <- 94.7 # Pa
  Ea_Kr <- 64200 # J mol^-1, from Boyd 2015 to match everything else
  Kr <- Kr_25 * exp((Ea_Kr * (tempK - 298.15))/(298.15 * R * tempK))
  
  Kp_25 <- 16 # Pa
  Ea_Kp <- 36300 # J mol^-1, for the Pa parameter ## Boyd et al 2015
  Kp <- Kp_25 * exp((Ea_Kp * (tempK - 298.15))/(298.15 * R * tempK))
  
  eta_star <- calc_nstar(temp, z)
  eta_star <- eta_star[[1]]
  
  xi <- sqrt((beta * (Kr + Kp))/(1.6 * eta_star))
  chi <- (xi / (xi + sqrt(vpd_pa)))
  ci <- (xi * ca_pa)/(xi * sqrt(vpd_pa)) #ci is in Pa now
  res <- c(chi, ci)
  
  return(res)
}
