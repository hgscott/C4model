# calc_full_model: calcualtes net photosynthesis given environmetal conditions
#
# Arguments
# tg_c: Growing season mean temperature (celsius), default = 25
# z: elevation (m), default = 0
# vpd: vapour pressure deficit (kPa), default = 1
# ca: Atmospheric CO2 (ppm), default = 400
# I: Light (micro mol m^-2 s^-1), default = 800
# phi: light use efficiency (unitless), default = 0.0528
#
# Returns
# tg_c: Growing season mean temperature (celsius), default = 25
# z: elevation (m), default = 0
# vpd: vapour pressure deficit (kPa), default = 1
# ca: Atmospheric CO2 (ppm), default = 400
# I: Light (micro mol m^-2 s^-1), default = 800
# kp_temp: PEPc Michaleis Menten constant (Pa CO2)
# kr_temp: Rubisco Michaleis Menten constant (Pa CO2)
# ci: Internal/mesophyll CO2 concentration (Pa)
# cbs: Bundle Sheath CO2 concentration (Pa)
# vcmax_temp: Vcmax instantaneous value (micro mol m^-2 s^-1)
# vpmax_temp: Vpmax instantaneous value (micro mol m^-2 s^-1)
# Al: Light limited photosynthesis (micro mol m^-2 s^-1)
# Ap: PEPc limited photosynthesis (micro mol m^-2 s^-1)
# Ac: Rubisco limited photosynthesis (micro mol m^-2 s^-1)
# An: Net photosynthesis (micro mol m^-2 s^-1)

calc_unacclimated <- function(tg_c = 25, z = 0, vpd = 1, ca = 400, I = 800, phi = 0.0528){
  source('~/School/Smith Research/Code/Functions/calc_full_model.R')
  
  # Run the acclimated model at standard conditions
  to <- (0.44 * tg_c) + 24.92
  
  acclim_std <- calc_full_model()
  chi_25 <- acclim_std$chi
  chi2_25 <- acclim_std$Cbs / acclim_std$Ci
  Vp_25 <- acclim_std$Vpmax_pred
  Vc_25 <- acclim_std$Vcmax#  / calc_tresp_mult(tg_c, tg_c, to)
  
  Al <- phi * I  * calc_tresp_mult(tg_c, tg_c, to)
  
  kp_temp <- calc_kp_temp_pa(tg_c, z)
  kr_temp <- calc_kc_temp_pa(tg_c, z)
  
  vc_temp <- Vc_25 * calc_tresp_mult(tg_c, 25, 25)
  vp_temp <- Vp_25 * calc_tresp_mult(tg_c, 25, 25)
  
  patm <- calc_patm(z)
  ca_pa <- ca * 1e-6 * patm
  ci <- ca_pa * chi_25
  cbs <- ci * chi2_25
  
  Ac <- (cbs * vc_temp)/(kr_temp + cbs)
  Ap <- (ci * vp_temp)/(kp_temp + ci)
  
  res <- data.frame(tg_c, z, vpd, ca, I, ci, cbs, kp_temp, kr_temp, vc_temp, vp_temp, Al, Ac, Ap)
  An <- apply(res[, 12:14], 1, min)
  res <- cbind(res, An)
  res <- as.data.frame(res)
  return(res)
}