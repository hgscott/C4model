Instantaenous_Response <- function(tg_c = 25,
                                   z = 0,
                                   vpdo = 1,
                                   cao = 400,
                                   oao = 209460, 
                                   paro = 800,
                                   q025 = 0.25,
                                   theta = 0.85,
                                   R = 8.314){
  source('C4model.R')
  
  # Run the full model at the acclimated conditions
  acclim_res <- C4model()
  
  acclim_chi <- acclim_res$chi
  acclim_jmax <- acclim_res$jmax
  acclim_vpmax <- acclim_res$vpmax
  acclim_vcmax <- acclim_res$vcmax
  acclim_leak <- acclim_res$Leakage
  
  jmax_temp <- calc_jmax_temp(acclim_jmax, tg_c, R)
  
  kp_temp <- calc_kp_temp_pa(tg_c, z)
  kr_temp <- calc_kc_temp_pa(tg_c, z)
  
  vc_temp <- acclim_vcmax * calc_tresp_mult(tg_c, 25, 25)
  vp_temp <- acclim_vpmax * calc_tresp_mult(tg_c, 25, 25)
  
  patm <- calc_patm(z)
  ca_pa <- cao * 1e-6 * patm
  ci <- ca_pa * acclim_chi
  
  gbs <- 0.003 # 3 mmol m^-2 s^-1
  cbs <- (acclim_leak + (gbs * ci)) /gbs
  
  Al <- jmax_temp / (8 * theta) # Check, means I'm using omega instead of omega star
  Ac <- (cbs * vc_temp)/(kr_temp + cbs)
  Ap <- (ci * vp_temp)/(kp_temp + ci)
  
  res <- data.frame(tg_c, z, vpdo, cao, paro, ci, cbs, jmax_temp, kp_temp, kr_temp,
                    vc_temp, vp_temp, Al, Ac, Ap)
  An <- apply(res[, 13:15], 1, min)
  res <- cbind(res, An)
  res <- as.data.frame(res)
  return(res)
}
