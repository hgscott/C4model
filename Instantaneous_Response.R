Instantaenous_Response <- function(tg_c = 25,
                                   z = 0,
                                   vpdo = 1,
                                   cao = 400,
                                   oao = 209460, 
                                   paro = 800,
                                   theta = 0.85,
                                   leakiness = 0.2,
                                   R = 8.314){
  
  # Run the full model at the acclimated conditions
  acclim_res <- C4model()
  
  acclim_chi <- acclim_res$chi_m
  acclim_chi_bs <- acclim_res$chi_bs
  acclim_jmax <- acclim_res$jmax
  acclim_vpmax <- acclim_res$vpmax
  acclim_vcmax <- acclim_res$vcmax
  acclim_leak <- acclim_res$Leakage
  
  q0 = -0.0805 + (0.022 * tg_c) - (0.00034 * tg_c * tg_c)
  
  jmax_temp <- calc_jmax_temp(acclim_jmax, tg_c, R)
  
  kp_temp <- calc_kp_temp_pa(tg_c, z)
  kr_temp <- calc_kc_temp_pa(tg_c, z)
  ko_temp <- calc_ko_temp_pa(tg_c, z)
  
  vc_temp <- acclim_vcmax * calc_tresp_mult(tg_c, 25, 25)
  vp_temp <- acclim_vpmax * calc_tresp_mult(tg_c, 25, 25)
  
  patm <- calc_patm(z)
  ca_pa <- cao * 1e-6 * patm
  ci <- ca_pa * acclim_chi
  
  oa_pa <- oao * 1e-6 * patm
  oi <- oa_pa * acclim_chi
  gamma_star_temp <- calc_gammastar_pa_c4(tg_c, z)
  
  m <- (ci - gamma_star_temp) / (ci + 2 * gamma_star_temp)
  omega <- jmax_temp / (q0 * paro)
  omega_star <- (1 + (omega) - sqrt((1 + (omega))^2 - (4 * theta * omega)))
  
  Al <- q0 * paro * m * omega_star / (8 * theta) # Eqn. 2.2
  
  leakage <- (leakiness * Al) / (1 - leakiness)
  Ap_gross <- (ci * vp_temp)/(kp_temp + ci)
  Ap <- Ap_gross - acclim_leak
  
  gbs <- 0.003 # 3 mmol m^-2 s^-1
  # cbs <- ci + ((Ap_gross - Al) / gbs) # Something is wrong here
  cbs <- ca_pa * acclim_chi_bs
  
  Ac <- vc_temp * ((cbs - gamma_star_temp) / (kr_temp * (1 + oi/ko_temp) + cbs))
  
  res <- data.frame(tg_c, z, vpdo, cao, paro, ci, cbs, jmax_temp, kp_temp, kr_temp,
                    vc_temp, vp_temp, Al, Ac, Ap)
  An <- apply(res[, 13:15], 1, min)
  res <- cbind(res, An)
  res <- as.data.frame(res)
  return(res)
}
