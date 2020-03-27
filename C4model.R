# C4model: Predicts acclimated 
# Helen Scott
# Last Updated: 03/27/2020
#
# Arguments
## tg_c: average growing season atmospheric temperature (C)
## z: elevation (m)
## vpdo: vapur pressure defecit (Pa?)
## cao: atmospheric carbon dioxode concentration (ppm)
## paro: light intensity (unit?)
## q025: phi value (unit?)
## theta: description (unit)
## R: ideal gas constant (unit?)
#
# Returns
## tg_c: average growing season atmospheric temperature (C)
## par: light intensity (unit?)
## ca: atmospheric carbon dioxode concentration (pa)
## z: elevation (m)
## vpd: vapur pressure defecit (Pa?)
## q0: : phi value (unit?)
## kp: michaelis menten coefficient for PEPc (unit?)
## kr: michaelis menten coefficient for carboxylation activity of Rubisco (unit?)
## chi: stomatal conductance ration (unitless)
## ci: internal (mesophyll) CO2 pressure (Pa)
## cbs: bundle sheath CO2 pressure (Pa)
## obs: bundle sheath O2 pressure (Pa)
## jmax,
## vpmax: PEPc (unit)
## vcmax: Rubsico-activity (unit?)
## Al: light-limited photosynthesis (unit?)
## Ap: PEPc-limited photosynthesis (unit?)
## Ac: Rubisco-limited photosynthesis (unit?)

C4model <- function(tg_c = 25, z = 0, vpdo = 1, cao = 400, 
                  paro = 800, q025 = 0.25, theta = 0.85, R = 8.314){
  
  # environmental terms
  patm <- calc_patm(z)
  par <- calc_par(paro, z)
  vpd <- calc_vpd(tg_c, z, vpdo)
  ca <- cao * 1e-6 * patm
  
  # Calculate Gamma star
  gamma_star <- calc_gammastar_pa(tg_c, z) # pa
  
  # calc chi
  chi <- calc_chi_xi_resp(ca, tg_c, vpd, z, gamma_star)[1]
  
  # calc ci ( = cm)
  ci <- chi * ca # Eqn. 2.36
  cm <- ci
    
  # Light Limited Photosynthesis
    
  # Jmax stuff
  omega <- calc_/mega(theta = theta, c = 0.079, m = 1) # Eq. S4
  omega_star <- (1 + (omega) - sqrt((1 + (omega))^2 - (4 * theta * omega)))  # Eq. 18
  
  q0 = q025 * phi_ftemp(tg_c)
  Al <- q0 * par * omega_star / (8 * theta) # Eqn. 2.2
  jmax = q0 * par * omega
    
  # calc kp
  kp <- calc_kp_temp_pa(tg_c, z) # Eqn. 2.43
  # calc kr
  kr <- calc_kc_temp_pa(tg_c, z) # Eqn. 2.48
  # calc ko
  ko <- calc_ko_temp_pa(tg_c, z)
   
  # calc vpmax
  vpmax <- ((kp + cm)/cm) * (q0 * par * omega_star / (8 * theta)) # Eqn. 2.42
  Ap <- vpmax * (cm / (cm + kp))
  
  # calc cbs
  cbs <- calc_cbs(z, Al, vpmax, cm) # Eqn. 2.41
  # calc obs
  chi_2 <- cbs/cm
  om <- 2.09476e5
  om_pa <- om * (1e-6) * patm
  obs <- om * chi_2
  
  # calc vcmax
  vcmax <- Ap * ((cbs + kr * (1 + obs/ko)) / cbs) # Eqn. 2.47
  Ac <- vcmax * ((cbs - gamma_star) / (kr * (1 + obs/ko) + cbs)) # Eqn. 2.4
  
  results <- data.frame("tg_c" = tg_c,
                        "par" = par,
                        "ca" = ca,
                        "z" = z,
                        "vpd" = vpd,
                        "q0" = q0,
                        "kp" = kp,
                        "kr" = kr,
                        "chi" = chi,
                        "ci" = ci,
                        "cbs" = cbs,
                        "obs" = obs,
                        "jmax" = jmax,
                        "vpmax" = vpmax,
                        "vcmax" = vcmax,
                        "Al" = Al,
                        "Ap" = Ap,
                        "Ac" = Ac)
  return(results)
}
