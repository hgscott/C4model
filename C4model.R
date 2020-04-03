# C4model: Predicts acclimated 
# Helen Scott
# Last Updated: 03/27/2020
#
# Arguments
## tg_c: acclimated temperature (degC)
## z: elevation (m)
## vpdo: vapor pressure deficit at sea level (kPa)
## cao: atmospheric CO2 at sea level (Pa)
## paro: photosynthetically active radiation at sea level (µmol m-2 s-1)
## q025: quantum efficiency of photosynthetic electron transport (mol/mol)
## theta: curvature of the light response of electron transport (unitless)
## R: universal gas constant (J mol-1 K-1)
#
# Returns
## tg_c: acclimated temperature (degC)
## par: photosynthetically active radiation at z (µmol m-2 s-1)
## ca: atmospheric CO2 at z (Pa)
## z: elevation (m)
## vpd: vapor pressure deficit at z (kPa)
## q0: quantum efficiency of photosynthetic electron transport at tg_c (mol/mol)
## kp: michaelis menten coefficient for PEPc (Pa)
## kr: Michaelis-Menten constant for Rubisco carboxylation (Pa)
## chi: leaf intercellular to atmospheric CO2 ratio (ci/ca) (unitless)
## ci: leaf intercellular CO2 concentation (Pa)
## cbs: bundle sheath CO2 pressure (Pa)
## obs: bundle sheath O2 pressure (Pa)
## jmax: optimal maximum rate of electron transport at tg (µmol m-2 s-1)
## vpmax: maximum rate of PEPc (µmol m-2 s-1)
## vcmax: maximum rate of Rubisco carboxylation (µmol m-2 s-1)
## Al: light-limited photosynthesis (µmol m-2 s-1)
## Ap: PEPc-limited photosynthesis (µmol m-2 s-1)
## Ac: Rubisco-limited photosynthesis (µmol m-2 s-1)

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
  chi <- calc_chi_xi_resp(ca, tg_c, vpd, z, gamma_star)
  
  # calc ci ( = cm)
  ci <- chi * ca # Eqn. 2.36
  cm <- ci
    
  # Light Limited Photosynthesis
    
  # Jmax stuff
  omega <- calc_omega(theta = theta, c = 0.079, m = 1) # Eq. S4
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
