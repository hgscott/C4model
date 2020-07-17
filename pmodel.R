# combined C3 and C4 models

pmodel = function(tg_c = 25, z = 0, vpdo = 1, cao = 400, 
                  paro = 800, q025 = 0.25, theta = 0.85, R = 8.314, c3c4 = "c3"){
  
  # environmental terms
  patm <- calc_patm(z)
  par <- calc_par(paro, z)
  vpd <- calc_vpd(tg_c, z, vpdo)
  ca <- cao * 1e-6 * patm
  to <- (0.44 * tg_c + 24.92)              # Eq. 21, note: intercept differs due to use of °C
  tg_K <- tg_c + 273.15
  
  if(c3c4 == 'c3'){
    
    # K and Gamma* model terms
    km <- calc_km_pa(tg_c, z)                # Eq. 3
    gammastar <- calc_gammastar_pa(tg_c, z)   
    
    # Coordination and least-cost hypothesis model terms
    chi <- calc_chi_c3(tg_c, z, vpdo, cao)              # Eq. 1
    ci <- chi * ca # Pa
    mc <- ((ci - gammastar) / (ci + km))             # Eq. 6
    m <- ((ci - gammastar)/(ci + (2 * gammastar)))   # Eq. 8
    omega <- calc_omega(theta = theta, c = 0.053, m = m) # Eq. S4
    omega_star <- (1 + (omega) - sqrt((1 + (omega))^2 - (4 * theta * omega)))  # Eq. 18
    
    # calculate q0
    q0 = q025 * phi_ftemp(tg_c) # uses dongsansuk
    
    # calculate vcmax and jmax	
    vcmax <- ((q0 * par * m) / mc) * (omega_star / (8 * theta))	          # Eq. 19
    jvrat <- ((8 * theta * mc * omega) / (m * omega_star))     # Eq. 15 / Eq. 19
    jmax <- jvrat * vcmax
    
    A = vcmax * mc
    
    # equivalently
    # jmax_prime <- q0 * par * omega * calc_tresp_mult(tg_c, tg_c, tref = to)    # Eq. 15 * Eq. 20
    # jvrat <- jmax_prime/vcmax_prime
    
    # output
    results <- data.frame("tg_c" = tg_c,
                          "par" = par,
                          "cao" = cao,
                          "ca" = ca,
                          "z" = z,
                          "vpd" = vpd,
                          "q0" = q0,
                          "kr" = km,
                          "chi" = chi,
                          "ci" = ci,
                          "vcmax" = vcmax,
                          "jmax" = jmax,
                          "jvrat" = jvrat,
                          "A" = A)
    
    return(results)
    
  }
  
  else if(c3c4 == 'c4'){
    
    # calc chi
    chi <- calc_chi(cao, tg_c, vpd, z, gamma_star)
    
    # calc ci ( = cm)
    ci <- chi * ca # Eqn. 2.36
    cm <- ci
    
    # Light Limited Photosynthesis
    
    # Jmax stuff
    omega <- calc_omega(theta = theta, c = 0.079, m = 1) # Eq. S4
    omega_star <- (1 + (omega) - sqrt((1 + (omega))^2 - (4 * theta * omega)))  # Eq. 18
    
    # Doing this here so I can use the variable in the Vpmax calculation
    q0 = q025 * phi_ftemp(tg_c)
    Al <- q0 * par * omega_star / (8 * theta) # Eqn. 2.2
    jmax = q0 * par * omega
    
    # calc kp
    kp <- calc_kp_temp_pa(tg_c, z) # Eqn. 2.43
    # calc kr
    kr <- calc_kc_temp_pa(tg_c, z) # Eqn. 2.48
    
    # calc vpmax
    vpmax <- ((kp + cm)/cm) * (q0 * par * omega_star / (8 * theta)) # Eqn. 2.42
    Ap <- vpmax * (cm / (cm + kp))
    
    # calc cbs
    cbs <- calc_cbs(z, Al, vpmax, cm) # Eqn. 2.41
    
    # calc vcmax
    vcmax <- Ap * ((kr + cbs) / cbs) # Eqn. 2.47
    Ac <- vcmax * (cbs / (kr + cbs)) # Eqn. 2.4
    
    
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
                          "jmax" = jmax,
                          "vpmax" = vpmax,
                          "vcmax" = vcmax,
                          "A" = Al)
    return(results)
    
  }
  
  else{
    
    print('c3c4 error')
    
  }
  
}
