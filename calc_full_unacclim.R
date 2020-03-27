# unacclimated single function, I'm trying
# 03/17/2019
# Helen Scott

# When acclimated to ideal conditions (tg_c = 25, z = 0, vpd = 1, ca = 400, I = 800, phi = 0.0528)

calc_full_unacclim <- function(tg_c = 25, z = 0, vpd = 1, ca = 400, I = 800, phi = 0.0528){
  library(rMR) # Not sure why I need this
  library(R.utils)
  
  # Make it work on personal machine
  setwd("C:/Users/HGS/Desktop/C4acclimation-nick_analyses/nick_analyses")
  
  # pmodel
  source("pmodel.R")
  sourceDirectory("functions/", modifiedOnly = FALSE)
  
  # Constants
  R <- 8.3145 #J mol^-1 K
  R <- R/1000 #kJ mol^1 K
  
  par <- calc_par(I, z)
  
  abs = 0.8 #leaf quantu, absorptance; 0.8 (Norman and Polley 1989,Collatz 1992)
  alpha_r = 0.11 # RuBP Quantum Requirement, Berry & Farquhar 1978, Collatz 1992
  f <- 0.6 # Fractional RuBP quantum requirement, Berry & Farquhar 1978, Collatz 1992
  phi_25 <- abs * alpha_r * f # quantum efficiency (mol mol-1)
  
  kr25 <- 65 # Michaelis-Menten coefficient or rubsico for CO2 at 25C (Pa); 65 from Von Caemmerer
  E_Km_C <- 80990 #(Bernacchi et. al 2002)
  
  Vpmax_unacclimated <- 120 # micromol m-2 s-1 (Von Caemmerer 2000)
  Vcmax_unacclimated <- 60 # micromol m-2 s-1 (Von Caemmerer 2000)
  Jmax_unacclimated <- 400 # micromol electrons m-2m s-1 (Von Caemmerer 2000)
  
  chi <- calc_chi_xi(ca = 400, temp = 25, vpd = 1, z = 0)
  ci <- chi * (ca * 1e-6 * calc_patm(z))
  # calc kp
  kp <- calc_kp_temp_pa(tg_c, z)
  # calc kr
  kr <- calc_kc_temp_pa(tg_c, z)
  # calc vpmax
  Vpmax_pred <- calc_vpmax(ci, I, kp, phi)
  # calc cbs
  cbs <- calc_cbs(ci, I, Vpmax_pred, phi)
  # calc vpmax'
  Vpmax_prime <- calc_vpmax_prime(Vpmax_pred, tg_c)
  # calc vcmax
  Vcmax_pred <- calc_vcmax(cbs, I, kr, phi)
  # calc Vcmax'
  Vcmax_prime <- calc_vcmax_prime(Vcmax_pred, tg_c)
  # Pick the min
  Vpmax <- min(Vpmax_prime, Vpmax_unacclimated)
  Vcmax <- min(Vcmax_prime, Vcmax_unacclimated)
  # Calc the Photos
  Al <- min((phi_25 * par), Jmax_unacclimated)
  Ac <- calc_Ac(cbs, Vcmax, kr)
  Ap <- calc_Ap(ci, Vpmax, kp)
  
  res <- data.frame("Temp" = tg_c, "Light" = I, "CO2" = ca, "Elev" = z, "VPD" = vpd, "Phi" = phi, "Kp" = kp, "Kr" = kr, "chi" = chi, "Ci" = ci, "Cbs" = cbs, "Vpmax" = Vpmax_prime, "Vcmax" = Vcmax_prime, "Al" = Al, "Ap" = Ap, "Ac" = Ac)
  An <- apply(res[, 14:16], 1, min)
  res <- cbind(res, An)
  return(res)
}