# vpmax_calc calculates the Vcmax and Vpmax given environmental conditions
# Written by Helen Scott (modified from code originally written by Nick Smith)
#
# Arguments
# t: temperatyre (in C)
# I: light
# ca: ambient CO2 concentration (ppm)
# z: elevation (m)
vpmax_calc <- function(t, I, ca, z){
  patm = calc_patm(z) # atmospheric pressure
  
  kr <- Arrhenius(kr25, E_Km_C, t)
  kp <- Arrhenius(kp25, E_Km_C, t)
  
  ci = ca * 1e-6 * patm * chi
  cm <- ci
  cbs <- cm
  
  # Sane Line: vpmax = ((kp + cm)/cm) * (phi * I)
  #Insane line, broken somehow vpmax <- ((cbs * (phi * I)) / (kr + cbs)) * ((kp + cm) / cm)
  #What I had before
  vpmax = ((kp + ci) / ci) * ((((phi * I * (kr + ci))/ci) * ci)) / (kr + ci)
  #Nick's idea
  # What I have: 
  vcmax = (cm * vpmax * (kr + cbs)) / ((kp + cm) * cbs)
  #His idea: vcmax = (cm * vpmax) / (kp + cm)
  Al <- phi * I
  Ap <- (cm * vpmax) / (kp + cm)
  Ac <- (cbs * vcmax) / (kr + cbs)
  ratio <- vpmax/vcmax
  res <- cbind.data.frame(kp, kr, ci, vpmax, vcmax, Al, Ap, Ac, ratio)
  res$An <- Ac
  return(res)
}