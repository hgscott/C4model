# Instantaneous temp response of Vcmax
# from Kattge Knorr 2007
# Equation 1
# using for both Vcmax and Vpmax

kk_Vc_temp <- function(licor_temp, gs_temp, licor_vc) {
  
  kT <- licor_vc
  
  # Assign temps
  temp <- licor_temp + 273
  trefK <- gs_temp + 273

  # Means from table 3
  Ha <- 71.513 # J mol^-1
  delta_s <- 649.12 # OR 668.39, from a? # J mol^-1 C^-1, what do I use now that I'm not sandwiching it around something????
  Hd <- 200000 # J mol^-1
  
  # Have to invert original equaiton so that I can calculate k25 (the growing season temp)
  kbeg=exp(Ha*(temp-trefK)/(trefK*R*temp))
  kend=((1+exp((trefK*delta_s - Hd)/(trefK * R)))/(1+exp((temp * delta_s-Hd)/(temp*R))))
  kbeg*kend # Equation 20 in Smith 2019
  k25 <- kT / (kbeg * kend)
  gs_mean_vc <- k25
  return(gs_mean_vc)
}