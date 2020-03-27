# Instantaneous temp response of Vcmax
# from Kattge Knorr 2007
# Equation 1
# using for both Vcmax and Vpmax

licor_temp_cor_vc <- function(licor_temp, gs_temp, licor_v) {
  
  kT <- licor_v
  
  # Assign temps
  temp <- licor_temp + 273
  trefK <- gs_temp + 273
  
  # Constants
  R <- 8.314

  # Massad Values, even though they suck
  Ha <- 67294 # J mol^-1
  delta_s <- 472
  Hd <- 144568 # J mol^-1
  
  # Have to invert original equaiton so that I can calculate k25 (the growing season temp)
  kbeg=exp(Ha*(temp-trefK)/(trefK*R*temp))
  kend=((1+exp((trefK*delta_s - Hd)/(trefK * R)))/(1+exp((temp * delta_s-Hd)/(temp*R))))
  kbeg*kend # Equation 20 in Smith 2019
  k25 <- kT / (kbeg * kend)
  gs_mean_v <- k25
  return(gs_mean_v)
}