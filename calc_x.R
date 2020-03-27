# Calcualte chi(x)
# For when you are given vpd, not rh, so likek 99% of the time for me
# Write a real header Helen, stop being the worst

calc_x <- function(vpd, z, temp){
  D0 <- calc_d0(vpd = vpd, temp = temp, z = z)
  temp_k <- temp + 273
  delta_t <- 298 - temp_k 
  z_km <- z/1000
  logit_chi <- (0.08805 * delta_t)- (0.057 * z_km) - (0.5 * D0) + 0.330747
  chi <- exp(logit_chi)/(1 + exp(logit_chi))
  return(chi)
}