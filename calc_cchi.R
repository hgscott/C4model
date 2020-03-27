# Calculate chi
# Cause I'm a mother flipping genius
# Take that

calc_chi <- function(rh, z, temp){
  P_atm <- Barom.Press(elevation.m = z, units = "kpa")
  vpd <- RHtoVPD(RH = rh, TdegC = temp, Pa = P_atm)
  D0 <- calc_d0(vpd = vpd, temp = temp, z = z)
  temp_k <- temp + 273
  delta_t <- 298 - temp_k 
  z_km <- z/1000
  logit_chi <- (0.08805 * delta_t)- (0.057 * z_km) - (0.5 * D0) + 0.330747
  chi <- exp(logit_chi)/(1 + exp(logit_chi))
  return(chi)
}