calc_kp_temp_M <- function(temp){ # Boyd 2015
  temp_k <- temp + 273
  kp_25 <- 60.5 # micro mol HCO3-
  Ea <- 27.2 # kJ mol-1
  kp <- kp_25 * exp(Ea * (temp_k - 298.15)/(298.25 * R * temp_k))
  return(kp)
}