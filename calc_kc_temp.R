calc_kc_temp <- function(temp){ # All from Zhou 2018, I really am not sure, need to ask Nick how to proceed
  temp_k <- temp + 273
  kc_25 <- 302 # micro mol m-2 s-1
  c <- 32.61 # micro mol mol-1
  delta_ha <- 79.73 # kJ mol-1
  R <- 0.008314 #kJ K-1 mol-1
  kc <- kc_25 * exp(c - delta_ha/(R * temp_k))
  return(kc)
}