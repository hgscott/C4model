# Humidity to vpd function, becasue Nick reports the humidity of his chambers, but not the vpd
# Eqn from http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit

calc_RH_to_VPD <- function(rh, temp){
  SVP <- 610.7 * 10^((7.5 * temp) / (237.3 + temp)) # Pa
  VPD <- ((100 - rh)/100) * SVP
  VPD <- VPD / 1000
  return(VPD)
}
