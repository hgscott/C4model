# calc_d0
# Calcualte D_0 from vpd, temp, and elevation

calc_d0 <- function(vpd, temp, z) {
  es <- 610.8 * exp((17.27 * temp) / (237.3 + temp)) # saturation vapor pressure (Pa) http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit
  patm <- calc_patm(z) # actual atmospheric pressure
  po <- 101325 # Atmospheric pressure under standard conditions (i.e., sea level)
  # eao <- (1000 * vpd - es)/(patm / po)
  eao <- ((es * po)/patm) - ((1000 * vpd * po)/patm)
  d0 <- (-eao + es) / 1000
}