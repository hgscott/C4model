# calc_iso_chi
# Calucaltes observed chi calue from isotope and climate data
# Wang Han 2017 eqn 28 
# Input variables
## Delta: delta 13C
## T_l: Leaf temperature in C

calc_iso_chi <- function(Delta, T_l){
  phi <- 0.2
  a_prime <- 4.4
  
  b_3 <- 30 # possibly 29 (Guy 1993), Lloyd Farquhar 1994
  b_4 <- (26.19 - (9483/(273.2 + T_l))) * 10^-3 # is right number Lloyd Farquhar 1994
  b_fe <- b_4 + b_3 * phi
  
  chi <- (Delta - a_prime) / (b_fe - a_prime)
}