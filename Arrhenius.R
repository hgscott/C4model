# Arrhenius calcualtes the temperatrue effected value of a constant with a reference value at 25 degreec C
# Written by Helen Scott (2018)
#
# Arguments
# Parameter_25: The reference vlaue at 25 degrees C
# E: Activation Energy
# Temp: Temperature (in C) for which the value needs to be calculated
#
# Returns
# Parameter_T: The value for the given parameter at the specified temperature
Arrhenius <- function(Parameter_25, E, Temp) {
  Parameter_T <- Parameter_25 * exp( ((Temp - 25) * E) / (298 * R *(Temp + 273)) ) 
  return(Parameter_T)
}