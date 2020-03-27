calc_Ap <- function(cm, Vpmax, kp){
  Ap <- (cm * Vpmax)/(kp + cm)
  return(Ap)
}