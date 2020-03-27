calc_vpmax <- function(cm, I, kp, phi){
  Vpmax <- ((kp + cm)/cm) * (phi * I)
  return(Vpmax)
}