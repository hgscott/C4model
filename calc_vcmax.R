calc_vcmax <- function(Ap, cbs, kr){
  # kp <- calc_kp_temp_pa(temp)
  # Vcmax <- (cm * vpmax * (kr + cbs)) / ((kp + cm) * cbs)
  Vcmax <- Ap * ((kr + cbs) / cbs)
  return(Vcmax)
}