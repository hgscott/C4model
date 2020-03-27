# Nicks abc vc/vp max temp funtion

calc_abc <- function(a, b, c, temp){
  R_t <- exp(a + b * temp + c * temp^2)
  return(R_t)
}