calc_Ac <- function(cbs, vcmax, kr){
  Ac <- (cbs * vcmax)/(kr + cbs)
}