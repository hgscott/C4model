# calculate optimal vcmax as in Smith et al. Photosynthetic capacity is optimized to the 
# environment. Ecology Letters. 

# helen's re-do to just yield vcmax prime, given my modeled vcmax (vcmax_pred)
calc_vcmax_prime <- function(Vcmax_pred, t_gc_C){
  # Define optimum temperature
  # to <- (0.44 * t_gc_C) + 24.92 # Number changed from paper for Celsius instead of K
  # Already in the main model
  vcmax_prime <- Vcmax_pred * calc_tresp_mult(t_gc_C, t_gc_C, tref = to)
  
  # to <- to_c + 273.15

  # Temp response constants from Massad 2007
  # R <- 8.31 # J
  #  Ea <- 67294
  # adelS= 668.39
  # bdelS= -1.07
  # Hd <- 144568
  
  # Assign temps to KK variables
  # temp <- t_gc_C + 273.15
  # trefK <- to
  # tmean <- t_gc_C
  
  # KK function
  # kkbeg <- exp(Ea * (temp - trefK) / (trefK * R * temp))
  # kkend <-((1+exp((trefK*dS-Hd)/(trefK*R)))/(1+exp((temp*dS-Hd)/(temp*R))))
  # kkend <- ((1+exp((trefK*(adelS+bdelS*tmean)-Hd)/(trefK*R)))/(1+exp((temp*(adelS+bdelS*tmean)-Hd)/(temp*R))))
  
  # vcmax_prime <- Vcmax_pred * kkbeg * kkend
  return(vcmax_prime)
}