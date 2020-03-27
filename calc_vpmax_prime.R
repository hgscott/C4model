calc_vpmax_prime <- function(Vpmax_pred, t_gc_C){
  to <- (0.44 * t_gc_C + 24.92) # Number changed from paper for Celsius instead of K, using same as for Vcmax, cause I don't know how you caculate this. Nick? I guess I should just read the paper.
  vpmax_prime <- Vpmax_pred * calc_tresp_mult_vp(t_gc_C, t_gc_C, tref = to)
  return(vpmax_prime)
}