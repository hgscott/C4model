# Bundle Sheath Conductance to Plug into whole model

calc_cbs <- function(z, Al, vpmax, cm) {
  patm <- calc_patm(z)

  gbs <- 3 # mmol m^-2 s^-1
  gbs <- gbs * 10^-3 # Convert from mmol to mol m^-2 s^-1
  
  A_gross <- Al
  Rd <- 0.04 * vpmax # Leaf mitochondrial respiration, 4 coming from rounding 3.9 or 4.4 % from the ecology LCE paper
  Rm <- 0.5 * Rd
  An <- A_gross - Rm
  CCM <- ((vpmax - An)/gbs) # micromol mol^-1 # NEEDS ADJUSTED
  
  CCM <- CCM * 1e-6 * patm # Convert to Pa # NEEDS ADJUSTED
  
  Cbs <- cm + CCM
  return(Cbs)
}