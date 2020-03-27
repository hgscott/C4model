# calculate the temperature response multiplier for Vpmax based on Massad parameters

calc_tresp_mult_vp = function(tleaf, tmean, tref){
  
  temp = tleaf + 273.15
  Ha= 70373 # Activation Energy (J mol^-1) Smith, 2019
  Hd= 117910 # Deactivaiton energy (J mol^-1) Smith, 2019
  delS <- 376 # entropy term, Massad just gives one, not a function of temperature
  tmeanK=tmean+273.15
  trefK=tref+273.15
  R=8.314 # Universal Gas constant (J mol^-1 K^-1)
  kbeg=exp(Ha*(temp-trefK)/(trefK*R*temp))
  kend=((1+exp((trefK* delS -Hd)/(trefK*R)))/(1+exp((temp* delS -Hd)/(temp*R))))
  ktotal <- kbeg*kend # Equation 20 in Smith 2019
  return(ktotal)
}
