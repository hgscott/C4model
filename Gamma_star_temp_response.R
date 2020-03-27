# Gamma_star_temp_response
# Citation?

Gamma_star_temp_response <- function(Gamma_star, Temp){
  Hgm <- 37830 # J mol-1
  R <- 8.314        # J K-1 mol-1
  Temp_k <- 273.15 + Temp
  
  Gamma_star_temp <- Gamma_star*exp((Hgm/R)*(1/298.15-1/Temp_k))
}