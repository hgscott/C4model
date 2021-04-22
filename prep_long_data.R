prep_long_data <- function(tg_c = 25, z = 0, vpdo = 1, cao = 400, oao = 209460, 
                           paro = 800, theta = 0.85, leakiness = 0.01, 
                           R = 8.314){
  # Get standard values for C4 (acclimated to 25)
  std <- C4model()
  
  # C4 acclimated
  # Run model
  c4_seq <- C4model(tg_c, z, vpdo, cao, oao, paro, theta, leakiness, R)
  std_values <- apply(c4_seq, 1, function(x) x/std)
  df <- data.frame(matrix(unlist(std_values), nrow=length(std_values), byrow=T))
  names(df) <- lapply(names(c4_seq), function(x) paste(x, "_std", sep=""))
  df_sub <- select(df,
                   Ac_std,
                   jvc_ratio_std, jvp_ratio_std, vcvp_ratio_std,
                   chi_m_std, chi_bs_std,
                   jmax_std, vpmax_std, vcmax_std)
  c4_sub <- select(c4_seq,
                   tg_c, par, vpd, cao,
                   Ac)
  c4_final <- cbind(c4_sub, df_sub)
  c4_final$Path <- "C4"
  c4_final$Status <- "Acclimated"
  
  # Convert to long form
  c4_long <- gather(c4_final, measure, value, -tg_c, -par, -vpd, -cao, -Path, -Status)
  
  # C3 model
  c3_seq <- calc_optimal_vcmax(tg_c = tg_c, z = z, vpdo = vpdo, 
                              cao = cao, paro = paro, 
                              theta = theta)
  c3_seq$A <- c3_seq$vcmax * c3_seq$mc
  # Get just A
  c3_sub <- select(c3_seq,
                   tg_c, par, vpd, cao,
                   A)
  c3_sub$Path = "C3"
  c3_sub$Status = "Acclimated"
  # Convert to long
  c3_long <- gather(c3_sub, measure, value, -tg_c, -par, -vpd, -cao, -Path, -Status)
  
  # C4 instantaneous response
  instant_seq <- Instantaenous_Response(tg_c = tg_c, z = z, vpdo = vpdo, 
                                        cao = cao, oao = oao, paro = paro, 
                                        theta = theta, R = R)
  instant_sub <- select(instant_seq,
                        tg_c, paro, vpdo, cao,
                        An)
  names(instant_sub) <- c("tg_c", "par", "vpd", "cao", "An")
  instant_sub$Path = "C4"
  instant_sub$Status = "Uncclimated"
  # Convert to long
  instant_long <- gather(instant_sub, measure, value, -tg_c, -par, -vpd, -cao, -Path, -Status)
  
  # Bind all the types together
  total_long <- rbind(c4_long, c3_long, instant_long)
  
  return(total_long)
}