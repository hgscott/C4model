# phi_fit <- function(phi){

library(readr)
library(ggplot2)

# Source Functions
source('~/School/Smith Research/Code/Functions/licor_temp_cor_vp.R')
source('~/School/Smith Research/Code/Functions/licor_temp_cor_vc.R')
source('~/School/Smith Research/Code/Functions/calc_RH_to_VPD.R')
source('~/School/Smith Research/Code/Functions/calc_full_model.R')

# Read in Field Data
#field_obsv_clim <- read_csv("~/School/Smith Research/Model Data Comparison for Poster/field_obsv_clim.csv")

# Read in Growth Camber Data
cont_env_obsv <- read_csv("~/School/Smith Research/Model Data Comparison for Poster/cont_env_obsv.csv")

# Licor temperature correction
#field_obsv_clim$Vcmax_cor <- licor_temp_cor_vc(field_obsv_clim$Licor_t, field_obsv_clim$temp, field_obsv_clim$Vcmax)
#field_obsv_clim$Vpmax_cor <- licor_temp_cor_vc(field_obsv_clim$Licor_t, field_obsv_clim$temp, field_obsv_clim$Vpmax)

cont_env_obsv$Vcmax_cor <- licor_temp_cor_vc(cont_env_obsv$Licor_t, cont_env_obsv$GrowthT, cont_env_obsv$Vcmax_obsv)
cont_env_obsv$Vpmax_cor <- licor_temp_cor_vc(cont_env_obsv$Licor_t, cont_env_obsv$GrowthT, cont_env_obsv$Vpmax_obsv)

# Calc for field data
#field_model <- calc_full_model(tg_c = field_obsv_clim$temp,z = field_obsv_clim$elevation,vpd = field_obsv_clim$vpd,ca = field_obsv_clim$CO2,I = field_obsv_clim$par,phi = 0.033)

# Calc for growth chamber data
growthchamber_model <- calc_full_model(tg_c = cont_env_obsv$GrowthT,
                                       z = cont_env_obsv$Elevation,
                                       vpd = calc_RH_to_VPD(cont_env_obsv$Humidty, cont_env_obsv$GrowthT),
                                       ca = cont_env_obsv$CO2,
                                       I = cont_env_obsv$PAR,
                                       phi = 0.007)

# Bind field obsv and expected together
# field_full <- cbind(field_obsv_clim, field_model)
# names(field_full)[22] <- "CO2_yravg"

# Bind growth chamber
growthchamber_full <- cbind(cont_env_obsv, growthchamber_model)
names(growthchamber_full)[16] <- "CO2_yravg"

# UNRELATED
# Vpmax/Vcmax Ratio Graph, I'll move this later, but not today
growthchamber_full$rat_obsv <- growthchamber_full$Vpmax_obsv/growthchamber_full$Vcmax_obsv
vpvc_rat_obsv <- ggplot(growthchamber_full, aes(GrowthT, rat_obsv)) + geom_point() + geom_smooth(method = "lm")
growthchamber_full$rat_pred <- growthchamber_full$Vpmax_prime/growthchamber_full$Vcmax
vpvc_rat_pred <- ggplot(growthchamber_full, aes(GrowthT, rat_pred)) + geom_point() + geom_smooth(method = "lm")

# Combine into one for easy plotting of combo graphs
# comp_comb <- data.frame("A_obs" = field_full$A_n, "Vc_obs" = field_full$Vcmax_cor, "Vp_obs" = field_full$Vpmax_cor, "A_pred" = field_full$Ac, "Vc_pred" = field_full$Vcmax, "Vp_pred" = field_full$Vpmax_prime)

gc_relev <- data.frame(growthchamber_full$An_obsv, growthchamber_full$Vcmax_cor, growthchamber_full$Vpmax_cor, growthchamber_full$Ac, growthchamber_full$Vcmax, growthchamber_full$Vpmax_prime)
# names(gc_relev) <- names(comp_comb)

# comp_comb <- rbind(comp_comb, gc_relev)

# Just the linear model for the phi fitting function
# Stupid names, but I'm just too lazy to change them right now
linear_a <- lm(growthchamber_full.An_obsv ~ growthchamber_full.Ac, data = gc_relev)
phi_a <- unlist(linear_a$coefficients[[2]])
linear_vc <- lm(growthchamber_full.Vcmax_cor ~ growthchamber_full.Vcmax, data = gc_relev)
phi_vc <- unlist(linear_vc$coefficients[[2]])
linear_vp <- lm(growthchamber_full.Vpmax_cor ~ growthchamber_full.Vpmax_prime, data = gc_relev)
phi_vp <- unlist(linear_vp$coefficients[[2]])
all <- c(phi_a, phi_vc, phi_vp)
all
# Make residulas graph
# comp_comb$resid_a <- NA
# comp_comb$resid_a[14] <- linear_a$residuals[[1]]
# comp_comb$resid_a[15] <- linear_a$residuals[[2]]
# comp_comb$resid_a[27:126] <- linear_a$residuals[3:102]
# comp_comb$resid_vc <- NA
# comp_comb$resid_vc[14] <- linear_vc$residuals[[1]]
# comp_comb$resid_vc[15] <- linear_vc$residuals[[2]]
# comp_comb$resid_vc[27:39] <- linear_vc$residuals[3:15]
# comp_comb$resid_vc[41:126] <- linear_vc$residuals[16:101]
# comp_comb$resid_vp <- NA
# comp_comb$resid_vp[47:126] <- linear_vp$residuals
# Attach the data with temps and stuff
# field_full$resid_a <- comp_comb$resid_a[1:89]
# field_full$resid_vc <- comp_comb$resid_vc[1:89]
# field_full$resid_vp <- comp_comb$resid_vp[1:89]
# growthchamber_full$resid_a <- comp_comb$resid_a[90:126]
# growthchamber_full$resid_vc <- comp_comb$resid_vc[90:126]
# growthchamber_full$resid_vp <- comp_comb$resid_vp[90:126]
# Actaully make the graphs
# I guess I gotta merge the datasets
# field_relev_lm <- data.frame(field_full$temp, field_full$par, field_full$CO2, field_full$vpd, field_full$resid_a, field_full$resid_vc, field_full$resid_vp)
# gc_relev_lm <- data.frame(growthchamber_full$GrowthT, growthchamber_full$PAR, growthchamber_full$CO2, growthchamber_full$VPD, growthchamber_full$resid_a, growthchamber_full$resid_vc, growthchamber_full$resid_vp)
# names(gc_relev_lm) <- names(field_relev_lm)
# relev_lm <- rbind(field_relev_lm, gc_relev_lm)
# library(reshape2)
# relev_lm_long <- melt(relev_lm, id = c("field_full.temp", "field_full.par", "field_full.CO2", "field_full.vpd"))

# resid_t <- ggplot(relev_lm_long, aes(field_full.temp, value, color = variable)) + geom_point() + labs(x = "Temperature (C)", y = "Residuals") + geom_smooth(method = lm)+ theme(legend.position = "none")

# resid_I <- ggplot(relev_lm_long, aes(field_full.par, value, color = variable)) + geom_point() + labs(x = "Light (micromol m^-2 s^-1)", y = "Residuals") + geom_smooth(method = lm) + theme(legend.position = "none")

# resid_c <- ggplot(relev_lm_long, aes(field_full.CO2, value, color = variable)) + geom_point() + labs(x = "CO2 (PPM)", y = "Residuals") + geom_smooth(method = lm) + theme(legend.position = "none")

# resid_v <- ggplot(relev_lm_long, aes(field_full.vpd, value, color = variable)) + geom_point() + labs(x = "VPD (kPa)", y = "Residuals") + geom_smooth(method = lm) + theme(legend.position = "none")



# return(all)
