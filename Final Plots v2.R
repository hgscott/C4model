# Load packages
library(R.utils)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
# library(viridis)
# library(rpmodel) # Not using anymore

# Source model code and functions
source("Model/C4model.R")
sourceDirectory("Model/functions/", modifiedOnly = FALSE)
source("Model/pmodel.R")

# Run model across all the variables
temp_seq <- C4model(tg_c = c(1:40))
par_seq <- C4model(paro = c(0:1000))
vpd_seq  <- C4model(vpdo = c(0:8))
co2_seq <- C4model(cao = c(5:1000))

# Run for C3 across the same sequences
c3_temp_seq <- pmodel(tg_c = c(1:40))
c3_par_seq <- pmodel(paro = c(0:1000))
c3_vpd_seq  <- pmodel(vpdo = c(0:8))
c3_co2_seq <- pmodel(cao = c(5:1000))

# Chi Plots (Figure X)
#####
chi_temp <- ggplot(temp_seq, aes(tg_c, chi)) + geom_line(size = 2) + 
  lims(y = c(0.3, 1)) + 
  labs(y = "Ci/Ca Ratio", x = "Temperature (C)") + 
  theme(axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
chi_light <- ggplot(par_seq, aes(par, chi)) + geom_line(size = 2) +
  lims(y = c(0.3, 1)) + 
  labs(x = expression(paste(Light," (", mu * mol %.% m^{-2} %.% s^{-1}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
chi_co2 <- ggplot(co2_seq, aes(ca, chi)) + geom_line(size = 2) + 
  lims(y = c(0.3, 1)) + 
  labs(y = "Ci/Ca Ratio", x = expression(paste('CO'[2], " (ppm)"))) + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
chi_vpd <- ggplot(vpd_seq, aes(vpd, chi)) + geom_line(size = 2) +
  lims(y = c(0.3, 1)) + 
  labs(x = "Vapour Pressure Deficit (kPa)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
grid.arrange(chi_temp, chi_light, chi_co2, chi_vpd, nrow = 2)
#####

# Vpmax Plots (Figure X)
#####
vp_temp <- ggplot(temp_seq, aes(tg_c, vpmax)) + geom_line(size = 2) + 
  lims(y = c(0, 100)) + 
  labs(y = expression(paste('V'[pmax]," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")),
       x = "Temperature (C)") + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
vp_light <- ggplot(par_seq, aes(par, vpmax)) + geom_line(size = 2) +
  lims(y = c(0, 100)) +
  labs(x = expression(paste(Light," (", mu * mol %.% m^{-2} %.% s^{-1}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
vp_co2 <- ggplot(co2_seq, aes(ca, vpmax)) + geom_line(size = 2) + 
  # lims(y = c(0, 100)) + 
  labs(y = expression(paste('V'[pmax]," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")), 
       x = expression(paste('CO'[2], " (ppm)"))) + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
vp_vpd <- ggplot(vpd_seq, aes(vpd, vpmax)) + geom_line(size = 2) +
  lims(y = c(0, 100)) + 
  labs(x = "Vapour Pressure Deficit (kPa)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
grid.arrange(vp_temp, vp_light, vp_co2, vp_vpd, nrow = 2)
#####

# CCM Plots (Figure X) ## NOT DONE
#####
#####

# Vcmax Plots (Figure X)
#####
vc_temp <- ggplot(temp_seq, aes(tg_c, vcmax)) + geom_line(size = 2) + 
  lims(y = c(0, 170)) + 
  labs(y = expression(paste('V'[cmax]," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")),
       x = "Temperature (C)") + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
vc_light <- ggplot(par_seq, aes(par, vcmax)) + geom_line(size = 2) +
  lims(y = c(0, 170)) + 
  labs(x = expression(paste(Light," (", mu * mol %.% m^{-2} %.% s^{-1}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
vc_co2 <- ggplot(co2_seq, aes(ca, vcmax)) + geom_line(size = 2) + 
  lims(y = c(0, 170)) + 
  labs(y = expression(paste('V'[cmax]," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")), 
       x = expression(paste('CO'[2], " (ppm)"))) + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
vc_vpd <- ggplot(vpd_seq, aes(vpd, vcmax)) + geom_line(size = 2) +
  lims(y = c(0, 170)) +  
  labs(x = "Vapour Pressure Deficit (kPa)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
grid.arrange(vc_temp, vc_light, vc_co2, vc_vpd, nrow = 2)
#####

# A Plots, C4 only (Figure X)
#####
a_temp <- ggplot(temp_seq, aes(tg_c, Al)) + geom_line(size = 2) + 
  lims(y = c(0, 60)) + 
  labs(y = expression(paste(Photosynthesis," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")),
       x = "Temperature (C)") + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
a_light <- ggplot(par_seq, aes(par, Al)) + geom_line(size = 2) +
  lims(y = c(0, 60)) +
  labs(x = expression(paste(Light," (", mu * mol %.% m^{-2} %.% s^{-1}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
a_co2 <- ggplot(co2_seq, aes(ca, Al)) + geom_line(size = 2) + 
  lims(y = c(0, 60)) +
  labs(y = expression(paste(Photosynthesis," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")),
       x = expression(paste('CO'[2], " (ppm)"))) + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
a_vpd <- ggplot(vpd_seq, aes(vpd, Al)) + geom_line(size = 2) +
  lims(y = c(0, 60)) +
  labs(x = "Vapour Pressure Deficit (kPa)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
grid.arrange(a_temp, a_light, a_co2, a_vpd, nrow = 2)
#####

# A Plots, C4 vs C3 (Figure X)
#####
temp_seq$Type = "C4"
c3_temp_seq$Type = "C3"

par_seq$Type = "C4"
c3_par_seq$Type = "C3"

co2_seq$Type = "C4"
c3_co2_seq$Type = "C3"

vpd_seq$Type = "C4"
c3_vpd_seq$Type = "C3"

a3v4_temp <- ggplot(temp_seq, aes(tg_c, Al, color = Type)) + geom_line(size = 2) +
  geom_line(data = c3_temp_seq, aes(tg_c, A, color = Type), size = 2, linetype = "dashed") +
  lims(y = c(0, 60)) + 
  labs(y = expression(paste(Photosynthesis," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")),
       x = "Temperature (C)") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)
  )
a3v4_light <- ggplot(par_seq, aes(par, Al, color = Type)) + geom_line(size = 2) +
  geom_line(data = c3_par_seq, aes(par, A, color = Type), size = 2, linetype = "dashed") +
  lims(y = c(0, 60)) +
  labs(x = expression(paste(Light," (", mu * mol %.% m^{-2} %.% s^{-1}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

a3v4_co2 <- ggplot(co2_seq, aes(ca, Al, color = Type)) + geom_line(size = 2) +
  geom_line(data = c3_co2_seq, aes(ca, A, color = Type), size = 2, linetype = "dashed") +
  lims(y = c(0, 60)) +
  labs(y = expression(paste(Photosynthesis," (", mu * mol %.% m^{-2} %.% s^{-1}, ")")),
       x = expression(paste('CO'[2], " (ppm)"))) + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
a3v4_vpd <- ggplot(vpd_seq, aes(vpd, Al, color = Type)) + geom_line(size = 2) +
  geom_line(data = c3_vpd_seq, aes(vpd, A, color = Type), size = 2, linetype = "dashed") +
  lims(y = c(0, 60)) +
  labs(x = "Vapour Pressure Deficit (kPa)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
ggarrange(a3v4_temp, a3v4_light, a3v4_co2, a3v4_vpd, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
#####

# Delta A Line Graphs (Figure X)
#####
temp_seq$Delta = ((temp_seq$Ac - c3_temp_seq$A) / c3_temp_seq$A) * 100
par_seq$Delta = ((par_seq$Ac - c3_par_seq$A) / c3_par_seq$A) * 100
co2_seq$Delta = ((co2_seq$Ac - c3_co2_seq$A) / c3_co2_seq$A) * 100
vpd_seq$Delta = ((vpd_seq$Ac - c3_vpd_seq$A) / c3_vpd_seq$A) * 100

delta_temp <- ggplot(temp_seq, aes(tg_c, Delta)) + geom_line(size = 2) +
  labs(x = "Temperature (C)",
    y = expression(paste(Delta, A," (%)"))) +
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

delta_light <- ggplot(par_seq, aes(par, Delta)) + geom_line(size = 2) +
  # lims(y = c(0, 60)) +
  labs(x = expression(paste(Light," (", mu * mol %.% m^{-2} %.% s^{-1}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

delta_co2 <- ggplot(co2_seq, aes(ca, Delta)) + geom_line(size = 2) +
  # lims(y = c(0, 60)) +
  labs(y = expression(paste(Delta, A," (%)")),
       x = expression(paste('CO'[2], " (ppm)"))) + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )
delta_vpd <- ggplot(vpd_seq, aes(vpd, Delta)) + geom_line(size = 2) +
  # lims(y = c(0, 60)) +
  labs(x = "Vapour Pressure Deficit (kPa)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
  )

grid.arrange(delta_temp, delta_light, delta_co2, delta_vpd, nrow = 2)
#####

# Delta A Multiple CO2 scenarios Line graphs (Overlayed) (Figure X)
#####
lowCO2_temp_seq <- C4model(tg_c = c(1:40), cao = 200)
c3_lowC02 <- pmodel(tg_c = c(1:40), cao = 200)
lowCO2_temp_seq$Delta = ((lowCO2_temp_seq$Ac - c3_lowC02$A) / c3_lowC02$A) * 100
highCO2_temp_seq <- C4model(tg_c = c(1:40), cao = 1000)
c3_highC02 <- pmodel(tg_c = c(1:40), cao = 1000)
highCO2_temp_seq$Delta = ((highCO2_temp_seq$Ac - c3_highC02$A) / c3_highC02$A) * 100

lowCO2_vpd_seq <- C4model(vpdo = c(0:8), cao = 200)
c3_lowC02_vpd <- pmodel(vpdo = c(0:8), cao = 200)
lowCO2_vpd_seq$Delta = lowCO2_vpd_seq$Ac - c3_lowC02_vpd$A
highCO2_vpd_seq <- C4model(vpdo = c(0:8), cao = 1000)
c3_highC02_vpd <- pmodel(vpdo = c(0:8), cao = 1000)
highCO2_vpd_seq$Delta = highCO2_vpd_seq$Ac - c3_highC02_vpd$A

delta_temp_coseq <- ggplot(temp_seq, aes(tg_c, Delta, color = ca)) + geom_line(size = 2) +
  geom_line(data = lowCO2_temp_seq, aes(tg_c, Delta, color = ca), size = 2, linetype = "dashed") +
  geom_line(data = highCO2_temp_seq, aes(tg_c, Delta, color = ca), size = 2, linetype = "dotdash") +
  labs(x = "Temperature (C)",
       y = expression(paste(Delta, A," (%)"))) +
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_color_distiller(palette = "Spectral")

delta_vpd_coseq <- ggplot(vpd_seq, aes(vpd, Delta, color = ca)) + geom_line(size = 2) +
  geom_line(data = lowCO2_vpd_seq, aes(vpd, Delta, color = ca), size = 2, linetype = "dashed") +
  geom_line(data = highCO2_vpd_seq, aes(vpd, Delta, color = ca), size = 2, linetype = "dotdash") +
  # lims(y = c(0, 60)) +
  labs(x = "Vapour Pressure Deficit (kPa)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_color_distiller(palette = "Spectral")

ggarrange(delta_temp_coseq, delta_vpd_coseq, nrow = 1, common.legend = TRUE, legend="right")
#####