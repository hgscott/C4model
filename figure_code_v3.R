# figure_code

# Libraries, check which ones are actually needed
library(R.utils)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(R.matlab)
library(raster)
library(dplyr)
library(tidyr)
library(reshape)

library(maps)
library(rgdal) # Can't install
library(SDMTools) # Can't install
library(RColorBrewer)
library(grDevices)
library(tiff) # Can't install

library(MODISTools) # Can't install

library(lme4)


# Source model code and functions
# setwd("~/GitHub/C4model")
source("C4model.R")
sourceDirectory("functions/", modifiedOnly = FALSE)
source("Instantaneous_Response.R")
source("prep_long_data.R")

# Source Nick Smith's C3 model
source('../optimal_vcmax_R/calc_optimal_vcmax.R')
sourceDirectory('../optimal_vcmax_R/functions', modifiedOnly = FALSE)

# Sizes of all images
width = 10
height = 6
units = "in"

# Line Graphs (Figures 2 - X)
################################################################################
# Generate data from models
#####
temp_seq <- prep_long_data(tg_c = c(5:40))
par_seq <- prep_long_data(paro = c(0:1000))
vpd_seq <- prep_long_data(vpdo = c(0:8))
co2_seq <- prep_long_data(cao = c(200:1000))

#### Plot Labels ####
labs <- c("Scaled Value",
          "Temperature (C)",
          expression(paste("Photosynthetically Active Radiation (", mu * mol %.% m^{-2} %.% s^{-1}, ")")),
          expression(paste('CO'[2], " (ppm)")),
          "Vapor Pressure Deficit (kPa)",
          expression(chi["m"]),
          expression(chi["bs"]),
          expression('J'[max]),
          expression('V'[cmax]),
          expression('V'[pmax]),
          expression(paste('J'[max], "/", 'V'[cmax])),
          expression(paste('J'[max], "/", 'V'[pmax])),
          expression(paste('V'[cmax], "/", 'V'[pmax])),
          " ",
          expression(paste("C"[3], " (Acclimated)")),
          expression(paste("C"[4], " (Acclimated)")),
          expression(paste("C"[4], " (Unacclimated)")),
          expression(paste(Photosynthesis," (", mu * mol %.% m^{-2} %.% s^{-1}, ")"))
          )

#### Chi Plot (Figure X) ####
chi_temp <- ggplot(filter(temp_seq, Path == "C4" & Status == "Acclimated" &
                          measure == "chi_m_std"|measure == "chi_bs_std"),
                   aes(tg_c, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed"), labels = labs[c(7, 6)]) +
  scale_color_manual(values=c("#000000", "#000000"), labels = labs[c(7, 6)]) +
  labs(y = labs[1], x = labs[2]) + 
  coord_cartesian(ylim = c(0.25, 1.6)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

chi_light <- ggplot(filter(par_seq, Path == "C4" & Status == "Acclimated" &
                            measure == "chi_m_std"|measure == "chi_bs_std"),
                   aes(par, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed"), labels = labs[c(7, 6)]) +
  scale_color_manual(values=c("#000000", "#000000"), labels = labs[c(7, 6)]) +
  labs(y= labs[14], x = labs[3]) +
  coord_cartesian(ylim = c(0.25, 1.6)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

chi_co2 <- ggplot(filter(co2_seq, Path == "C4" & Status == "Acclimated" &
                            measure == "chi_m_std"|measure == "chi_bs_std"),
                   aes(cao, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed"), labels = labs[c(7, 6)]) +
  scale_color_manual(values=c("#000000", "#000000"), labels = labs[c(7, 6)]) +
  labs(y = labs[1], x = labs[4]) +
  coord_cartesian(ylim = c(0.25, 1.6)) + 
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

chi_vpd <- ggplot(filter(vpd_seq, Path == "C4" & Status == "Acclimated" &
                            measure == "chi_m_std"|measure == "chi_bs_std"),
                   aes(vpd, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed"), labels = labs[c(7, 6)]) +
  scale_color_manual(values=c("#000000", "#000000"), labels = labs[c(7, 6)]) +
  labs(y = labs[14], x = labs[5]) +
  coord_cartesian(ylim = c(0.25, 1.6)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

ggarrange(chi_temp, chi_light, chi_co2, chi_vpd, ncol = 2, nrow = 2,
          common.legend = TRUE, legend="bottom", labels = c("A", "B", "C", "D"),
          hjust = c(0, 0.25, 0, 0.25))
ggsave("chi.png", width = width, height = width, units = units)

#### Biochemical Variables ####
bc_temp <- ggplot(filter(temp_seq, Path == "C4" & Status == "Acclimated" &
                           measure == "jmax_std" | measure == "vpmax_std" |
                           measure == "vcmax_std"),
                  aes(tg_c, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[8:10]) +
  scale_color_manual(values=c("#473C8B", "#3288BD", "#D53E4F"), labels = labs[8:10]) +
  labs(y = labs[1], x = labs[2]) + 
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

bc_light <- ggplot(filter(par_seq, Path == "C4" & Status == "Acclimated" &
                           measure == "jmax_std" | measure == "vpmax_std" |
                           measure == "vcmax_std"),
                  aes(par, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[8:10]) +
  scale_color_manual(values=c("#473C8B", "#3288BD", "#D53E4F"), labels = labs[8:10]) +
  labs(y = labs[14], x = labs[3]) + 
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

bc_co2 <- ggplot(filter(co2_seq, Path == "C4" & Status == "Acclimated" &
                           measure == "jmax_std" | measure == "vpmax_std" |
                           measure == "vcmax_std"),
                  aes(cao, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[8:10]) +
  scale_color_manual(values=c("#473C8B", "#3288BD", "#D53E4F"), labels = labs[8:10]) +
  labs(y = labs[1], x = labs[4]) + 
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

bc_vpd <- ggplot(filter(vpd_seq, Path == "C4" & Status == "Acclimated" &
                           measure == "jmax_std" | measure == "vpmax_std" |
                           measure == "vcmax_std"),
                  aes(vpd, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[8:10]) +
  scale_color_manual(values=c("#473C8B", "#3288BD", "#D53E4F"), labels = labs[8:10]) +
  labs(y = labs[14], x = labs[5]) + 
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

ggarrange(bc_temp, bc_light, bc_co2, bc_vpd, ncol = 2, nrow = 2,
          common.legend = TRUE, legend="bottom", labels = c("A", "B", "C", "D"),
          hjust = c(0, 0.25, 0, 0.25))
ggsave("biochem.png", width = width, height = width, units = units)


#### Ratios ####
rat_temp <- ggplot(filter(temp_seq, Path == "C4" & Status == "Acclimated" &
                             measure == "jvc_ratio_std" | measure == "jvp_ratio_std" |
                             measure == "vcvp_ratio_std"),
                    aes(tg_c, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[11:13]) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"), labels = labs[11:13]) +
  labs(y = labs[1], x = labs[2]) + 
  coord_cartesian(ylim = c(0.25, 3.75)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

rat_light <- ggplot(filter(par_seq, Path == "C4" & Status == "Acclimated" &
                            measure == "jvc_ratio_std" | measure == "jvp_ratio_std" |
                            measure == "vcvp_ratio_std"),
                   aes(par, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[11:13]) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"), labels = labs[11:13]) +
  labs(y = labs[1], x = labs[3]) + 
  coord_cartesian(ylim = c(0.25, 3.75)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

rat_co2 <- ggplot(filter(co2_seq, Path == "C4" & Status == "Acclimated" &
                            measure == "jvc_ratio_std" | measure == "jvp_ratio_std" |
                            measure == "vcvp_ratio_std"),
                   aes(cao, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[11:13]) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"), labels = labs[11:13]) +
  labs(y = labs[1], x = labs[4]) + 
  coord_cartesian(ylim = c(0.25, 3.75)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

rat_vpd <- ggplot(filter(vpd_seq, Path == "C4" & Status == "Acclimated" &
                            measure == "jvc_ratio_std" | measure == "jvp_ratio_std" |
                            measure == "vcvp_ratio_std"),
                   aes(vpd, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[11:13]) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"), labels = labs[11:13]) +
  labs(y = labs[1], x = labs[5]) + 
  coord_cartesian(ylim = c(0.25, 3.75)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

ggarrange(rat_temp, rat_light, rat_co2, rat_vpd, ncol = 2, nrow = 2,
          common.legend = TRUE, legend="bottom", labels = c("A", "B", "C", "D"),
          hjust = c(0, 0.25, 0, 0.25))
ggsave("ratios.png", width = width, height = width/2, units = units)

#### C4 (acclimated and unacclimated) vs C3 (acclimated) A (unscaled) #####
a_temp <- ggplot(filter(temp_seq, measure == "Ac" | measure == "A" | measure == "An"),
                  aes(tg_c, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[15:17]) +
  scale_color_manual(values=c("#CC6666", "#9999CC", "#66CC99"), labels = labs[15:17]) +
  labs(y = labs[18], x = labs[2]) + 
  coord_cartesian(ylim = c(0, 40)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

a_light <- ggplot(filter(par_seq, measure == "Ac" | measure == "A" | measure == "An"),
                 aes(par, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[15:17]) +
  scale_color_manual(values=c("#CC6666", "#9999CC", "#66CC99"), labels = labs[15:17]) +
  labs(y = labs[18], x = labs[3]) + 
  coord_cartesian(ylim = c(0, 40)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

a_co2 <- ggplot(filter(co2_seq, measure == "Ac" | measure == "A" | measure == "An"),
                  aes(cao, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[15:17]) +
  scale_color_manual(values=c("#CC6666", "#9999CC", "#66CC99"), labels = labs[15:17]) +
  labs(y = labs[18], x = labs[4]) + 
  coord_cartesian(ylim = c(0, 40)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

a_vpd <- ggplot(filter(vpd_seq, measure == "Ac" | measure == "A" | measure == "An"),
                  aes(vpd, value, group = measure)) + 
  geom_line(size = 2, aes(linetype = measure, color = measure)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels = labs[15:17]) +
  scale_color_manual(values=c("#CC6666", "#9999CC", "#66CC99"), labels = labs[15:17]) +
  labs(y = labs[18], x = labs[5]) + 
  coord_cartesian(ylim = c(0, 40)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(5, "line"),
        legend.text = element_text(size = 14))

ggarrange(a_temp, a_light, a_co2, a_vpd, ncol=2, nrow=2,
          common.legend = TRUE, legend="bottom", labels = c("A", "B", "C", "D"),
          hjust = c(0, 0.5, 0, 0.5))
ggsave("A.png", width = width, height = height, units = units) # Mismatch on purpsoe

#### Scatter Plot ####
# Lat/Lon for rasters
lon_lat_clim <- readMat("../Publication2020/Climate_Data/cru_pre_1901.mat")[[1]][,1:2]
colnames(lon_lat_clim) <- c("lon", "lat")
lon_lat <- as.data.frame(lon_lat_clim)

# Climate data for specified coordinates
clim_globe <- read.csv("..//Publication2020/avg-1960-2015-clim.csv") # Remove this when sharing

# MODIS Land Cover
# Make a key
MODIS_code <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 255)
Land_Cover_Type <- c("Evergreen Needleleaf Forests",
                     "Evergreen Broadleaf Forests",
                     "Deciduous Needleleaf Forests",
                     "Deciduous Broadleaf Forests",
                     "Mixed Forests",
                     "Closed Shrublands",
                     "Open Shrublands",
                     "Woody Savannas",
                     "Savannas",
                     "Grasslands",
                     "Permanent Wetlands",
                     "Croplands",
                     "Urban and Built-up Lands",
                     "Cropland/Natural Vegetation Mosaics",
                     "Permanent Snow and Ice",
                     "Barren",
                     "Water Bodies",
                     "Unclassified")
key <- data.frame(MODIS_code, Land_Cover_Type)
# Read in MODIS data
modis_2001 <- raster('../Publication2020/MODIS-data/LC_hd_global_2001.tif')
# Extract data for the lon/lat from the model
MODIS_code <- raster::extract(modis_2001, lon_lat)
clim_cover <- cbind(lon_lat_clim, MODIS_code)

# Run models
# Calculate Vcmax, Vpmax, A (for C4s)
res_c4s <- C4model(tg_c = clim_globe[, "tmp"],
                   z = clim_globe[, "z"],
                   vpdo = clim_globe[, "vpd"],
                   cao = clim_globe[, "co2"],
                   paro = clim_globe[, "par"])

# Calculate C3 A
res_c3s <- calc_optimal_vcmax(tg_c = clim_globe[, "tmp"],
                  z = clim_globe[, "z"],
                  vpdo = clim_globe[, "vpd"],
                  cao = clim_globe[, "co2"],
                  paro = clim_globe[, "par"],
                  theta = 0.85)

# Calculate DeltaA
deltaA <- ((res_c4s$Ac - (res_c3s$vcmax * res_c3s$mc)) / (res_c3s$vcmax * res_c3s$mc)) * 100
res_deltaA_plots <- cbind(clim_cover, deltaA)
deltaA_df <- as.data.frame(res_deltaA_plots)

# NASA percent composition data
# Read in data
data <- raster('../Publication2020/NASA Percent Data/data/c4_percent_1d.asc') # Remove when sharing
# Extract data
rasValue <- raster::extract(data, lon_lat)
# Bind it to the lat_lim values
res <- cbind(clim_cover, rasValue)
res_df <- as.data.frame(res)
res_df$calc_deltaA <- deltaA_df$deltaA
res_na <- na_if(res_df, -999)

# Merge in MODIS data word key, and filter by land type
res_final <- merge(res_na, key, by = "MODIS_code")

veg_cover <- res_final[res_final$Land_Cover_Type != "Barren", ]
grasslands <- veg_cover[veg_cover$Land_Cover_Type == "Grasslands" | 
                          veg_cover$Land_Cover_Type == "Savannas" |
                          veg_cover$Land_Cover_Type == "Open Shrublands" |
                          veg_cover$Land_Cover_Type == "Woody Savannas", ]

# Plot, with a single trendline for all biomes
plot <- ggplot(grasslands, aes(rasValue, calc_deltaA)) +
  geom_point(alpha = 0.3) +
  stat_smooth(method=lm, size = 2) +
  labs(y = expression(paste(Delta, A," (%)")),
       x = expression(paste('C'[4], " Vegetation (%)"))) + 
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  theme_bw()
# Save plot as png
ggsave("scatterplot.png", width = width, height = height, units = units)

# Numbers I call out in the paper
#####
# Vcmax response to VPD
vpd_seq_02 <-C4model(vpdo = c(0:8))
# Decrease in obs
(vpd_seq_02$obs[1] - vpd_seq_02$obs[9])/vpd_seq_02$obs[1]
# Decrease in chi_bs==
(vpd_seq_02$chi_bs[1] - vpd_seq_02$chi_bs[9])/vpd_seq_02$chi_bs[1]

# Jmax:Vpmax percent change
(filter(temp_seq, measure == "jvp_ratio_std")$value[1] - filter(temp_seq, measure == "jvp_ratio_std")$value[36])/filter(temp_seq, measure == "jvp_ratio_std")$value[1]
(filter(par_seq, measure == "jvp_ratio_std")$value[2] - filter(par_seq, measure == "jvp_ratio_std")$value[1001])/filter(par_seq, measure == "jvp_ratio_std")$value[1]

# Delta does indeed decrease with CO2
delta <- (filter(co2_seq, Path == "C4", measure == "Ac")$value - filter(co2_seq, Path == "C3", measure == "A")$value)/filter(co2_seq, Path == "C3", measure == "A")$value
plot(delta)
# And delta does indeed increase with temperature
delta <- (filter(temp_seq, Path == "C4", measure == "Ac")$value - filter(temp_seq, Path == "C3", measure == "A")$value)/filter(temp_seq, Path == "C3", measure == "A")$value
plot(delta)
# BUT delta DECREASES with VPD
delta <- (filter(vpd_seq, Path == "C4", measure == "Ac")$value - filter(vpd_seq, Path == "C3", measure == "A")$value)/filter(vpd_seq, Path == "C3", measure == "A")$value
plot(delta)

# Delta numbers
current_co2 <- C4model(tg_c = c(1:40))
current_co2_c3 <- calc_optimal_vcmax(tg_c = c(1:40))
current_co2_c3$A <- current_co2_c3$vcmax * current_co2_c3$mc
current_delta <- (current_co2$Ac - current_co2_c3$A)/current_co2_c3$A

high_co2 <- C4model(tg_c = c(1:40), cao = 1000)
high_co2_c3 <- calc_optimal_vcmax(tg_c = c(1:40), cao = 1000)
high_co2_c3$A <- high_co2_c3$vcmax * high_co2_c3$mc
high_delta <- (high_co2$Ac - high_co2_c3$A)/high_co2_c3$A

current_delta[15]

# Call out percent difference between chi_m and chi_bs
res <- C4model()
res$chi_bs/res$chi_m * 100

