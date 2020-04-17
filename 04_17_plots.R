
# Load packages
library(R.utils)
library(ggplot2)

# Source model code and functions
source("C4model.R")
source("pmodel.R")
sourceDirectory("functions/", modifiedOnly = FALSE)

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

# Plot chi
plot_temp <- ggplot(temp_seq, aes(tg_c, chi)) + geom_line()
plot_temp

plot_par <- ggplot(par_seq, aes(par, chi)) + geom_line()
plot_par

plot_vpd <- ggplot(vpd_seq, aes(vpd, chi)) + geom_line()
plot_vpd

plot_co2 <- ggplot(co2_seq, aes(ca, chi)) + geom_line()
plot_co2

# Plot A
plot_A_temp <- ggplot(temp_seq, aes(tg_c, Al)) + geom_line()
plot_A_temp

plot_A_par <- ggplot(par_seq, aes(par, Al)) + geom_line()
plot_A_par

plot_A_vpd <- ggplot(vpd_seq, aes(vpd, Al)) + geom_line()
plot_A_vpd

plot_A_co2 <- ggplot(co2_seq, aes(ca, Al)) + geom_line()
plot_A_co2

# Plot Vcmax
plot_v_temp <- ggplot(temp_seq, aes(tg_c, vcmax)) + geom_line()
plot_v_temp

plot_v_par <- ggplot(par_seq, aes(par, vcmax)) + geom_line()
plot_v_par

plot_v_vpd <- ggplot(vpd_seq, aes(vpd, vcmax)) + geom_line()
plot_v_vpd

plot_v_co2 <- ggplot(co2_seq, aes(ca, vcmax)) + geom_line()
plot_v_co2

# Plot Vpmax
plot_vp_temp <- ggplot(temp_seq, aes(tg_c, vpmax)) + geom_line()
plot_vp_temp

plot_vp_par <- ggplot(par_seq, aes(par, vpmax)) + geom_line()
plot_vp_par

plot_vp_vpd <- ggplot(vpd_seq, aes(vpd, vpmax)) + geom_line()
plot_vp_vpd

plot_vp_co2 <- ggplot(co2_seq, aes(ca, vpmax)) + geom_line()
plot_vp_co2

# plot cbs
plot_c_temp <- ggplot(temp_seq, aes(tg_c, cbs)) + geom_line()
plot_c_temp

plot_c_par <- ggplot(par_seq, aes(par, cbs)) + geom_line()
plot_c_par

plot_c_vpd <- ggplot(vpd_seq, aes(vpd, cbs)) + geom_line()
plot_c_vpd

plot_c_co2 <- ggplot(co2_seq, aes(ca, cbs)) + geom_line()
plot_c_co2

# Plot obs
plot_o_temp <- ggplot(temp_seq, aes(tg_c, obs)) + geom_line()
plot_o_temp

plot_o_par <- ggplot(par_seq, aes(par, obs)) + geom_line()
plot_o_par

plot_o_vpd <- ggplot(vpd_seq, aes(vpd, obs)) + geom_line()
plot_o_vpd

plot_o_co2 <- ggplot(co2_seq, aes(ca, obs)) + geom_line()
plot_o_co2

# C3 comparison plots
c4c3_temp <- ggplot(temp_seq, aes(tg_c, Al)) + geom_line(color = "red") + geom_line(data = c3_temp_seq, aes(x = tg_c, y = A))
c4c3_temp

c4c3_par <- ggplot(par_seq, aes(par, Al)) + geom_line(color = "red") + geom_line(data = c3_par_seq, aes(x = par, y = A))
c4c3_par

c4c3_vpd <- ggplot(vpd_seq, aes(vpd, Al)) + geom_line(color = "red") + geom_line(data = c3_vpd_seq, aes(x = vpd, y = A))
c4c3_vpd

c4c3_co2 <- ggplot(co2_seq, aes(ca, Al)) + geom_line(color = "red") + geom_line(data = c3_co2_seq, aes(x = ca, y = A))
c4c3_co2