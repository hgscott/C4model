# Figure out why vcmax is so crazy high

# Load packages
library(R.utils)
library(ggplot2)

# Source model code and functions
source("C4model.R")
sourceDirectory("functions/", modifiedOnly = FALSE)

# Run model across all the variables
temp_seq <- C4model(tg_c = c(1:40))
par_seq <- C4model(paro = c(0:1000))
vpd_seq  <- C4model(vpdo = c(0:8))
co2_seq <- C4model(cao = c(5:1000))

# Chi seems low
# Also looks like it's not changing in any of these
plot_temp <- ggplot(temp_seq, aes(tg_c, chi)) + geom_line()
plot_temp

plot_par <- ggplot(par_seq, aes(par, chi)) + geom_line()
plot_par

plot_vpd <- ggplot(vpd_seq, aes(vpd, chi)) + geom_line()
plot_vpd

plot_co2 <- ggplot(co2_seq, aes(ca, chi)) + geom_line()
plot_co2