rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
data <- read.table("FOX_atq.per.obs_clim.txt", h = T, sep = "\t")
summary(data)

library(viridis) # For colors in plot
library(mgcv)
#### Data exploration ####

# Keeping the observation more or equal than 3 min (180 s)
data <- data[data$Obs_lenght >= 180,]

par(mfrow = c(1, 2))
boxplot(data$all_atq_rate)
boxplot(data$goo_atq_rate)

# MAX DAILY TEMP
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$Year))) # color choice in existing color palette (option A, B, C, or D)
plot(data$max.daily.temp, data$AD_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Daily maximum temperature (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")
plot(data$max.daily.temp, data$all_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "All predated items", xlab = "Daily maximum temperature (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")

# MIN DAILY TEMP
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$Year))) # color choice in existing color palette (option A, B, C, or D)
plot(data$min.daily.temp, data$AD_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Daily minimum temperature (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")
plot(data$min.daily.temp, data$all_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "All predated items", xlab = "Daily minimum temperature (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")

# PRECIPITATION
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$Year))) # color choice in existing color palette (option A, B, C, or D)
plot(data$rain, data$AD_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Daily precipitation (mm)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")
plot(data$rain, data$all_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "All predated items", xlab = "Daily precipitation (mm)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")

# wind speed
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$Year))) # color choice in existing color palette (option A, B, C, or D)
plot(data$max.daily.speed, data$AD_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Daily maximum wind speed", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")
plot(data$max.daily.speed, data$all_atq_rate, bty = "n", col = color[as.numeric(factor(data$Year))], pch = 19, cex = 1.5, main = "All predated items", xlab = "Daily maximum wind speed", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$Year)), col = color, pch = 19, bty = "n")


# Lemming abundance
color <- viridis_pal(option = "D")(length(unique(data$lmg.year)))
par(mfrow = c(1, 2))
plot(data$lmg, data$AD_atq_rate, bty = "n", col = color[as.numeric(data$lmg.year)], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Lemming abundance", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(data$lmg.year), col = color, pch = 19, bty = "n")
plot(data$lmg, data$all_atq_rate, bty = "n", col = color[as.numeric(data$lmg.year)], pch = 19, cex = 1.5, main = "All predated items", xlab = "Lemming abundance", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(data$lmg.year), col = color, pch = 19, bty = "n")


# Models

atq.mod <- list()

atq.mod[[1]] <- gam(AD_atq_rate ~ s(max.daily.temp),
                     data = data)
atq.mod[[2]] <- gam(AD_atq_rate ~ s(min.daily.temp),
                    data = data)
m1 <- gam(AD_atq_rate ~ s(max.daily.temp), data = data, method = "REML")
summary(m1)
coef(m1)
plot(atq.mod[[1]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[2]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
summary(atq.mod[[2]])
coef(atq.mod[[2]])
