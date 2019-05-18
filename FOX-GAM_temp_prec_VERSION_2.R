rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
data <- read.table("FOX_atq.per.obs_clim.txt", h = T, sep = "\t")
summary(data)

library(viridis) # For colors in plot
library(mgcv)
library(DHARMa)
#### Data exploration ####

# Keeping the observation more or equal than 3 min (180 s)
data <- data[data$Obs_lenght >= 180,]


par(mfrow = c(1, 2))
boxplot(data$all_atq_rate, bty = "n", main = "Attack rate for all predated items")
boxplot(data$goo_atq_rate, bty = "n", main = "Attack rate for nests with adults")

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

data$lmg.year <- as.factor(as.numeric(data$lmg.year)) # 1 = crash, 2 = inter, 3 = peak
# Models

atq.mod <- list()

atq.mod[[1]] <- gam(AD_atq_rate ~ s(max.daily.temp),
                    method = "REML",
                    data = data)
atq.mod[[2]] <- gam(AD_atq_rate ~ s(min.daily.temp),
                    method = "REML",
                    data = data)
atq.mod[[3]] <- gam(AD_atq_rate ~ s(rain),
                    method = "REML",
                    data = data)
atq.mod[[4]] <- gam(AD_atq_rate ~ s(rain) + lmg.year,
                    method = "REML",
                    data = data)
atq.mod[[5]] <- gam(AD_atq_rate ~ s(rain) + s(max.daily.temp),
                    method = "REML",
                    data = data)
atq.mod[[6]] <- gam(AD_atq_rate ~ s(rain, by = lmg.year) + s(max.daily.temp, by = lmg.year) + lmg.year,
                    method = "REML",
                    data = data)
atq.mod[[7]] <- gam(AD_atq_rate ~ s(rain) + s(max.daily.temp) + s(lmg, k = 5),
                    method = "REML",
                    data = data)
summary(atq.mod[[7]])
plot(atq.mod[[7]], pages = 1)

m <- gam(AD_atq_rate ~ s(rain) + s(max.daily.temp) + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re"),
       method = "REML",
       #select = TRUE,
       data = data)
summary(m)
x11()
plot(m, page = 1)
gam.check(m)

m.test <- gam(AD_atq ~ s(rain) + s(max.daily.temp) + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + offset(log(Obs_lenght)),
              data = data,
              select = TRUE,
              family = poisson)
summary(m.test)
plot(m.test, page = 1)


plot(simulateResiduals(m))

concurvity(m, full = TRUE)

m1 <- gam(lmg ~ s(nest.density, k = 5),
          data = data)
summary(m1)
plot(m1, se = TRUE, residuals = TRUE, pch = 1, cex = 1, bty = "n", shade = TRUE)



par(mfrow=c(2,2))
visreg(m)
plot(model.frame(m))


par(mfrow = c(1, 2))
plot(atq.mod[[1]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[1]], bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[2]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[2]],bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[3]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[3]],bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[5]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[6]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))

coef(atq.mod[[5]])
summary(atq.mod[[5]])
gam.check(atq.mod[[5]])

atq.mod[[5]] <- gam(AD_atq_rate ~ s(rain, k = 5) + s(max.daily.temp, k= 5),
                    method = "REML",
                    data = data)
gam.check(atq.mod[[5]])



AIC(atq.mod[[1]], atq.mod[[2]], atq.mod[[3]], atq.mod[[4]], atq.mod[[5]])

gam.check(atq.mod[[6]])

plot(simulateResiduals(atq.mod[[6]]))
