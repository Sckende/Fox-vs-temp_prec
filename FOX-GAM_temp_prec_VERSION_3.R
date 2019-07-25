rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
data <- read.table("FOX_PAPER_Complete_database.txt", h = T, sep = "\t", dec = ".") # ***Climatic variables per 4hours observation***
summary(data)

library(viridis) # For colors in plot
library(mgcv)
library(DHARMa)
#### Data exploration ####
data$YEAR <- as.factor(data$YEAR)

# Modification of the lmg.year variable
data$lmg.year <- as.character(data$lmg.year)
data$lmg.year[data$lmg.year %in% c("peak", "inter")] <- "noCrash"
data$lmg.year <- as.factor(data$lmg.year)

# Keeping the observation more or equal than 3 min (180 s)
data <- data[data$OBS.LENGTH >= 180,]

x11()
par(mfrow = c(1, 2))
boxplot(data$goo.atq.rate, bty = "n", main = "Attack rate for all predated goose items")
boxplot(data$AD.atq.rate, bty = "n", main = "Attack rate for nests with adults")

# MAX TEMP
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$YEAR))) # color choice in existing color palette (option A, B, C, or D)
plot(data$max.temp, data$AD.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Maximum temperature during obs. bloc (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")
plot(data$max.temp, data$goo.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "All predated goose items", xlab = "Maximum temperature during obs. bloc (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")

# MIN TEMP
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$YEAR))) # color choice in existing color palette (option A, B, C, or D)
plot(data$mini.temp, data$AD.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Minimum temperature during obs. bloc (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")
plot(data$mini.temp, data$goo.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "All predated goose items", xlab = "Minimum temperature during obs. bloc (C)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")

# PRECIPITATION
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$YEAR))) # color choice in existing color palette (option A, B, C, or D)
plot(data$prec, data$AD.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Daily precipitation (mm)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")
plot(data$prec, data$goo.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "All predated goose items", xlab = "Daily precipitation (mm)", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")

# Maximum wind speed
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$YEAR))) # color choice in existing color palette (option A, B, C, or D)
plot(data$max.wind, data$AD.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Maximum wind speed during obs. bloc", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")
plot(data$max.wind, data$goo.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "All predated goose items", xlab = "Maximum wind speed during obs. bloc", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")

# Mean wind speed
par(mfrow = c(1, 2))
color <- viridis_pal(option = "D")(length(unique(data$YEAR))) # color choice in existing color palette (option A, B, C, or D)
plot(data$mean.wind, data$AD.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Maximum wind speed during obs. bloc", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")
plot(data$mean.wind, data$goo.atq.rate, bty = "n", col = color[as.numeric(factor(data$YEAR))], pch = 19, cex = 1.5, main = "All predated goose items", xlab = "Maximum wind speed during obs. bloc", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(as.factor(data$YEAR)), col = color, pch = 19, bty = "n")


# Lemming abundance
x11()
color <- viridis_pal(option = "D")(length(unique(data$lmg.year)))
par(mfrow = c(1, 2))
plot(data$lmg.abun, data$AD.atq.rate, bty = "n", col = color[as.numeric(data$lmg.year)], pch = 19, cex = 1.5, main = "Nests with adults", xlab = "Lemming abundance", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(data$lmg.year), col = color, pch = 19, bty = "n")
plot(data$lmg.abun, data$goo.atq.rate, bty = "n", col = color[as.numeric(data$lmg.year)], pch = 19, cex = 1.5, main = "All predated goose items", xlab = "Lemming abundance", ylab = "Attack rate per ind. per obs.")
legend("topright", legend = levels(data$lmg.year), col = color, pch = 19, bty = "n")

data$lmg.year <- as.factor(as.numeric(data$lmg.year)) # 1 = crash, 2 = noCrash


#### Zero-inflated Poisson family in GAM-M ####
#data <- data[!data$YEAR %in% 2004:2005,]
# Full model
zip.1 <- gam(AD.atq.number ~ s(prec) + max.temp + s(max.wind)+ s(YEAR, bs = "re") + offset(log(OBS.LENGTH)),
             family = ziP(), # Have to understand more details to use the ziplss() family
             method = "REML",
             #select = TRUE,
             data = data)
summary(zip.1)
x11(); plot(zip.1, page = 1, all.terms = TRUE)
plot(simulateResiduals(zip.1))
x11();visreg:::visreg(zip.1, "max.temp", by = "YEAR", scale = "linear", overlay = TRUE, band = FALSE)
plot(predict(zip.1, type = "response"))
par(mfrow = c(1, 2))
hist(data$AD.atq.number, breaks = 0:50)
hist(predict(zip.1, type = "response"), breaks = 0:50)

# Models compairison
zip <- list()
zip[[1]] <- gam(AD.atq.number ~ s(prec) + s(max.temp) + s(max.wind) + s(lmg.abun, k = 5) + s(nest.dens, k = 5) + s(YEAR, bs = "re") + offset(log(OBS.LENGTH)),
                family = ziP(), # Have to understand more details to use the ziplss() family
                method = "REML",
                #select = TRUE,
                data = data)
zip[[2]] <- gam(AD.atq.number ~ s(max.temp) + s(max.wind) + s(lmg.abun, k = 5) + s(nest.dens, k = 5) + s(YEAR, bs = "re") + offset(log(OBS.LENGTH)),
                family = ziP(), # Have to understand more details to use the ziplss() family
                method = "REML",
                #select = TRUE,
                data = data)
zip[[3]] <- gam(AD.atq.number ~ s(prec) + s(max.temp) + s(lmg.abun, k = 5) + s(nest.dens, k = 5) + s(YEAR, bs = "re") + offset(log(OBS.LENGTH)),
                family = ziP(), # Have to understand more details to use the ziplss() family
                method = "REML",
                #select = TRUE,
                data = data)
zip[[4]] <- gam(AD.atq.number ~ s(max.temp) + s(lmg.abun, k = 5) + s(nest.dens, k = 5) + s(YEAR, bs = "re") + offset(log(OBS.LENGTH)),
                family = ziP(), # Have to understand more details to use the ziplss() family
                method = "REML",
                #select = TRUE,
                data = data)
zip[[5]] <- gam(AD.atq.number ~ s(max.temp) + s(YEAR, bs = "re") + offset(log(OBS.LENGTH)),
                family = ziP(), # Have to understand more details to use the ziplss() family
                method = "REML",
                #select = TRUE,
                data = data)
zip[[6]] <- gam(AD.atq.number ~ s(max.temp, by = lmg.year) +  offset(log(OBS.LENGTH)),
                family = ziP(), # Have to understand more details to use the ziplss() family
                method = "REML",
                #select = TRUE,
                data = data)
zip[[7]] <- gam(AD.atq.number ~ s(prec, by = lmg.year) + s(max.temp, by = lmg.year) + s(max.wind, by = lmg.year) + s(nest.dens, k = 5) + s(YEAR, bs = "re") + offset(log(OBS.LENGTH)),
                family = ziP(), # Have to understand more details to use the ziplss() family
                method = "REML",
                #select = TRUE,
                data = data)
AIC(zip[[1]], zip[[2]], zip[[3]], zip[[4]], zip[[5]], zip[[6]], zip[[7]])
summary(zip[[7]])

#### *** HERE I AM *** ####
# Best model compairison with or without smooth

bmod <- list()
bmod[[1]] <- gam(AD.atq.number ~ s(prec) + s(max.daily.temp) + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re") + offset(log(OBS.LENGTH)),
                 family = ziP(), # Have to understand more details to use the ziplss() family
                 method = "REML",
                 #select = TRUE,
                 data = data)
bmod[[2]] <- gam(AD.atq.number ~ s(prec) + max.daily.temp + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re") + offset(log(OBS.LENGTH)),
                 family = ziP(), # Have to understand more details to use the ziplss() family
                 method = "REML",
                 #select = TRUE,
                 data = data)
bmod[[3]] <- gam(AD.atq.number ~ prec + max.daily.temp + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re") + offset(log(OBS.LENGTH)),
                 family = ziP(), # Have to understand more details to use the ziplss() family
                 method = "REML",
                 #select = TRUE,
                 data = data)

bmod[[4]] <- gam(AD.atq.number ~ prec + max.daily.temp + max.daily.speed + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re") + offset(log(OBS.LENGTH)),
                 family = ziP(), # Have to understand more details to use the ziplss() family
                 method = "REML",
                 #select = TRUE,
                 data = data)

AIC(bmod[[1]], bmod[[2]], bmod[[3]], bmod[[4]])

# Best model and variation of k value

m <- gam(AD_atq ~ s(rain) + s(max.daily.temp, k = 2) + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re") + offset(log(Obs_lenght)),
         family = ziP(), # Have to understand more details to use the ziplss() family
         method = "REML",
         #select = TRUE,
         data = data)
summary(m)
x11(); plot(m, page = 1, all.terms = TRUE)

m.1 <- gam(AD_atq ~ s(rain) + s(max.daily.temp, k = 4) + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re") + offset(log(Obs_lenght)),
           family = ziP(), # Have to understand more details to use the ziplss() family
           method = "REML",
           #select = TRUE,
           data = data)
summary(m.1)
x11(); plot(m.1, page = 1, all.terms = TRUE)

m.2 <- gam(AD_atq ~ s(rain) + max.daily.temp + s(max.daily.speed) + s(lmg, k = 5) + s(nest.density, k = 5) + s(Year, bs = "re") + offset(log(Obs_lenght)),
           family = ziP(), # Have to understand more details to use the ziplss() family
           method = "REML",
           #select = TRUE,
           data = data)
summary(m.2)
x11(); plot(m.2, page = 1, all.terms = TRUE, rug = T, residuals = T)

m.3 <- gam(AD_atq ~ s(rain) + max.daily.temp + s(max.daily.speed) + s(Year, bs = "re") + offset(log(Obs_lenght)),
           family = ziP(), # Have to understand more details to use the ziplss() family
           method = "REML",
           #select = TRUE,
           data = data)
summary(m.3)
summary(m.3)$s.table
x11(); plot(m.3, page = 1, all.terms = TRUE, rug = T, residuals = T)
plot(m.3, select = 2)

# Try to plot depending on level of random plot
x11()
par(mfrow=c(1,2), cex=1.1)
itsadug:::plot_smooth(m.3, view="rain", rm.ranef=TRUE, main="intercept + s(x1)", rug=FALSE)
plot_smooth(m.3, view="rain", cond=list(Year="1"), 
            main="... + s(fac)", col='orange', rug=FALSE)
plot_smooth(gamm_intercept, view="rain", cond=list(Year="2"), add=TRUE, col='red')
plot_smooth(gamm_intercept, view="rain", cond=list(Year="3"), add=TRUE, col='purple')
plot_smooth(gamm_intercept, view="rain", cond=list(Year="4"), add=TRUE, col='turquoise')



# shape constrained gam