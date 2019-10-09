rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
data <- read.table("FOX_PAPER_Complete_database.txt", h = T, sep = "\t", dec = ".") # ***Climatic variables per 4hours observation***
summary(data)

# ****** #
#data <- data[!data$YEAR %in% 2004:2005,]

library("viridis") # For colors in plot
library("lme4") # For generalised linear models
library("glmmTMB")
library("optimx")
library("visreg") # Vizualisation of model effects
library("DHARMa") # For simulations
library("AICcmodavg") # For AIC comparison
library("car") # For the Anova command
library("multcomp") # For the contrast analysis
library("emmeans") # For the contrast analysis
library("modEvA") # For the variance explained
library("scales") # For the colour transparency
library("ggplot2")
library("GGally") # correlation panels
library("mgcv")

# ------------------------------- #
#### Creation of the database ####
# ------------------------------#

# Keeping the observation more or equal than 3 min (180 s)
data <- data[data$OBS.LENGTH >= 180,]

# Creation of the random variable fox.year
data$fox.year <- paste(data$FOX.ID, data$YEAR, sep = "-")
data$fox.year <- as.factor(data$fox.year)

# Creation of a lemming variable with only two levels
data$lmg.crash[data$lmg.year == "crash"] <- "crash"
data$lmg.crash[data$lmg.year %in% c("inter", "peak")] <- "noCrash"

# Creation of the variable for the offset
data$log.obs <- log(data$OBS.LENGTH)

# Log transformation of lemming abundance variable
data$log.lmgAbun <- log(data$lmg.abun)

# WARNING - data_test contains a modified variable for "lmg.year"
data_test <- data
data_test$lmg.year <- as.character(data_test$lmg.year)
data_test$lmg.year[data_test$lmg.abun < 1] <- "crash"

data_test$lmg.crash[data_test$lmg.year == "crash"] <- "crash"

data_test$lmg.year <- as.factor(data_test$lmg.year)
data_test$lmg.crash <- as.factor(data_test$lmg.crash)

summary(data_test)

# Scaled data
scaleData <- apply(data_test[,c(2, 14, 17, 21, 22, 24, 28)], MARGIN = 2, scale)

scaleData <- cbind(scaleData, data_test[, c(7, 8, 23, 25:27)])
summary(scaleData)

# ------------------------ #
### the best glmm model ###
# ---------------------- #
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2000000))

foxGlmm <- glmer(AD.atq.number ~ prec*lmg.crash + max.temp*lmg.crash + nest.dens + DATE*lmg.crash
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = scaleData)

summary(foxGlmm)

# Tests with DHARma package
sims <- simulateResiduals(foxGlmm)
plot(sims)
testDispersion(sims)
testZeroInflation(sims)

x11();
par(mfrow = c(2, 2))
visreg(foxGlmm, "max.temp", by = "lmg.crash", overlay = T)
visreg(foxGlmm, "prec", by = "lmg.crash", overlay = T)
visreg(foxGlmm, "nest.dens", by = "lmg.crash", overlay = T)
visreg(foxGlmm, "DATE", by = "lmg.crash", overlay = T)
# -------------------------- #
### test with gamm models ###
# ------------------------ #
lmgGamm <- list()

lmgGamm[[1]] <- gam(AD.atq.number ~ s(max.temp, by = lmg.crash) + lmg.crash
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[1]])

plot(lmgGamm[[1]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[2]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + lmg.crash
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[2]])

plot(lmgGamm[[2]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[3]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[3]])

plot(lmgGamm[[3]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[4]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + DATE
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[4]])

plot(lmgGamm[[4]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[5]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + DATE + nest.dens
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[5]])

plot(lmgGamm[[5]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[6]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + s(DATE) + nest.dens
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[6]])

plot(lmgGamm[[6]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[7]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + s(DATE, by = lmg.crash) + nest.dens
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[7]])

x11(); plot(lmgGamm[[7]], page = 1, all.terms = TRUE, residuals = TRUE)
# Tests with DHARma package
sims <- simulateResiduals(lmgGamm[[7]])
x11();plot(sims)

# plot.gam(lmgGamm[[7]], select = 1)
x11(); par(mfrow = c(2, 2))
visreg(lmgGamm[[7]], "max.temp", "lmg.crash", overlay = T, scale = "response")
visreg(lmgGamm[[7]], "prec", "lmg.crash", overlay = T, scale = "response")
visreg(lmgGamm[[7]], "DATE", "lmg.crash", overlay = T, bty = "n", scale = "response")
visreg(lmgGamm[[7]], "nest.dens", "lmg.crash", overlay = T, bty = "n", scale = "response")

graphics.off()

### ---- ###
### AIC ###
### -- ###

AIC(lmgGamm[[1]], lmgGamm[[2]], lmgGamm[[3]], lmgGamm[[4]], lmgGamm[[5]], lmgGamm[[6]], lmgGamm[[7]])

### ------------------------------ ###
### k variation in the best model ###
### ---------------------------- ###

kMod <- list()
kMod[[1]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + s(DATE, by = lmg.crash) + nest.dens
                 + s(fox.year, bs = "re") 
                 + offset(log.obs),
                 family = poisson(),
                 method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                 #select = TRUE,
                 data = data_test)

kMod[[2]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash, k = 3) + lmg.crash + s(DATE, by = lmg.crash) + nest.dens
                 + s(fox.year, bs = "re") 
                 + offset(log.obs),
                 family = poisson(),
                 method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                 #select = TRUE,
                 data = data_test)
#gam.check(kMod[[2]])
x11()
plot(kMod[[2]], page = 1, all.terms = TRUE)
plot(kMod[[2]], select = 9, all.terms = TRUE)
summary(kMod[[2]])

x11(); par(mfrow = c(2, 2))
visreg(kMod[[2]], "prec", "lmg.crash", overlay = T)
# plot(j$fit$visregFit[j$fit$lmg.crash == "crash"], type = "l")
# lines(j$fit$visregFit[j$fit$lmg.crash == "noCrash"])
# points(j$res$visregRes)
visreg(kMod[[2]], "max.temp", "lmg.crash", overlay = T)
visreg(kMod[[2]], "DATE", "lmg.crash", overlay = T)
visreg(kMod[[2]], "nest.dens", "lmg.crash", overlay = T)

kMod[[3]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash, k = 3) + lmg.crash + s(DATE, by = lmg.crash, k = 3) + nest.dens
                 + s(fox.year, bs = "re") 
                 + offset(log.obs),
                 family = poisson(),
                 method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                 #select = TRUE,
                 data = data_test)
#gam.check(kMod[[2]])
#x11()
plot(kMod[[3]], page = 1, all.terms = TRUE)

kMod[[4]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash, k = 3) + s(max.temp, by = lmg.crash, k = 3) + lmg.crash + s(DATE, by = lmg.crash) + nest.dens
                 + s(fox.year, bs = "re") 
                 + offset(log.obs),
                 family = poisson(),
                 method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                 #select = TRUE,
                 data = data_test)

x11()
plot(kMod[[4]], page = 1, all.terms = TRUE)

AIC(kMod[[1]], kMod[[2]], kMod[[3]], kMod[[4]])

### ---------------------------------------------- ###
### Predictions for the best model - lmgGamm[[7]] ###
### -------------------------------------------- ###
graphics.off()

# In order to obtain correct confindent intervals
# 1 - Computation predictions without type = response cause SEs are not back transformed in this case
# 2 - Computation of min & max values of CI
# 3 - Back transformation of obtained values

range(data_test$max.temp) # 0.595 - 15.351

v <- seq(0, 16, by = 0.01)
newdat.crash <- data.frame(max.temp = v,
                           prec = mean(data_test$prec),
                           nest.dens = mean(data_test$nest.dens),
                           DATE = mean(data_test$DATE),
                           lmg.crash = "crash",
                           log.obs = mean(data_test$log.obs),
                           fox.year = data_test$fox.year[1])
newdat.noCrash <- data.frame(max.temp = v,
                             prec = mean(data_test$prec),
                             nest.dens = mean(data_test$nest.dens),
                             DATE = mean(data_test$DATE),
                             lmg.crash = "noCrash",
                             log.obs = mean(data_test$log.obs),
                             fox.year = data_test$fox.year[1])

p.crash <- predict(lmgGamm[[7]], newdata = newdat.crash, type = "link", re.form = NA, se.fit = TRUE)
p.noCrash <- predict(lmgGamm[[7]], newdata = newdat.noCrash, se.fit = TRUE, type = "link", re.form = NA)

#######################

temp_newData <- data.frame(max.temp = rep(v, 2),
                       prec = mean(data_test$prec),
                       nest.dens = mean(data_test$nest.dens),
                       DATE = mean(data_test$DATE),
                       lmg.crash = c(rep("crash", 1601), rep("noCrash", 1601)),
                       log.obs = mean(data_test$log.obs),
                       fox.year = data_test$fox.year[1])
temp_newData$tempPred <- predict(lmgGamm[[7]], newdata = temp_newData, type = "link", se.fit = TRUE)[[1]]
temp_newData$tempPred_se <- predict(lmgGamm[[7]], newdata = temp_newData, type = "link", se.fit = TRUE)[[2]]
temp_newData$transf_tempPred <- predict(lmgGamm[[7]], newdata = temp_newData, type = "response", se.fit = FALSE)
temp_newData$transf_tempPred_2 <- 10^temp_newData$tempPred 

# Confindent intervals

temp_newData$maxIC <- 10^(temp_newData$tempPred + 1.96*(temp_newData$tempPred_se))
temp_newData$minIC <- 10^(temp_newData$tempPred - 1.96*(temp_newData$tempPred_se))



# Atq number vs. prec #
# ---------------------- #
range(data_test$prec) # 0 - 20

v1 <- seq(0, 20, by = 0.01)
newdat1.crash <- data.frame(max.temp = mean(data_test$max.temp),
                            prec = v1,
                            nest.dens = mean(data_test$nest.dens),
                            lmg.crash = "crash",
                            DATE = mean(data_test$DATE),
                            log.obs = mean(data_test$log.obs),
                            fox.year = data_test$fox.year[1])

p1.crash <- predict(lmgGamm[[7]], newdata = newdat1.crash, se.fit = TRUE, type = "link", re.form = NA)

# ----- #
newdat1.noCrash <- data.frame(max.temp = mean(data_test$max.temp),
                              prec = v1,
                              nest.dens = mean(data_test$nest.dens),
                              lmg.crash = "noCrash",
                              DATE = mean(data_test$DATE),
                              log.obs = mean(data_test$log.obs),
                              fox.year = data_test$fox.year[1])

p1.noCrash <- predict(lmgGamm[[7]], newdata = newdat1.noCrash, se.fit = TRUE, type = "link", re.form = NA)

# Atq number vs. nest.dens #
# ----------------------- #
range(data_test$nest.dens) # 0.42 - 9.26

v2 <- seq(0, 10, by = 0.01)
newdat2.noCrash <- data.frame(max.temp = mean(data_test$max.temp),
                      prec = mean(data_test$prec),
                      nest.dens = v2,
                      DATE = mean(data_test$DATE),
                      lmg.crash = "noCrash",
                      log.obs = mean(data$log.obs),
                      fox.year = data_test$fox.year[1])

p2.noCrash <- predict(lmgGamm[[7]], newdata = newdat2.noCrash, se.fit = TRUE, type = "link", re.form = NA)

newdat2.crash <- data.frame(max.temp = mean(data_test$max.temp),
                              prec = mean(data_test$prec),
                              nest.dens = v2,
                              DATE = mean(data_test$DATE),
                              lmg.crash = "crash",
                              log.obs = mean(data$log.obs),
                              fox.year = data_test$fox.year[1])

p2.crash <- predict(lmgGamm[[7]], newdata = newdat2.crash, se.fit = TRUE, type = "link", re.form = NA)

# Atq number vs. date #
# ------------------- #
range(data_test$DATE) # 159 - 205

v3 <- seq(159, 205, by = 1)
newdat3.noCrash <- data.frame(max.temp = mean(data_test$max.temp),
                      prec = mean(data_test$prec),
                      nest.dens = mean(data_test$nest.dens),
                      DATE = v3,
                      lmg.crash = "noCrash",
                      log.obs = mean(data$log.obs),
                      fox.year = data_test$fox.year[1])

p3.noCrash <- predict(lmgGamm[[7]], newdata = newdat3.noCrash, se.fit = TRUE, type = "link", re.form = NA)

newdat3.crash <- data.frame(max.temp = mean(data_test$max.temp),
                              prec = mean(data_test$prec),
                              nest.dens = mean(data_test$nest.dens),
                              DATE = v3,
                              lmg.crash = "crash",
                              log.obs = mean(data$log.obs),
                              fox.year = data_test$fox.year[1])

p3.crash <- predict(lmgGamm[[7]], newdata = newdat3.crash, se.fit = TRUE, type = "link", re.form = NA)


# --------- #
# GRAPHICS #
# ------- #

# -------------------- #
# Temperature effect...
# ------------------- #
x11()
par(mfrow = c(2, 2))

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_temp.tiff",
#     res=300,
#     width=25,
#     height= 15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

# ------ #
plot(v, 10^p.crash$fit, ylim = c(0, 10), type = "l", bty = "n", lwd = 2.5, xlab = "Maximal temperature (C)", ylab = "Fox attack number per hour", col = "darkorange4")
lines(v, 10^p.noCrash$fit, lwd = 2.5, col = "darkorange3")
legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("darkorange4", "darkorange3"), border = NA, bty = "n")

# Confindent intervalls
polygon(x = c(v, rev(v)),
        y = c(10^(p.crash$fit - 1.96*(p.crash$se.fit)), 10^(rev(p.crash$fit) + 1.96*(rev(p.crash$se.fit)))),
        col = alpha("darkorange4", 0.25),
        border = NA)

polygon(x = c(v, rev(v)),
        y = c(10^(p.noCrash$fit - 1.96*(p.noCrash$se.fit)), 10^(rev(p.noCrash$fit) + 1.96*(rev(p.noCrash$se.fit)))),
        col = alpha("darkorange3", 0.25),
        border = NA)


##########################
plot(v, temp_newData$transf_tempPred_2[temp_newData$lmg.crash == "crash"], ylim = c(0, 10), type = "l", bty = "n", lwd = 2.5, xlab = "Maximal temperature (C)", ylab = "Fox attack number per hour", col = "darkorange4")
lines(v, temp_newData$transf_tempPred_2[temp_newData$lmg.crash == "noCrash"], lwd = 2.5, col = "darkorange3")
legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("darkorange4", "darkorange3"), border = NA, bty = "n")

# Confindent intervalls
polygon(x = c(v, rev(v)),
        y = c(temp_newData$minIC[temp_newData$lmg.crash == "crash"], rev(temp_newData$maxIC[temp_newData$lmg.crash == "crash"])),
        col = alpha("darkorange4", 0.25),
        border = NA)

polygon(x = c(v, rev(v)),
        y = c(temp_newData$minIC[temp_newData$lmg.crash == "noCrash"], rev(temp_newData$maxIC[temp_newData$lmg.crash == "noCrash"])),
        col = alpha("darkorange3", 0.25),
        border = NA)
#dev.off()

# ------------------------ #
# Precipitation effects....#
# ------------------------ #
# x11()
# par(mfrow = c(1, 2))

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_prec.tiff",
#     res=300,
#     width=25,
#     height= 15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

# ----- #
plot(v1, p1.crash$fit, type = "l", ylim = c(-1, 2), lwd = 2, col = "skyblue4", bty = "n", ylab = "Fox attack number per hour", xlab = "Cumulative precipitation (mm)")

lines(v1, p1.noCrash$fit, lwd = 2, col = "skyblue3")

legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("skyblue4", "skyblue3"), border = NA, bty = "n")

# Confident intervals
polygon(x = c(v1, rev(v1)),
        y = c((p1.crash$fit - 1.96*(p1.crash$se.fit)), (rev(p1.crash$fit) + 1.96*(rev(p1.crash$se.fit)))),
        col = alpha("skyblue4", 0.25),
        border = NA)

polygon(x = c(v1, rev(v1)),
        y = c((p1.noCrash$fit - 1.96*(p1.noCrash$se.fit)), (rev(p1.noCrash$fit) + 1.96*(rev(p1.noCrash$se.fit)))),
        col = alpha("skyblue3", 0.25),
        border = NA)

#dev.off()

# --------------- #
# Date effect ...#
# ------------- #

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_date.tiff",
#     res=300,
#     width=25,
#     height= 15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

plot(v3, p3.crash$fit, ylim = c(-3, 9), type = "l", lwd = 2, bty = "n", xlab = "Date of observations (Julian date)", col = "plum4", ylab = "Fox attack number per hour")

lines(v3, p3.noCrash$fit, lwd = 2, col = "plum3")

legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("plum4", "plum3"), border = NA, bty = "n")

# Confident intervals
polygon(x = c(v3, rev(v3)),
        y = c((p3.crash$fit - 1.96*(p3.crash$se.fit)), (rev(p3.crash$fit) + 1.96*(rev(p3.crash$se.fit)))),
        col = alpha("plum4", 0.25),
        border = NA)

polygon(x = c(v3, rev(v3)),
        y = c((p3.noCrash$fit - 1.96*(p3.noCrash$se.fit)), (rev(p3.noCrash$fit) + 1.96*(rev(p3.noCrash$se.fit)))),
        col = alpha("plum3", 0.25),
        border = NA)

#dev.off()

# ----------------------- #
# Nest density effect ...#
# --------------------- #
#x11()
 
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_nest_dens.tiff",
#     res=300,
#     width=25,
#     height= 15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

plot(v2, p2.crash$fit, ylim = c(-0.5, 2), type = "l", lwd = 2, bty = "n", xlab = "Goose nest density (nb/ha)", col = "forestgreen", ylab = "Fox attack number per hour")

lines(v2, p2.noCrash$fit, lwd = 2, col = "chartreuse3")

legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("forestgreen", "chartreuse3"), border = NA, bty = "n")

# Confident intervals
polygon(x = c(v2, rev(v2)),
        y = c((p2.crash$fit - 1.96*(p2.crash$se.fit)), (rev(p2.crash$fit) + 1.96*(rev(p2.crash$se.fit)))),
        col = alpha("forestgreen", 0.25),
        border = NA)

polygon(x = c(v2, rev(v2)),
        y = c((p2.noCrash$fit - 1.96*(p2.noCrash$se.fit)), (rev(p2.noCrash$fit) + 1.96*(rev(p2.noCrash$se.fit)))),
        col = alpha("chartreuse3", 0.25),
        border = NA)

#dev.off()


graphics.off()

