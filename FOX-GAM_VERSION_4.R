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

newD1 <- data.frame(max.temp = rep(v, 2),
                       prec = mean(data_test$prec),
                       nest.dens = mean(data_test$nest.dens),
                       DATE = mean(data_test$DATE),
                       lmg.crash = c(rep("crash", 1601), rep("noCrash", 1601)),
                       log.obs = mean(data_test$log.obs),
                       fox.year = "Big one-2004")

newD1$fit <- predict(lmgGamm[[7]], newdata = newD1, type = "link", se.fit = TRUE)[[1]]
newD1$se.fit <- predict(lmgGamm[[7]], newdata = newD1, type = "link", se.fit = TRUE)[[2]]
newD1$tranFit <- predict(lmgGamm[[7]], newdata = newD1, type = "response", se.fit = FALSE)
#newD1$tranFit2 <- 10^newD1$fit 
#newD1$tranFit3 <- exp(newD1$fit)

# Confindent intervals

newD1$maxIC <- exp(newD1$fit + 1.96*(newD1$se.fit))
newD1$minIC <- exp(newD1$fit - 1.96*(newD1$se.fit))



# Atq number vs. prec #
# ---------------------- #
range(data_test$prec) # 0 - 20

v1 <- seq(0, 20, by = 0.01)

newD2 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = rep(v1, 2),
                    nest.dens = mean(data_test$nest.dens),
                    DATE = mean(data_test$DATE),
                    lmg.crash = c(rep("crash", 2001), rep("noCrash", 2001)),
                    log.obs = mean(data_test$log.obs),
                    fox.year = "Big one-2004")

newD2$fit <- predict(lmgGamm[[7]], newdata = newD2, type = "link", se.fit = TRUE)[[1]]
newD2$se.fit <- predict(lmgGamm[[7]], newdata = newD2, type = "link", se.fit = TRUE)[[2]]
newD2$tranFit <- predict(lmgGamm[[7]], newdata = newD2, type = "response", se.fit = FALSE)
#newD2$tranFit2 <- 10^newD2$fit 
#newD2$tranFit3 <- exp(newD2$fit)

# Confindent intervals

newD2$maxIC <- exp(newD2$fit + 1.96*(newD2$se.fit))
newD2$minIC <- exp(newD2$fit - 1.96*(newD2$se.fit))

# Atq number vs. date #
# ------------------- #
range(data_test$DATE) # 159 - 205

v3 <- seq(159, 205, by = 1)

newD3 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = mean(data_test$nest.dens),
                    DATE = rep(v3, 2),
                    lmg.crash = c(rep("crash", 47), rep("noCrash", 47)),
                    log.obs = mean(data_test$log.obs),
                    fox.year = "Big one-2004")

newD3$fit <- predict(lmgGamm[[7]], newdata = newD3, type = "link", se.fit = TRUE)[[1]]
newD3$se.fit <- predict(lmgGamm[[7]], newdata = newD3, type = "link", se.fit = TRUE)[[2]]
newD3$tranFit <- predict(lmgGamm[[7]], newdata = newD3, type = "response", se.fit = FALSE)
#newD3$tranFit2 <- 10^newD3$fit 
#newD3$tranFit3 <- exp(newD3$fit)

# Confindent intervals

newD3$maxIC <- exp(newD3$fit + 1.96*(newD3$se.fit))
newD3$minIC <- exp(newD3$fit - 1.96*(newD3$se.fit))


# Atq number vs. nest.dens #
# ----------------------- #
range(data_test$nest.dens) # 0.42 - 9.26

v2 <- seq(0, 10, by = 0.01)

newD4 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = rep(v2, 2),
                    DATE = mean(data_test$DATE),
                    lmg.crash = c(rep("crash", 1001), rep("noCrash", 1001)),
                    log.obs = mean(data_test$log.obs),
                    fox.year = "Big one-2004")

newD4$fit <- predict(lmgGamm[[7]], newdata = newD4, type = "link", se.fit = TRUE)[[1]]
newD4$se.fit <- predict(lmgGamm[[7]], newdata = newD4, type = "link", se.fit = TRUE)[[2]]
newD4$tranFit <- predict(lmgGamm[[7]], newdata = newD4, type = "response", se.fit = FALSE)
#newD4$tranFit2 <- 10^newD4$fit 
#newD4$tranFit3 <- exp(newD4$fit)

# Confindent intervals

newD4$maxIC <- exp(newD4$fit + 1.96*(newD4$se.fit))
#newD4$maxIC2 <- lmgGamm[[7]]$family$linkinv(newD4$fit + 1.96*(newD4$se.fit))
newD4$minIC <- exp(newD4$fit - 1.96*(newD4$se.fit))
                   
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

plot(v,
     newD1$tranFit[newD1$lmg.crash == "crash"],
     ylim = c(0, 2),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Maximal temperature (C)",
     ylab = "Fox attack number per hour",
     col = "darkorange4")
lines(v,
      newD1$tranFit[newD1$lmg.crash == "noCrash"],
      lwd = 2.5,
      col = "darkorange3")
legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("darkorange4", "darkorange3"), border = NA, bty = "n")

# Confindent intervalls
polygon(x = c(v, rev(v)),
        y = c(newD1$minIC[newD1$lmg.crash == "crash"], rev(newD1$maxIC[newD1$lmg.crash == "crash"])),
        col = alpha("darkorange4", 0.25),
        border = NA)

polygon(x = c(v, rev(v)),
        y = c(newD1$minIC[newD1$lmg.crash == "noCrash"], rev(newD1$maxIC[newD1$lmg.crash == "noCrash"])),
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
plot(v1,
     newD2$tranFit[newD2$lmg.crash == "crash"],
     ylim = c(0, 2.5),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Cumulative precipitation (mm)",
     ylab = "Fox attack number per hour",
     col = "skyblue4")
lines(v1,
      newD2$tranFit[newD2$lmg.crash == "noCrash"],
      lwd = 2.5,
      col = "skyblue3")
legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("skyblue4", "skyblue3"), border = NA, bty = "n")

# Confindent intervalls
polygon(x = c(v1, rev(v1)),
        y = c(newD2$minIC[newD2$lmg.crash == "crash"], rev(newD2$maxIC[newD2$lmg.crash == "crash"])),
        col = alpha("skyblue4", 0.25),
        border = NA)

polygon(x = c(v1, rev(v1)),
        y = c(newD2$minIC[newD2$lmg.crash == "noCrash"], rev(newD2$maxIC[newD2$lmg.crash == "noCrash"])),
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

plot(v3,
     newD3$tranFit[newD3$lmg.crash == "crash"],
     ylim = c(0, 12),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Date (julian date)",
     ylab = "Fox attack number per hour",
     col = "plum4")
lines(v3,
      newD3$tranFit[newD3$lmg.crash == "noCrash"],
      lwd = 2.5,
      col = "plum3")
legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("plum4", "plum3"), border = NA, bty = "n")

# Confindent intervalls
polygon(x = c(v3, rev(v3)),
        y = c(newD3$minIC[newD3$lmg.crash == "crash"], rev(newD3$maxIC[newD3$lmg.crash == "crash"])),
        col = alpha("plum4", 0.25),
        border = NA)

polygon(x = c(v3, rev(v3)),
        y = c(newD3$minIC[newD3$lmg.crash == "noCrash"], rev(newD3$maxIC[newD3$lmg.crash == "noCrash"])),
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

plot(v2,
     newD4$tranFit[newD4$lmg.crash == "crash"],
     ylim = c(0, 2.5),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Goose nest density (nb/ha)",
     ylab = "Fox attack number per hour",
     col = "forestgreen")
lines(v2,
      newD4$tranFit[newD4$lmg.crash == "noCrash"],
      lwd = 2.5,
      col = "chartreuse3")
legend("topright", legend = c("Lemming crash", "No lemming crash"), fill = c("forestgreen", "chartreuse3"), border = NA, bty = "n")

# Confidence intervals
polygon(x = c(v2, rev(v2)),
        y = c(newD4$minIC[newD4$lmg.crash == "crash"], rev(newD4$maxIC[newD4$lmg.crash == "crash"])),
        col = alpha("forestgreen", 0.25),
        border = NA)

polygon(x = c(v2, rev(v2)),
        y = c(newD4$minIC[newD4$lmg.crash == "noCrash"], rev(newD4$maxIC[newD4$lmg.crash == "noCrash"])),
        col = alpha("chartreuse3", 0.25),
        border = NA)

#dev.off()


graphics.off()

# ------------------------------------------------ #
# Comparing smooths in factor-smooth interactions #
# ---------------------------------------------- #

# Method on https://www.fromthebottomoftheheap.net/2017/10/10/difference-splines-i/

summary(lmgGamm[[7]])

# Packages loading
library('readr')
library('dplyr')
library('ggplot2')
library('mgcv')

# Function


smooth_diff <- function(model, newdata, f1, f2, var, alpha = 0.05,
                        unconditional = FALSE) {
  xp <- predict(model, newdata = newdata, type = 'lpmatrix')
  c1 <- grepl(f1, colnames(xp))
  c2 <- grepl(f2, colnames(xp))
  r1 <- newdata[[var]] == f1
  r2 <- newdata[[var]] == f2
  ## difference rows of xp for data from comparison
  X <- xp[r1, ] - xp[r2, ]
  ## zero out cols of X related to splines for other lochs
  X[, ! (c1 | c2)] <- 0
  ## zero out the parametric cols
  X[, !grepl('^s\\(', colnames(xp))] <- 0
  dif <- X %*% coef(model)
  se <- sqrt(rowSums((X %*% vcov(model, unconditional = unconditional)) * X))
  crit <- qt(alpha/2, df.residual(model), lower.tail = FALSE)
  upr <- dif + (crit * se)
  lwr <- dif - (crit * se)
  data.frame(pair = paste(f1, f2, sep = '-'),
             diff = dif,
             se = se,
             upper = upr,
             lower = lwr)
}

# Pairwise comparison of the estimated smooths

# Max.temp
comp1 <- smooth_diff(lmgGamm[[7]], newD1, 'crash', 'noCrash', 'lmg.crash')
x11(); par(mfrow = c(2, 2))
plot(v,
     comp1$diff,
     ylim = c(min(comp1$lower), max(comp1$upper)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "max.temp",
     ylab = "difference btw crash/noCrash smooth",
     col = "darkorange4")

polygon(x = c(v, rev(v)),
        y = c(comp1$upper, rev(comp1$lower)),
        col = alpha("darkorange4", 0.25),
        border = NA)
abline(h = 0, col = "grey", lwd = 2.5, lty = 4)


# Prec
comp2 <- smooth_diff(lmgGamm[[7]], newD2, 'crash', 'noCrash', 'lmg.crash')

plot(v1,
     comp2$diff,
     ylim = c(min(comp2$lower), max(comp2$upper)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "prec",
     ylab = "difference btw crash/noCrash smooth",
     col = "skyblue4")

polygon(x = c(v1, rev(v1)),
        y = c(comp2$upper, rev(comp2$lower)),
        col = alpha("skyblue4", 0.25),
        border = NA)
abline(h = 0, col = "grey", lwd = 2.5, lty = 4)

# DATE
comp3 <- smooth_diff(lmgGamm[[7]], newD3, 'crash', 'noCrash', 'lmg.crash')

plot(v3,
     comp3$diff,
     ylim = c(min(comp3$lower), max(comp3$upper)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "DATE",
     ylab = "difference btw crash/noCrash smooth",
     col = "plum4")

polygon(x = c(v3, rev(v3)),
        y = c(comp3$upper, rev(comp3$lower)),
        col = alpha("plum4", 0.25),
        border = NA)
abline(h = 0, col = "grey", lwd = 2.5, lty = 4)

# nest.dens
comp4 <- smooth_diff(lmgGamm[[7]], newD4, 'crash', 'noCrash', 'lmg.crash')

plot(v2,
     comp4$diff,
     ylim = c(min(comp4$lower), max(comp4$upper)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "nest.dens",
     ylab = "difference btw crash/noCrash smooth",
     col = "forestgreen")

polygon(x = c(v2, rev(v2)),
        y = c(comp4$upper, rev(comp4$lower)),
        col = alpha("forestgreen", 0.25),
        border = NA)
abline(h = 0, col = "grey", lwd = 2.5, lty = 4)





