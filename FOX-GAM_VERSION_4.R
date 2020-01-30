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

# -------------------------- #
#### the best glmm model ####
# ------------------------ #
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
# ---------------------------- #
#### test with gamm models ####
# -------------------------- #
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
lmgGamm[[4]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + REL.DATE
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[4]])

plot(lmgGamm[[4]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[5]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + REL.DATE + nest.dens
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[5]])

plot(lmgGamm[[5]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[6]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + s(REL.DATE) + nest.dens
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[6]])

plot(lmgGamm[[6]], page = 1, all.terms = TRUE, residuals = TRUE)

# ----- #
lmgGamm[[7]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + s(max.temp, by = lmg.crash) + lmg.crash + s(REL.DATE, by = lmg.crash) + nest.dens
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
visreg(lmgGamm[[7]], "max.temp", "lmg.crash", overlay = T, scale = "response", cond = list(log.obs = log(3600)),
       #log = "y", 
       ylim = c(0.01, 70), bty = "n")
y <- ((data_test$goo.atq.number/data_test$OBS.LENGTH)*3600) + 0.01
points(x = data_test$max.temp, y = y, col = ifelse(data_test$lmg.crash == "crash", "red", "blue"))

visreg(lmgGamm[[7]], "max.temp", "lmg.crash", overlay = T, scale = "linear", cond = list(log.obs = log(3600)), points = list(cex = 1), bty = "n")


visreg(lmgGamm[[7]], "prec", "lmg.crash", overlay = T, scale =
         "response")
visreg(lmgGamm[[7]], "REL.DATE", "lmg.crash", overlay = T, bty = "n", scale = "response")
visreg(lmgGamm[[7]], "nest.dens", "lmg.crash", overlay = T, bty = "n", scale = "response")

predict(lmgGamm[[7]], type = "response")

#saveRDS(lmgGamm[[7]], "lmgGamm7")

# ----- #
lmgGamm[[8]] <- gam(AD.atq.number ~ 1
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[8]])

# ----- #
lmgGamm[[9]] <- gam(AD.atq.number ~ prec*lmg.crash + s(max.temp, by = lmg.crash) + lmg.crash + s(REL.DATE, by = lmg.crash) + nest.dens
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[9]])
visreg(lmgGamm[[9]], "prec", "lmg.crash", overlay = T)

# ----- #
lmgGamm[[10]] <- gam(AD.atq.number ~ s(prec , by = lmg.crash) + max.temp*lmg.crash + lmg.crash + s(REL.DATE, by = lmg.crash) + nest.dens
                    + s(fox.year, bs = "re") 
                    + offset(log.obs),
                    family = poisson(),
                    method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                    #select = TRUE,
                    data = data_test)
summary(lmgGamm[[10]])
#saveRDS(lmgGamm[[10]], "lmgGamm10")

visreg(lmgGamm[[10]], "prec", "lmg.crash", overlay = T)
visreg(lmgGamm[[10]], "max.temp", "lmg.crash", overlay = T)
visreg(lmgGamm[[10]], "REL.DATE", "lmg.crash", overlay = T)
x11(); plot(lmgGamm[[10]], page = 1, all.terms = TRUE, residuals = TRUE)
# Tests with DHARma package
sims <- simulateResiduals(lmgGamm[[10]])
x11();plot(sims)

# plot.gam(lmgGamm[[10]], select = 1)
x11(); par(mfrow = c(2, 2))
visreg(lmgGamm[[10]], "max.temp", "lmg.crash", overlay = T, scale = "response", cond = list(log.obs = log(3600)),
       #log = "y", 
       ylim = c(0.01, 70), bty = "n")
y <- ((data_test$goo.atq.number/data_test$OBS.LENGTH)*3600) + 0.01
points(x = data_test$max.temp, y = y, col = ifelse(data_test$lmg.crash == "crash", "red", "blue"))

visreg(lmgGamm[[10]], "max.temp", "lmg.crash", overlay = T, scale = "linear", cond = list(log.obs = log(3600)), points = list(cex = 1), bty = "n")


visreg(lmgGamm[[10]], "prec", "lmg.crash", overlay = T, scale =
         "response")
visreg(lmgGamm[[10]], "REL.DATE", "lmg.crash", overlay = T, bty = "n", scale = "response")
visreg(lmgGamm[[10]], "nest.dens", "lmg.crash", overlay = T, bty = "n", scale = "response")

predict(lmgGamm[[10]], type = "response")


# ----- #
lmgGamm[[11]] <- gam(AD.atq.number ~ te(prec , max.temp) + lmg.crash + s(REL.DATE, by = lmg.crash) + nest.dens
                     + s(fox.year, bs = "re") 
                     + offset(log.obs),
                     family = poisson(),
                     method = "REML", # Automatic selection of the lambda term (trade-off between likelihood and wiggliness)
                     #select = TRUE,
                     data = data_test)
summary(lmgGamm[[11]])
visreg(lmgGamm[[11]],"prec" , "max.temp", overlay = T)
visreg(lmgGamm[[11]], "max.temp", "prec", overlay = T)

graphics.off()

### ------ ###
#### AIC ####
### ---- ###
# h <- lapply(lmgGamm, function(x){
#   j <- print(x$formula)
#   j
# })
# h <- as.vector(as.character(h))
# 
# 
# Modnames <- paste(paste("mod", 1:length(lmgGamm), sep = " "), h, sep = "-")
# AIC <- aictab(cand.set = lmgGamm, modnames = Modnames, sort = TRUE)
# print(AIC, digit = 2)
comp_aic <- AIC(lmgGamm[[1]], lmgGamm[[2]], lmgGamm[[3]], lmgGamm[[4]], lmgGamm[[5]], lmgGamm[[6]], lmgGamm[[7]], lmgGamm[[8]], lmgGamm[[9]], lmgGamm[[10]], lmgGamm[[11]])
comp_aic$delta <- round(comp_aic$AIC - min(comp_aic$AIC), digits = 2)
comp_aic[order(comp_aic$AIC), ]
### -------------------------------- ###
#### k variation in the best model ####
### ------------------------------ ###

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

