rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
data <- read.table("FOX_PAPER_Complete_database.txt", h = T, sep = "\t", dec = ".") # ***Climatic variables per 4hours observation***
summary(data)

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

# ----------------------------- #
#### Poisson family in GLM-M ####
# ------------------------------#

# Creation of the random variable fox.year
data$fox.year <- paste(data$FOX.ID, data$YEAR, sep = "-")

# Creation of a lemming variable with only two levels
data$lmg.crash[data$lmg.year == "crash"] <- "crash"
data$lmg.crash[data$lmg.year %in% c("inter", "peak")] <- "noCrash"

#data <- data[!data$YEAR %in% 2004:2005,]

# Full model
pglmm <- glmer(AD.atq.number ~ prec + I(max.temp) + max.wind + lmg.crash + nest.dens
                 + (1|fox.year)
                 + offset(log(OBS.LENGTH)),
             family = poisson(),
             #method = "REML",
             #select = TRUE,
             data = data)
summary(pglmm)

par(mfrow = c(2, 2)); plot(pglmm.1)
x11(); plot(simulateResiduals(pglmm.1))

plot(predict(pglmm.1, type = "response"))
par(mfrow = c(1, 2))
hist(data$AD.atq.number, breaks = 0:50)
hist(predict(pglmm.1, type = "response"), breaks = 0:50)

# Testing significance of random effects

pglm <- glm(AD.atq.number ~ prec + I(max.temp) + max.wind + lmg.crash + nest.dens
               + offset(log(OBS.LENGTH)),
               family = poisson(),
               #method = "REML",
               #select = TRUE,
               data = data)
anova(pglmm, pglm) # DOESN'T WORK 


# Models compairison
pglmm <- list()

# pglmm[1] <- glmer(AD.atq.number ~ 1
#                   + offset(log(OBS.LENGTH))
#                   + (1|fox.year),
#                   family = poisson(),
#                   #method = "REML", # "REstricted Maximum Likelihood"
#                   #select = TRUE,
#                   data = data)# DOESN'T WORK 

pglmm[1] <- glmer(AD.atq.number ~ prec + I(max.temp) + max.wind + lmg.crash + nest.dens
                 + offset(log(OBS.LENGTH))
                 + (1|fox.year),
                 family = poisson(),
                 #method = "REML",
                 #select = TRUE,
                 data = data)
