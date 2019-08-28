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
pglmm <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + I(scale(max.temp)^2) + scale(max.wind) + lmg.crash + scale(nest.dens)
                 + (1|fox.year)
                 + offset(log(OBS.LENGTH)),
             family = poisson(),
             #method = "REML",
             #select = TRUE,
             data = data)
summary(pglmm)

par(mfrow = c(2, 2)); plot(pglmm)
x11(); 
sims <- simulateResiduals(pglmm)
plot(sims)
testDispersion(sims)
testZeroInflation(sims)
#### *** TO DO LIST ####
# 1 - correlation between variables
# 2 - Plot of variables !

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
