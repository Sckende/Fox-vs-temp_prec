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

# -------------------------------- #
#### Correlation btw variables ####
# ------------------------------ #

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r = ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p = ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p = ", "< 0.01", sep = "")
  text(0.5, 0.4, txt2)
}
x11(); pairs(data[, c(14, 17, 22, 24)], upper.panel = panel.cor)

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
plot(pglmm)

sims <- simulateResiduals(pglmm)

x11(); plot(sims)
testDispersion(sims)
testZeroInflation(sims)

x11(); par(mfrow = c(1, 2))
hist(data$AD.atq.number, breaks = 0:50)
hist(predict(pglmm, type = "response"), breaks = 0:50)

# --------------------------------------- #
# Testing significance of random effects #
# ------------------------------------- #

pglm <- glm(AD.atq.number ~ prec + I(max.temp) + max.wind + lmg.crash + nest.dens
            + offset(log(OBS.LENGTH)),
            family = poisson(),
            #method = "REML",
            #select = TRUE,
            data = data)
anova(pglmm, pglm) # DOESN'T WORK 

# ------------------- #
# Models compairison #
# ----------------- #
mod <- list()
mod[[1]] <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + I(scale(max.temp)^2) + scale(max.wind) + lmg.crash + scale(nest.dens)
               + (1|fox.year)
               + offset(log(OBS.LENGTH)),
               family = poisson(),
               #method = "REML",
               #select = TRUE,
               data = data)
summary

mod[[2]] <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + I(scale(max.temp)^2) + scale(max.wind) + scale(nest.dens)
                  + (1|fox.year)
                  + offset(log(OBS.LENGTH)),
                  family = poisson(),
                  #method = "REML",
                  #select = TRUE,
                  data = data)
summary(mod[[2]])

mod[[3]] <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + scale(max.wind) + scale(nest.dens)
                  + (1|fox.year)
                  + offset(log(OBS.LENGTH)),
                  family = poisson(),
                  #method = "REML",
                  #select = TRUE,
                  data = data)
summary(mod[[3]])

mod[[4]] <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + scale(nest.dens)
                  + (1|fox.year)
                  + offset(log(OBS.LENGTH)),
                  family = poisson(),
                  #method = "REML",
                  #select = TRUE,
                  data = data)
summary(mod[[4]])

sims <- simulateResiduals(mod[[4]])

x11(); plot(sims)
testDispersion(sims)
testZeroInflation(sims)

x11(); par(mfrow = c(1, 2))
hist(data$AD.atq.number, breaks = 0:50)
hist(predict(mod[[4]], type = "response"), breaks = 0:50)

# ------------------- #
#### Results plot ####
# ------------------#

plot(data$max.temp, predict(pglmm, type = "response"))
plot(data$prec, predict(pglmm, type = "response"))

