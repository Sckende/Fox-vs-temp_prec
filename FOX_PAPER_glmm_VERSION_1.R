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

# Creation of the variable for the offset
data$log.obs <- log(data$OBS.LENGTH)

# Full model
pglmm <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + I(scale(max.temp)^2) + scale(max.wind) + lmg.crash + scale(nest.dens)
                 + (1|fox.year)
                 + offset(log.obs),
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
            + offset(log.obs),
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
               + offset(log.obs),
               family = poisson(),
               #method = "REML",
               #select = TRUE,
               data = data)
summary(mod[[1]])

mod[[2]] <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + I(scale(max.temp)^2) + scale(max.wind) + scale(nest.dens)
                  + (1|fox.year)
                  + offset(log.obs),
                  family = poisson(),
                  #method = "REML",
                  #select = TRUE,
                  data = data)
summary(mod[[2]])

mod[[3]] <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + scale(max.wind) + scale(nest.dens)
                  + (1|fox.year)
                  + offset(log.obs),
                  family = poisson(),
                  #method = "REML",
                  #select = TRUE,
                  data = data)
summary(mod[[3]])

mod[[4]] <- glmer(AD.atq.number ~ scale(prec) + scale(max.temp) + scale(nest.dens)
                  + (1|fox.year)
                  + offset(log.obs),
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

mod[[5]] <- glmer(AD.atq.number ~ 1
                  + (1|fox.year)
                  + offset(log.obs),
                  family = poisson(),
                  #method = "REML",
                  #select = TRUE,
                  data = data)
summary(mod[[5]])

# ------------------- #
#### Results plot ####
# ------------------#

# Atq number vs. max.temp #
# ---------------------- #
range(data$max.temp) # 0.595 - 15.351

v <- seq(0, 16, by = 0.01)
newdat <- data.frame(max.temp = v, prec = mean(data$prec), nest.dens = mean(data$nest.dens), log.obs = mean(data$log.obs))

p <- predict(mod[[4]], newdata = newdat, type = "response", re.form = NA)

#plot(data$max.temp, data$AD.atq.rate)

# Atq number vs. prec #
# ---------------------- #
range(data$prec) # 0 - 20

v1 <- seq(0, 20, by = 0.01)
newdat1 <- data.frame(max.temp = mean(data$max.temp), prec = v1, nest.dens = mean(data$nest.dens), log.obs = mean(data$log.obs))

p1 <- predict(mod[[4]], newdata = newdat1, type = "response", re.form = NA)

#plot(data$max.temp, data$AD.atq.rate)
#plot(v1, p1, type = "l", bty = "n", lwd = 2, xlab = "Cumulative precipitation")

# Atq number vs. nest.dens #
# ----------------------- #
range(data$nest.dens) # 0.42 - 9.26

v2 <- seq(0, 10, by = 0.01)
newdat2 <- data.frame(max.temp = mean(data$max.temp), prec = mean(data$prec), nest.dens = v2, log.obs = mean(data$log.obs))

p2 <- predict(mod[[4]], newdata = newdat2, type = "response", re.form = NA)


# GRAPHICS #
# ------- #

# Temperature effect...
x11()
plot(v, p, ylim = c(0, 7), type = "l", bty = "n", lwd = 2.5, xlab = "Maximal temperature", ylab = "Fox attack number per hour", col = "darkorange4")
# ... & associated random effects 
re <- unique(data$fox.year)
for(i in re){
  newdat2 <- data.frame(max.temp = v, prec = mean(data$prec), nest.dens = mean(data$nest.dens), log.obs = mean(data$log.obs), fox.year = i)
  pp <- predict(mod[[4]], newdata = newdat2, type = "response")
  lines(v, pp, type = "l", lwd = 1, col = alpha("darkorange", 0.25))
}
points(data$max.temp, 3600*data$AD.atq.rate,col = "darkorange4") # WARNINGS ! points are missing on the plot because their value is really high

# Precipitation effects....
x11()
plot(v1, p1, type = "l", ylim = c(0, 4), lwd = 2, col = "cyan4", bty = "n", ylab = "Fox attack number per hour", xlab = "Cumulative precipitation (mm)")

# ... & associated random effects
re <- unique(data$fox.year)
for(i in re){
  newdat3 <- data.frame(max.temp = mean(data$max.temp), prec = v1, nest.dens = mean(data$nest.dens), log.obs = mean(data$log.obs), fox.year = i)
  pp <- predict(mod[[4]], newdata = newdat3, type = "response")
  lines(v1, pp, type = "l", lwd = 1, col = alpha("cyan", 0.25))
}
points(data$prec, 3600*data$AD.atq.rate, col = "cyan4") # WARNINGS ! points are missing on the plot because their value is really high


# Nest density effect ...
x11()
plot(v2, p2, ylim = c(0, 6), type = "l", lwd = 2, bty = "n", xlab = "Goose nest density", col = "darkgreen", ylab = "Fox attack number per hour")

# ... & associated random effects
re <- unique(data$fox.year)
for(i in re){
  newdat4 <- data.frame(max.temp = mean(data$max.temp), prec = mean(data$prec), nest.dens = v2, log.obs = mean(data$log.obs), fox.year = i)
  pp <- predict(mod[[4]], newdata = newdat4, type = "response")
  lines(v2, pp, type = "l", lwd = 1, col = alpha("green", 0.25))
}
points(data$nest.dens, 3600*data$AD.atq.rate, col = "darkgreen") # WARNINGS ! points are missing on the plot because their value is really high
