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
library("MuMIn")
library("splines")
library("r2glmm")


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
x11(); 
pairs(data[, c(13, 14, 15, 16, 17, 21, 22, 24, 26)], upper.panel = panel.cor)

ggpairs(data[, c(13, 14, 15, 16, 17, 21, 22, 24, 26)])
graphics.off()

# ------------------------------- #
#### Recuperation of database ####
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

# Scaled data
scaleData <- apply(data_test[,c(14, 17, 21, 22, 24, 26)], MARGIN = 2, scale)

scaleData <- cbind(scaleData, data_test[, c(7, 8, 23, 27:30)])
summary(scaleData)

# ----------------------------- #
#### Poisson family in GLM-M ####
# ------------------------------#
# ------------------- #
# Models compairison  #
# ----------------- #

control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2000000)) # Helping the convergence with "maxfun" increasing the number of iterations 

# Models
mod <- list()

################## lmg.crash ###################
mod[[1]] <- glmer(AD.atq.number ~ 1
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[1]])

mod[[2]] <- glmer(AD.atq.number ~ prec*lmg.crash + max.temp*lmg.crash + nest.dens*lmg.crash + REL.DATE
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[2]])

mod[[3]] <- glmer(AD.atq.number ~ prec*lmg.crash + max.temp*lmg.crash + nest.dens + REL.DATE*lmg.crash
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test,
                  contrasts = list(lmg.crash = contr.sum))
summary(mod[[3]])

# Marginal and conditional R2
r.squaredGLMM(mod[[3]]) # package MuMIn

x11(); par(mfrow = c(2, 2))
visreg(mod[[3]], "max.temp", by = "lmg.crash", overlay = T)
visreg(mod[[3]], "prec", by = "lmg.crash", overlay = T)
visreg(mod[[3]], "REL.DATE", by = "lmg.crash", overlay = T)
visreg(mod[[3]], "nest.dens")

# Tests with DHARma package
sims <- simulateResiduals(mod[[3]])
plot(sims)
testDispersion(sims)
testZeroInflation(sims)

mod[[4]] <- glmer(AD.atq.number ~ prec*lmg.crash + max.temp*lmg.crash + nest.dens*lmg.crash + REL.DATE*lmg.crash
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[4]])
X11(); par(mfrow = c(2, 2))
visreg(mod[[4]], "max.temp", by = "lmg.crash", overlay = T)
visreg(mod[[4]], "prec", by = "lmg.crash", overlay = T)
visreg(mod[[4]], "DATE", by = "lmg.crash", overlay = T)
visreg(mod[[4]], "nest.dens", by = "lmg.crash", overlay = T)

################## lmg.abun ###################
mod[[5]] <- glmer(AD.atq.number ~ prec*lmg.abun + max.temp*lmg.abun + nest.dens*lmg.abun + REL.DATE
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[5]])

mod[[6]] <- glmer(AD.atq.number ~ prec*lmg.abun + max.temp*lmg.abun + nest.dens + REL.DATE*lmg.abun
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[6]])

mod[[7]] <- glmer(AD.atq.number ~ prec*lmg.abun + max.temp*lmg.abun + nest.dens*lmg.abun + REL.DATE*lmg.abun
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[7]])


mod[[8]] <- glmer(AD.atq.number ~ prec*lmg.abun + max.temp*lmg.abun + nest.dens + REL.DATE
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[8]])

####

mod[[9]] <- glmer(AD.atq.number ~ prec + max.temp + nest.dens + REL.DATE + lmg.crash
                  + (1|fox.year)
                  + offset(log.obs),
                  family = poisson(),
                  control = control,
                  #method = "REML",
                  #select = TRUE,
                  data = data_test)
summary(mod[[9]])

mod[[10]] <- glmer(AD.atq.number ~ prec*lmg.crash + max.temp*lmg.crash + nest.dens + REL.DATE
                  + (1|fox.year)
                  + offset(log.obs),
                  family = poisson(),
                  control = control,
                  #method = "REML",
                  #select = TRUE,
                  data = data_test)
summary(mod[[10]])

mod[[11]] <- glmer(AD.atq.number ~ lmg.crash + nest.dens + REL.DATE
                   + (1|fox.year)
                   + offset(log.obs),
                   family = poisson(),
                   control = control,
                   #method = "REML",
                   #select = TRUE,
                   data = data_test)
summary(mod[[11]])


# AIC table
aictab(mod, modnames = NULL)

# summary of the best model
summary(mod[[3]])

# Computation of confidence intervals
#confint(mod[[3]])

# Computation of R squared
r.squaredGLMM(mod[[3]])
r.squaredGLMM(mod[[3]], mod[[1]]) # Check the method !

# VÉRIFICATION DE LA MULTICOLINÉARITÉ
car::vif(mod[[3]])
car::vif(mod[[4]])
car::vif(mod[[2]])
car::vif(mod[[6]])
car::vif(mod[[7]])
car::vif(mod[[8]])
car::vif(mod[[9]])
car::vif(mod[[10]])

# Test de la significativité des pentes
emtrends(mod[[3]], pairwise~lmg.crash, var = "max.temp")
emmip(mod[[3]], lmg.crash~max.temp, cov.reduce = range)

emtrends(mod[[3]], pairwise~lmg.crash, var = "prec")
emmip(mod[[3]], lmg.crash~prec, cov.reduce = range)

emtrends(mod[[3]], pairwise~lmg.crash, var = "REL.DATE")
emmip(mod[[3]], lmg.crash~REL.DATE, cov.reduce = range)
# Save the best model for rmarkdown document
# save(mod, file = "FOX_attack_all_glmm.rda")
# bestMod <- mod[[18]]
# save(bestMod, file = "FOX_attack_best_glmm.rda")

# ------------------- #
#### Results plot ####
# ------------------#

# Atq number vs. max.temp #
# ---------------------- #
range(data_test$max.temp) # 0.595 - 15.351

v <- seq(0, 16, by = 0.01)
newdat.crash <- data.frame(max.temp = v,
                           prec = mean(data_test$prec),
                           nest.dens = mean(data_test$nest.dens),
                           REL.DATE = mean(data_test$REL.DATE),
                           lmg.crash = "crash",
                           log.obs = mean(data$log.obs))
newdat.noCrash <- data.frame(max.temp = v,
                             prec = mean(data_test$prec),
                             nest.dens = mean(data_test$nest.dens),
                            REL.DATE = mean(data_test$REL.DATE),
                             lmg.crash = "noCrash",
                             log.obs = mean(data$log.obs))

p.crash <- predict(mod[[3]], newdata = newdat.crash, type = "response", re.form = NA)
p.noCrash <- predict(mod[[3]], newdata = newdat.noCrash, type = "response", re.form = NA)

#plot(data$max.temp, data$AD.atq.rate)

# Atq number vs. prec #
# ---------------------- #
range(data_test$prec) # 0 - 20

v1 <- seq(0, 20, by = 0.01)
newdat1.crash <- data.frame(max.temp = mean(data_test$max.temp),
                            prec = v1,
                            nest.dens = mean(data_test$nest.dens),
                            lmg.crash = "crash",
                            REL.DATE = mean(data_test$REL.DATE),
                            log.obs = mean(data_test$log.obs))

p1.crash <- predict(mod[[3]], newdata = newdat1.crash, type = "response", re.form = NA)

# ----- #
newdat1.noCrash <- data.frame(max.temp = mean(data_test$max.temp),
                              prec = v1,
                              nest.dens = mean(data_test$nest.dens),
                              lmg.crash = "noCrash",
                              REL.DATE = mean(data_test$DATE),
                              log.obs = mean(data_test$log.obs))

p1.noCrash <- predict(mod[[3]], newdata = newdat1.noCrash, type = "response", re.form = NA)

#plot(data$max.temp, data$AD.atq.rate)
#plot(v1, p1, type = "l", bty = "n", lwd = 2, xlab = "Cumulative precipitation")

# Atq number vs. nest.dens #
# ----------------------- #
range(data_test$nest.dens) # 0.42 - 9.26

v2 <- seq(0, 10, by = 0.01)
newdat2 <- data.frame(max.temp = mean(data_test$max.temp),
                      prec = mean(data_test$prec),
                      nest.dens = v2,
                      REL.DATE = mean(data_test$REL.DATE),
                      lmg.crash = "noCrash",
                      log.obs = mean(data$log.obs))

p2 <- predict(mod[[3]], newdata = newdat2, type = "response", re.form = NA)

# Atq number vs. rel.date #
# ------------------- #
range(data_test$REL.DATE) # -4 - 42

v3 <- seq(-4, 42, by = 1)
newdat3.crash <- data.frame(max.temp = mean(data_test$max.temp),
                      prec = mean(data_test$prec),
                      nest.dens = mean(data_test$nest.dens),
                      REL.DATE = v3,
                      lmg.crash = "crash",
                      log.obs = mean(data$log.obs))

p3.crash <- predict(mod[[3]], newdata = newdat3.crash, type = "response", re.form = NA)



newdat3.noCrash <- data.frame(max.temp = mean(data_test$max.temp),
                            prec = mean(data_test$prec),
                            nest.dens = mean(data_test$nest.dens),
                            REL.DATE = v3,
                            lmg.crash = "noCrash",
                            log.obs = mean(data$log.obs))

p3.noCrash <- predict(mod[[3]], newdata = newdat3.noCrash, type = "response", re.form = NA)
# --------- #
# GRAPHICS #
# ------- #

# -------------------- #
# Temperature effect...
# ------------------- #
x11()
par(mfrow = c(1, 2))
# ------ #
plot(v, p.crash, ylim = c(0, 7), type = "l", bty = "n", lwd = 2.5, xlab = "Maximal temperature", ylab = "Fox attack number per hour", col = "darkorange4", main = "Crash of lemmings")
# ... & associated random effects
re <- unique(data_test$fox.year)
for(i in re){
  nd2.crash <- data.frame(max.temp = v,
                          prec = mean(data_test$prec),
                          nest.dens = mean(data_test$nest.dens),
                          REL.DATE = mean(data_test$REL.DATE),
                          lmg.crash = "crash",
                          log.obs = mean(data_test$log.obs),
                          fox.year = i)
  pp.crash <- predict(mod[[3]], newdata = nd2.crash, type = "response")
  lines(v, pp.crash, type = "l", lwd = 1, col = alpha("darkorange", 0.25))
}
#points(data_test$max.temp, 3600*data_test$AD.atq.rate,col = "darkorange4") # WARNINGS ! points are missing on the plot because their value is really high

# # ------ #
plot(v, p.noCrash, ylim = c(0, 7), type = "l", bty = "n", lwd = 2.5, xlab = "Maximal temperature", ylab = "Fox attack number per hour", col = "darkorange4", main = "No crash of lemming")
# ... & associated random effects
re <- unique(data_test$fox.year)
for(i in re){
  nd2.noCrash <- data.frame(max.temp = v,
                            prec = mean(data_test$prec),
                            nest.dens = mean(data_test$nest.dens),
                            REL.DATE = mean(data_test$REL.DATE),
                            lmg.crash = "noCrash",
                            log.obs = mean(data_test$log.obs),
                            fox.year = i)
  pp.noCrash <- predict(mod[[3]], newdata = nd2.noCrash, type = "response")
  lines(v, pp.noCrash, type = "l", lwd = 1, col = alpha("darkorange", 0.25))
}
# points(data_test$max.temp, 3600*data_test$AD.atq.rate,col = "darkorange4") # WARNINGS ! points are missing on the plot because their value is really high
# 
# # ------------------------ #
# # Precipitation effects....#
# # ------------------------ #
x11()
par(mfrow = c(1, 2))
# ----- #
plot(v1, p1.crash, type = "l", ylim = c(0, 8), lwd = 2, col = "skyblue4", bty = "n", ylab = "Fox attack number per hour", xlab = "Cumulative precipitation (mm)", main = "Crash of lemmings")

# ... & associated random effects
re <- unique(data_test$fox.year)
for(i in re){
  nd3.crash <- data.frame(max.temp = mean(data_test$max.temp),
                          prec = v1,
                          nest.dens = mean(data_test$nest.dens),
                          REL.DATE = mean(data_test$REL.DATE),
                          lmg.crash = "crash",
                          log.obs = mean(data_test$log.obs),
                          fox.year = i)
  pp.crash <- predict(mod[[3]], newdata = nd3.crash, type = "response")
  lines(v1, pp.crash, type = "l", lwd = 1, col = alpha("skyblue3", 0.25))
}
# points(data_test$prec, 3600*data_test$AD.atq.rate, col = "skyblue4") # WARNINGS ! points are missing on the plot because their value is really high
# 
# # ----- #
plot(v1, p1.noCrash, type = "l", ylim = c(0, 8), lwd = 2, col = "skyblue4", bty = "n", ylab = "Fox attack number per hour", xlab = "Cumulative precipitation (mm)", main = "No crash of lemmings")

# ... & associated random effects
re <- unique(data_test$fox.year)
for(i in re){
  nd3.noCrash <- data.frame(max.temp = mean(data_test$max.temp),
                            prec = v1,
                            nest.dens = mean(data_test$nest.dens),
                            REL.DATE = mean(data_test$REL.DATE),
                            lmg.crash = "noCrash",
                            log.obs = mean(data_test$log.obs),
                            fox.year = i)
  pp.noCrash <- predict(mod[[3]], newdata = nd3.noCrash, type = "response")
  lines(v1, pp.noCrash, type = "l", lwd = 1, col = alpha("skyblue3", 0.25))
}
# points(data_test$prec, 3600*data_test$AD.atq.rate, col = "skyblue4") # WARNINGS ! points are missing on the plot because their value is really high
# 
# # ----------------------- #
# # Nest density effect ...#
# # --------------------- #
# x11()
# par(mfrow = c(1, 2))
plot(v2, p2, ylim = c(0, 6), type = "l", lwd = 2, bty = "n", xlab = "Goose nest density", col = "darkgreen", ylab = "Fox attack number per hour")

# ... & associated random effects
re <- unique(data_test$fox.year)
for(i in re){
  nd4 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = v2,
                    REL.DATE = mean(data_test$REL.DATE),
                    lmg.crash = "noCrash",
                    log.obs = mean(data$log.obs),
                    fox.year = i)
  pp <- predict(mod[[3]], newdata = nd4, type = "response")
  lines(v2, pp, type = "l", lwd = 1, col = alpha("green", 0.25))
}
# points(data_test$nest.dens, 3600*data_test$AD.atq.rate, col = "darkgreen") # WARNINGS ! points are missing on the plot because their value is really high
# 
# # --------------- #
# # Date effect ...#
# # ------------- #
# x11()
# par(mfrow = c(1, 2))
plot(v3, p3.crash, ylim = c(0, 15), type = "l", lwd = 2, bty = "n", xlab = "Date of observations", col = "plum4", ylab = "Fox attack number per hour", main = "Crash of lemmings")

# ... & associated random effects
re <- unique(data_test$fox.year)
for(i in re){
  nd5 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = mean(data_test$nest.dens),
                    REL.DATE = v3,
                    lmg.crash = "crash",
                    log.obs = mean(data$log.obs),
                    fox.year = i)
  pp <- predict(mod[[3]], newdata = nd5, type = "response")
  lines(v3, pp, type = "l", lwd = 1, col = alpha("plum3", 0.25))
}
# points(data_test$DATE, 3600*data_test$AD.atq.rate, col = "plum4") # WARNINGS ! points are missing on the plot because their value is really high
# 

plot(v3, p3.noCrash, ylim = c(0, 15), type = "l", lwd = 2, bty = "n", xlab = "Date of observations", col = "plum4", ylab = "Fox attack number per hour", main = "No crash of lemmings")

# ... & associated random effects
re <- unique(data_test$fox.year)
for(i in re){
  nd5 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = mean(data_test$nest.dens),
                    REL.DATE = v3,
                    lmg.crash = "noCrash",
                    log.obs = mean(data$log.obs),
                    fox.year = i)
  pp <- predict(mod[[3]], newdata = nd5, type = "response")
  lines(v3, pp, type = "l", lwd = 1, col = alpha("plum3", 0.25))
}
# graphics.off()

#### Visreg and paper plots ####
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Glmm_visreg_temp.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

visreg(mod[[3]],
       "max.temp",
       "lmg.crash",
       overlay = T,
       scale = "response",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "Maximal temperature (°C)",
       #xlim = c(0,16),
       xaxt = "n",
       ylab = "Fox attack rate (nb/hour)",
       ylim = c(0, 10),
       gg = F,
       fill.par = list(col = c(alpha("darkorange1", 0.25), alpha("darkorange4", 0.25))),
       line = list(col = c("darkorange1", "darkorange4")),
       points = list(col = c(alpha("darkorange1", 0.7), alpha("darkorange4", 0.7)), cex = 2),
       legend = F,
#       strip.names = c("low", "high"),
       cex.axis = 1.5,
       cex.lab = 1.5,
       cex.main = 1.5,
       rug = 2,
       partial = FALSE) 
legend("topright", legend = c("low", "high"), fill = c("darkorange1", "darkorange4"), border = NA, bty = "n", cex = 1.5)
axis(1, cex.axis = 1.5, at = seq(0, 16, 2), xpd = T)
#dev.off()

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Glmm_prec_visreg.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
visreg(mod[[3]],
       "prec",
       "lmg.crash",
       overlay = T,
       scale = "response",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "Total precipitation (mm)",
       ylab = "Fox attack rate (nb/hour)",
       gg = F,
       fill.par = list(col = c(alpha("skyblue1", 0.25), alpha("skyblue4", 0.25))),
       line = list(col = c("skyblue1", "skyblue4")),
       points = list(col = c(alpha("skyblue1", 0.7), alpha("skyblue4", 0.7)), cex = 1),
       legend = F,
       cex.axis = 1.5,
       cex.lab = 1.5,
       rug = 2,
       partial = F)
legend(x = 0, y = 14, legend = c("low", "high"), fill = c("skyblue1", "skyblue4"), border = NA, bty = "n", cex = 1.5)
#dev.off()

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Glmm_rel.date_visreg.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
j <- visreg(mod[[3]],
       "REL.DATE",
       "lmg.crash",
       overlay = T,
       scale = "response",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "Relative date",
       xaxt = "n",
       ylab = "Fox attack rate (nb/hour)",
       ylim = c(0, 30),
       gg = F,
       fill.par = list(col = c(alpha("plum2", 0.25), alpha("plum4", 0.25))),
       line = list(col = c("plum2", "plum4")),
       points = list(col = c(alpha("plum1", 0.7), alpha("plum4", 0.7)), cex = 1),
       legend = F,
       cex.axis = 1.5,
       cex.lab = 1.5,
       rug = 2,
       partial = F)
legend("topright", legend = c("low", "high"), fill = c("plum2", "plum4"), border = NA, bty = "n", cex = 1.5)
axis(1, cex.axis = 1.5, at = seq(-4, 44, 4), xpd = T)
#dev.off()

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Glmm_nest_visreg.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
visreg(mod[[3]],
       "nest.dens",
       scale = "response",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "Goose nest dens",
       xlim = c(min(data_test$nest.dens), max(data_test$nest.dens)),
       xaxt = "n",
       ylab = "Fox attack rate (nb/hour)",
       ylim = c(0, 6),
       gg = F,
       fill.par = list(col = c(alpha("forestgreen", 0.25))),
       line = list(col = c("forestgreen")),
       points = list(col = c(alpha("forestgreen", 0.7)), cex = 1),
       legend = F,
       cex.axis = 1.5,
       cex.lab = 1.5,
       rug = 2,
       partial = F)
axis(1, at = seq(0, 9.5, 2), xpd = T, cex.axis = 1.5)
#dev.off()
