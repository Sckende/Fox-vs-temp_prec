rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
data <- read.table("FOX_PAPER_Complete_database.txt", h = T, sep = "\t", dec = ".") #
lmgGamm7 <- readRDS("lmgGamm7")
lmgGamm10 <- readRDS("lmgGamm10")


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

### ------------------------------------------------ ###
#### Predictions for the best model - lmgGamm7/lmgGamm10 ####
### ---------------------------------------------- ###

# In order to obtain correct confindent intervals
# 1 - Computation predictions without type = response cause SEs are not back transformed in this case and per hour => log.obs = log(3600)
# 2 - Computation of min & max values of CI
# 3 - Back transformation of obtained values

range(data_test$max.temp) # 0.595 - 15.351

v <- seq(0, 16, by = 0.01)

newD1 <- data.frame(max.temp = rep(v, 2),
                    prec = mean(data_test$prec),
                    nest.dens = mean(data_test$nest.dens),
                    REL.DATE = mean(data_test$REL.DATE),
                    lmg.crash = c(rep("crash", 1601), rep("noCrash", 1601)),
                    log.obs = log(3600),
                    fox.year = "Big one-2004")

newD1$fit <- predict(lmgGamm10, newdata = newD1, type = "link", se.fit = TRUE)[[1]]
newD1$se.fit <- predict(lmgGamm10, newdata = newD1, type = "link", se.fit = TRUE)[[2]]
newD1$tranFit <- predict(lmgGamm10, newdata = newD1, type = "response", se.fit = FALSE)

k <- visreg(lmgGamm10, "max.temp", "lmg.crash", plot = F)
plot(k, overlay = T)

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
                    REL.DATE = mean(data_test$REL.DATE),
                    lmg.crash = c(rep("crash", 2001), rep("noCrash", 2001)),
                    log.obs = log(3600),
                    fox.year = "Big one-2004")

newD2$fit <- predict(lmgGamm10, newdata = newD2, type = "link", se.fit = TRUE)[[1]]
newD2$se.fit <- predict(lmgGamm10, newdata = newD2, type = "link", se.fit = TRUE)[[2]]
newD2$tranFit <- predict(lmgGamm10, newdata = newD2, type = "response", se.fit = FALSE)

# Confindent intervals

newD2$maxIC <- exp(newD2$fit + 1.96*(newD2$se.fit))
newD2$minIC <- exp(newD2$fit - 1.96*(newD2$se.fit))

# Atq number vs. REL.DATE #
# ------------------- #
range(data_test$REL.DATE) # -4 - 42

v3 <- seq(-4, 42, by = 1)

newD3 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = mean(data_test$nest.dens),
                    REL.DATE = rep(v3, 2),
                    lmg.crash = c(rep("crash", 47), rep("noCrash", 47)),
                    log.obs = log(3600),
                    fox.year = "Big one-2004")

newD3$fit <- predict(lmgGamm10, newdata = newD3, type = "link", se.fit = TRUE)[[1]]
newD3$se.fit <- predict(lmgGamm10, newdata = newD3, type = "link", se.fit = TRUE)[[2]]
newD3$tranFit <- predict(lmgGamm10, newdata = newD3, type = "response", se.fit = FALSE)

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
                    REL.DATE = mean(data_test$REL.DATE),
                    lmg.crash = c(rep("crash", 1001), rep("noCrash", 1001)),
                    log.obs = log(3600),
                    fox.year = "Big one-2004")

newD4$fit <- predict(lmgGamm10, newdata = newD4, type = "link", se.fit = TRUE)[[1]]
newD4$se.fit <- predict(lmgGamm10, newdata = newD4, type = "link", se.fit = TRUE)[[2]]
newD4$tranFit <- predict(lmgGamm10, newdata = newD4, type = "response", se.fit = FALSE)

# Confindent intervals

newD4$maxIC <- exp(newD4$fit + 1.96*(newD4$se.fit))
newD4$minIC <- exp(newD4$fit - 1.96*(newD4$se.fit))

# With no lemming interaction
newD5 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = v2,
                    REL.DATE = mean(data_test$REL.DATE),
                    lmg.crash = "noCrash",
                    log.obs = log(3600),
                    fox.year = "Big one-2004")
newD5$fit <- predict(lmgGamm10, newdata = newD5, type = "link", se.fit = TRUE)[[1]]
newD5$se.fit <- predict(lmgGamm10, newdata = newD5, type = "link", se.fit = TRUE)[[2]]
newD5$tranFit <- predict(lmgGamm10, newdata = newD5, type = "response", se.fit = FALSE)

# Confindent intervals

newD5$maxIC <- exp(newD5$fit + 1.96*(newD5$se.fit))
newD5$minIC <- exp(newD5$fit - 1.96*(newD5$se.fit))

# --------- #
# GRAPHICS #
# ------- #

# -------------------- #
# Temperature effect...
# ------------------- #
x11()
plot(v,
     newD1$tranFit[newD1$lmg.crash == "crash"],
     ylim = c(0, max(newD1$maxIC)),
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

# ------------------------ #
# Precipitation effects....#
# ------------------------ #

plot(v1,
     newD2$tranFit[newD2$lmg.crash == "crash"],
     ylim = c(0, max(newD2$maxIC)),
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

# --------------- #
# REL.DATE effect ...#
# ------------- #

plot(v3,
     newD3$tranFit[newD3$lmg.crash == "crash"],
     ylim = c(0, max(newD3$maxIC)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "REL.DATE",
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

plot(v2,
     newD4$tranFit[newD4$lmg.crash == "crash"],
     ylim = c(0, max(newD4$maxIC)),
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


graphics.off()

# ------------------------------------------------ #
# Comparing smooths in factor-smooth interactions #
# ---------------------------------------------- #

# Method on https://www.fromthebottomoftheheap.net/2017/10/10/difference-splines-i/

summary(lmgGamm10)

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
# comp1 <- smooth_diff(lmgGamm10, newD1, 'crash', 'noCrash', 'lmg.crash')
# #x11(); par(mfrow = c(2, 2))
# plot(v[comp1$upper >= 0],
#      comp1$diff[comp1$upper >= 0],
#      ylim = c(min(comp1$lower), max(comp1$upper)),
#      xlim = c(min(v), max(v)),
#      type = "l",
#      bty = "n",
#      lwd = 2.5,
#      xlab = "max.temp",
#      ylab = "difference btw crash/noCrash smooth",
#      col = "darkorange4")
# 
# lines(v[comp1$upper < 0],
#       comp1$diff[comp1$upper < 0],
#       ylim = c(min(comp1$lower), max(comp1$upper)),
#       lwd = 2.5,
#       col = "darkorange2")
# 
# polygon(x = c(v[comp1$upper >= 0], rev(v[comp1$upper >= 0])),
#         y = c(comp1$upper[comp1$upper >= 0], rev(comp1$lower[comp1$upper >= 0])),
#         col = alpha("darkorange4", 0.25),
#         border = NA)
# polygon(x = c(v[comp1$upper < 0], rev(v[comp1$upper < 0])),
#         y = c(comp1$upper[comp1$upper < 0], rev(comp1$lower[comp1$upper < 0])),
#         col = alpha("darkorange2", 0.25),
#         border = NA)
# #abline(h = 0, col = "grey", lwd = 2.5, lty = 4)
# legend("topright", legend = c("Non significant difference", "Significant difference"), fill = c("darkorange4", "darkorange2"), border = NA, bty = "n")


# Prec
comp2 <- smooth_diff(lmgGamm10, newD2, 'crash', 'noCrash', 'lmg.crash')

plot(v1,
     comp2$diff,
     ylim = c(min(comp2$lower), max(comp2$upper)),
     xlim = c(min(v1), max(v1)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "prec",
     ylab = "difference btw crash/noCrash smooth",
     col = "skyblue2")

lines(v1[comp2$upper <= 0],
      comp2$diff[comp2$upper <= 0],
      lwd = 2.5,
      col = "skyblue4")

lines(v1[comp2$lower >= 0],
      comp2$diff[comp2$lower >= 0],
      lwd = 2.5,
      col = "skyblue4")

polygon(x = c(v1, rev(v1)),
        y = c(comp2$upper, rev(comp2$lower)),
        col = alpha("skyblue4", 0.25),
        border = NA)

polygon(x = c(v1[comp2$upper <= 0], rev(v1[comp2$upper <= 0])),
        y = c(comp2$upper[comp2$upper <= 0], rev(comp2$lower[comp2$upper <= 0])),
        col = alpha("skyblue4", 0.25),
        border = NA)

polygon(x = c(v1[comp2$lower >= 0], rev(v1[comp2$lower >= 0])),
        y = c(comp2$upper[comp2$lower >= 0], rev(comp2$lower[comp2$lower >= 0])),
        col = alpha("skyblue4", 0.25),
        border = NA)
#abline(h = 0, col = "grey", lwd = 2.5, lty = 4)
legend("topright", legend = c("Non significant difference", "Significant difference"), fill = c("skyblue2", "skyblue4"), border = NA, bty = "n")

# REL.DATE
comp3 <- smooth_diff(lmgGamm10, newD3, 'crash', 'noCrash', 'lmg.crash')

plot(v3,
     comp3$diff,
     ylim = c(min(comp3$lower), max(comp3$upper)),
     xlim = c(min(v3), max(v3)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "REL.DATE",
     ylab = "difference btw crash/noCrash smooth",
     col = "plum2")

lines(v3[comp3$upper <= 0],
      comp3$diff[comp3$upper <= 0],
      lwd = 2.5,
      col = "plum4")

lines(v3[comp3$lower >= 0],
      comp3$diff[comp3$lower >= 0],
      lwd = 2.5,
      col = "plum4")

polygon(x = c(v3, rev(v3)),
        y = c(comp3$upper, rev(comp3$lower)),
        col = alpha("plum2", 0.25),
        border = NA)

polygon(x = c(v3[comp3$upper <= 0], rev(v3[comp3$upper <= 0])),
        y = c(comp3$upper[comp3$upper <= 0], rev(comp3$lower[comp3$upper <= 0])),
        col = alpha("plum4", 0.25),
        border = NA)

polygon(x = c(v3[comp3$lower >= 0], rev(v3[comp3$lower >= 0])),
        y = c(comp3$upper[comp3$lower >= 0], rev(comp3$lower[comp3$lower >= 0])),
        col = alpha("plum4", 0.25),
        border = NA)
#abline(h = 0, col = "grey", lwd = 2.5, lty = 4)
legend("topright", legend = c("Non significant difference", "SIgnificant difference"), fill = c("plum2", "plum4"), border = NA, bty = "n")

# nest.dens
# comp4 <- smooth_diff(lmgGamm10, newD4, 'crash', 'noCrash', 'lmg.crash')
# 
# plot(v2,
#      comp4$diff,
#      ylim = c(min(comp4$lower), max(comp4$upper)),
#      type = "l",
#      bty = "n",
#      lwd = 2.5,
#      xlab = "nest.dens",
#      ylab = "difference btw crash/noCrash smooth",
#      col = "forestgreen")
# 
# polygon(x = c(v2, rev(v2)),
#         y = c(comp4$upper, rev(comp4$lower)),
#         col = alpha("forestgreen", 0.25),
#         border = NA)
# abline(h = 0, col = "grey", lwd = 2.5, lty = 4)

### ------------- ###
#### Paper plots ####
### ------------- ###

# Temperatures #

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_2_temp.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
# par(mfrow = c(1, 2))

# --- #

plot(v,
     newD1$tranFit[newD1$lmg.crash == "crash"],
     ylim = c(min(newD1$minIC), max(newD1$maxIC)+2),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Maximal temperature (°C)",
     ylab = "Number of fox attacks per hour",
     col = "darkorange1")
lines(v,
      newD1$tranFit[newD1$lmg.crash == "noCrash"],
      lwd = 2.5,
      col = "darkorange4")
legend("topleft",
  #x = 7,
  #y = 12.0,
  legend = c("High lemming abundance", "Low lemming abundance"), fill = c("darkorange4", "darkorange1"), border = NA, bty = "n")
#legend("topleft", "(a)", bty = "n")


# points(k$res$max.temp[k$res$lmg.crash == "crash"],
#        k$res$visregRes[k$res$lmg.crash == "crash"],
#        col = "darkorange4",
#        pch = ".",
#        cex = 2)
# points(k$res$max.temp[k$res$lmg.crash == "noCrash"],
#        k$res$visregRes[k$res$lmg.crash == "noCrash"],
#        col = "darkorange3",
#        pch = ".",
#        cex = 2)

polygon(x = c(v, rev(v)),
        y = c(newD1$minIC[newD1$lmg.crash == "crash"], rev(newD1$maxIC[newD1$lmg.crash == "crash"])),
        col = alpha("darkorange1", 0.25),
        border = NA)

polygon(x = c(v, rev(v)),
        y = c(newD1$minIC[newD1$lmg.crash == "noCrash"], rev(newD1$maxIC[newD1$lmg.crash == "noCrash"])),
        col = alpha("darkorange4", 0.25),
        border = NA)
# --- #

# plot(v[comp1$upper >= 0],
#      comp1$diff[comp1$upper >= 0],
#      ylim = c(min(comp1$lower), 2),
#      xlim = c(min(v), max(v)),
#      type = "l",
#      bty = "n",
#      lwd = 2.5,
#      xlab = "Maximal temperature (°C)",
#      ylab = "Difference between the fitted trends",
#      col = "darkgreen")
# 
# lines(v[comp1$upper < 0],
#       comp1$diff[comp1$upper < 0],
#       ylim = c(min(comp1$lower), max(comp1$upper)),
#       lwd = 2.5,
#       col = "red")
# 
# polygon(x = c(v[comp1$upper >= 0], rev(v[comp1$upper >= 0])),
#         y = c(comp1$upper[comp1$upper >= 0], rev(comp1$lower[comp1$upper >= 0])),
#         col = alpha("darkgreen", 0.25),
#         border = NA)
# polygon(x = c(v[comp1$upper < 0], rev(v[comp1$upper < 0])),
#         y = c(comp1$upper[comp1$upper < 0], rev(comp1$lower[comp1$upper < 0])),
#         col = alpha("red", 0.25),
#         border = NA)
# abline(v = v[comp1$upper < 0][1], col = "darkgrey", lwd = 2, lty = "dotdash")
# 
# legend("bottomleft", legend = c("Non significant difference", "Significant difference"), fill = c("darkgreen", "red"), border = NA, bty = "n")
# legend("topleft", "(b)", bty = "n")
# 

# --- #
#dev.off()

# Precipitations #

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_2_prec.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
# par(mfrow = c(1, 2))

# ----- #
#l <- visreg(lmgGamm10, "prec", "lmg.crash", plot = F)

plot(v1,
     newD2$tranFit[newD2$lmg.crash == "crash"],
     ylim = c(min(newD2$minIC), max(newD2$maxIC)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Cumulative precipitation (mm)",
     ylab = "Number of fox attacks per hour",
     col = "skyblue2")
lines(v1,
      newD2$tranFit[newD2$lmg.crash == "noCrash"],
      lwd = 2.5,
      col = "skyblue4")
legend(x = 1, y = 14, legend = c("High lemming abundance", "Low lemming abundance"), fill = c("skyblue4", "skyblue2"), border = NA, bty = "n")

legend("topleft", bty = "n", "(a)")

polygon(x = c(v1, rev(v1)),
        y = c(newD2$minIC[newD2$lmg.crash == "crash"], rev(newD2$maxIC[newD2$lmg.crash == "crash"])),
        col = alpha("skyblue2", 0.25),
        border = NA)

polygon(x = c(v1, rev(v1)),
        y = c(newD2$minIC[newD2$lmg.crash == "noCrash"], rev(newD2$maxIC[newD2$lmg.crash == "noCrash"])),
        col = alpha("skyblue4", 0.25),
        border = NA)
# --- #

plot(v1,
     comp2$diff,
     ylim = c(min(comp2$lower), max(comp2$upper) + 1),
     xlim = c(min(v1), max(v1)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "prec",
     ylab = "difference btw low/high smooth",
     col = "darkgreen")
lines(v1[comp2$upper <= 0],
      comp2$diff[comp2$upper <= 0],
      lwd = 2.5,
      col = "red")

lines(v1[comp2$lower >= 0],
      comp2$diff[comp2$lower >= 0],
      lwd = 2.5,
      col = "red")

polygon(x = c(v1, rev(v1)),
        y = c(comp2$upper, rev(comp2$lower)),
        col = alpha("darkgreen", 0.25),
        border = NA)

polygon(x = c(v1[comp2$upper <= 0], rev(v1[comp2$upper <= 0])),
        y = c(comp2$upper[comp2$upper <= 0], rev(comp2$lower[comp2$upper <= 0])),
        col = alpha("red", 0.25),
        border = NA)

polygon(x = c(v1[comp2$lower >= 0], rev(v1[comp2$lower >= 0])),
        y = c(comp2$upper[comp2$lower >= 0], rev(comp2$lower[comp2$lower >= 0])),
        col = alpha("red", 0.25),
        border = NA)
abline(v = v1[comp2$upper < 0][1], col = "darkgrey", lwd = 2, lty = "dotdash")
abline(v = v1[comp2$lower > 0][length(v1[comp2$lower > 0])], col = "darkgrey", lwd = 2, lty = "dotdash")

legend("topright", legend = c("Non significant difference", "Significant difference"), fill = c("darkgreen", "red"), border = NA, bty = "n")

legend("topleft", bty = "n", "(b)")

# --- #
#dev.off()

# REL.DATEs #

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_2_REL.DATE.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
#  par(mfrow = c(1, 2))

# --- #
#m <- visreg(lmgGamm10, "REL.DATE", "lmg.crash", plot = F)

plot(v3,
     newD3$tranFit[newD3$lmg.crash == "crash"],
     ylim = c(min(newD3$minIC), max(newD3$maxIC)+10),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Relative date",
     ylab = "Number of fox attacks per hour",
     col = "plum2")
lines(v3,
      newD3$tranFit[newD3$lmg.crash == "noCrash"],
      lwd = 2.5,
      col = "plum4")
legend(x = 0, y = max(newD3$maxIC), legend = c("High lemming abundance", "Low lemming abundance"), fill = c("plum4", "plum2"), border = NA, bty = "n")

legend("topleft", "(a)", bty = "n", adj = c(1, 0))

polygon(x = c(v3, rev(v3)),
        y = c(newD3$minIC[newD3$lmg.crash == "crash"], rev(newD3$maxIC[newD3$lmg.crash == "crash"])),
        col = alpha("plum2", 0.25),
        border = NA)

polygon(x = c(v3, rev(v3)),
        y = c(newD3$minIC[newD3$lmg.crash == "noCrash"], rev(newD3$maxIC[newD3$lmg.crash == "noCrash"])),
        col = alpha("plum4", 0.25),
        border = NA)
# --- #
plot(v3,
     comp3$diff,
     ylim = c(min(comp3$lower), max(comp3$upper)),
     xlim = c(min(v3), max(v3)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Relative date",
     ylab = "difference btw low/high smooth",
     col = "darkgreen")

lines(v3[comp3$upper <= 0],
      comp3$diff[comp3$upper <= 0],
      lwd = 2.5,
      col = "red")

lines(v3[comp3$lower >= 0],
      comp3$diff[comp3$lower >= 0],
      lwd = 2.5,
      col = "red")

polygon(x = c(v3, rev(v3)),
        y = c(comp3$upper, rev(comp3$lower)),
        col = alpha("darkgreen", 0.25),
        border = NA)

polygon(x = c(v3[comp3$upper <= 0], rev(v3[comp3$upper <= 0])),
        y = c(comp3$upper[comp3$upper <= 0], rev(comp3$lower[comp3$upper <= 0])),
        col = alpha("red", 0.25),
        border = NA)

polygon(x = c(v3[comp3$lower >= 0], rev(v3[comp3$lower >= 0])),
        y = c(comp3$upper[comp3$lower >= 0], rev(comp3$lower[comp3$lower >= 0])),
        col = alpha("red", 0.25),
        border = NA)

abline(v = v3[comp3$upper <= 0][1], col = "darkgrey", lwd = 2, lty = "dotdash")
abline(v = v3[comp3$lower >= 0][1], col = "darkgrey", lwd = 2, lty = "dotdash")
abline(v = v3[comp3$lower >= 0][length(v3[comp3$lower >= 0])], col = "darkgrey", lwd = 2, lty = "dotdash")


legend(x = -6, y = -3, legend = c("Non significant difference", "Significant difference"), fill = c("darkgreen", "red"), border = NA, bty = "n")

legend("topleft", "(b)", bty = "n", adj = c(1, 0))
# --- #
#dev.off()

# Goose nest density #

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_2_nest_dens.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

# --- #

#n <- visreg(lmgGamm10, "nest.dens", "lmg.crash", plot = F)

plot(v2,
     newD5$tranFit,
     ylim = c(min(newD5$minIC), max(newD5$maxIC) + 1),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Goose nest density (nb/ha)",
     ylab = "Number of fox attacks per hour",
     col = "forestgreen")

polygon(x = c(v2, rev(v2)),
        y = c(newD5$minIC, rev(newD5$maxIC)),
        col = alpha("forestgreen", 0.25),
        border = NA)

# --- #
#dev.off()

### ------------------- ###
#### Paper appendices ####
### ----------------- ###

### Temp - partial residuals
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_Annexe_temp.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

visreg(lmgGamm10,
       "max.temp",
       "lmg.crash",
       overlay = T,
       scale = "linear",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "Maximal temperature (°C)",
       ylab = "f(Maximal temperature)",
       gg = F,
       fill.par = list(col = c(alpha("darkorange1", 0.25), alpha("darkorange4", 0.25))),
       line = list(col = c("darkorange1", "darkorange4")),
       points = list(col = c(alpha("darkorange1", 0.7), alpha("darkorange4", 0.7)), cex = 1),
       legend = F)
legend("topright", legend = c("High lemming abundance", "Low lemming abundance"), fill = c("darkorange4", "darkorange1"), border = NA, bty = "n")

#dev.off()

### Prec - partial residuals
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_Annexe_prec.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

visreg(lmgGamm10,
       "prec",
       "lmg.crash",
       overlay = T,
       scale = "linear",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "Cumulative precipitation (mm)",
       ylab = "f(Cumulative precipitation)",
       gg = F,
       fill.par = list(col = c(alpha("skyblue3", 0.25), alpha("skyblue4", 0.25))),
       line = list(col = c("skyblue3", "skyblue4")),
       points = list(col = c(alpha("skyblue3", 0.7), alpha("skyblue4", 0.7)), cex = 1),
       legend = F)
legend("topright", legend = c("High lemming abundance", "Low lemming abundance"), fill = c("skyblue4", "skyblue3"), border = NA, bty = "n")

#dev.off()

### REL.DATE - partial residuals
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_Annexe_REL.DATE.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

visreg(lmgGamm10,
       "REL.DATE",
       "lmg.crash",
       overlay = T,
       scale = "linear",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "REL.DATE (Julian REL.DATE)",
       ylab = "f(REL.DATE)",
       gg = F,
       fill.par = list(col = c(alpha("plum3", 0.35), alpha("plum4", 0.35))),
       line = list(col = c("plum3", "plum4")),
       points = list(col = c(alpha("plum3", 0.7), alpha("plum4", 0.7)), cex = 1),
       legend = F)
legend("topright", legend = c("High lemming abundance", "Low lemming abundance"), fill = c("plum4", "plum3"), border = NA, bty = "n")

#dev.off()

### Nest density - partial residuals
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Gamm_Annexe_nestdens.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

visreg(lmgGamm10,
       "nest.dens",
       scale = "linear",
       cond = list(log.obs = log(3600)),
       jitter = T,
       bty = "n",
       xlab = "Goose nest density (nb/ha)",
       ylab = "f(Goose nest density)",
       gg = F,
       fill.par = list(col = alpha("forestgreen", 0.25)),
       line = list(col = "forestgreen"),
       points = list(col = alpha("forestgreen", 0.35), cex = 1),
       legend = F)

#dev.off()

#### Plot with random effects ####
data_peak <- data_test[data_test$lmg.crash == "noCrash",]
data_crash <- data_test[data_test$lmg.crash == "crash",]

#####  TEMPERATURE ####
# 
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Ann_Random_eff_temp.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

par(mar=c(4.1, 4.1, 0.2, 0.2), mfrow=c(1,2))
## Temp - lmg peak
plot(v,
     newD1$tranFit[newD1$lmg.crash == "noCrash"],
     ylim = c(min(newD1$minIC), max(newD1$maxIC)+2),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Maximal temperature (°C)",
     ylab = "Number of fox attacks per hour",
     col = "darkorange4")

polygon(x = c(v, rev(v)),
        y = c(newD1$minIC[newD1$lmg.crash == "noCrash"], rev(newD1$maxIC[newD1$lmg.crash == "noCrash"])),
        col = alpha("darkorange4", 0.25),
        border = NA)

re <- as.vector(unique(data_peak$fox.year))
for(i in re){
  ND1 <- data.frame(max.temp = v,
                    prec = mean(data_peak$prec),
                    nest.dens = mean(data_peak$nest.dens),
                    REL.DATE = mean(data_peak$REL.DATE),
                    lmg.crash = "noCrash",
                    log.obs = log(3600),
                    fox.year = i)
  
  ND1$tranFit <- predict(lmgGamm10, newdata = ND1, type = "response", se.fit = FALSE)
  
  
  lines(ND1$max.temp,
        ND1$tranFit,
        ype = "l",
        lwd = 1,
        col = alpha("darkorange4", 0.25))
  
}
legend("topleft", "(a)", bty = "n", adj = c(1, 0))



## Temp - lmg crash
par(mar = c(4.1, 0.1, 0.1, 0.1))
plot(v,
     newD1$tranFit[newD1$lmg.crash == "crash"],
     ylim = c(min(newD1$minIC), max(newD1$maxIC)+2),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Maximal temperature (°C)",
     ylab = "",
     yaxt = "n",
     col = "darkorange1")

polygon(x = c(v, rev(v)),
        y = c(newD1$minIC[newD1$lmg.crash == "crash"], rev(newD1$maxIC[newD1$lmg.crash == "crash"])),
        col = alpha("darkorange1", 0.25),
        border = NA)

re <- as.vector(unique(data_crash$fox.year))
for(i in re){
  ND1 <- data.frame(max.temp = v,
                    prec = mean(data_crash$prec),
                    nest.dens = mean(data_crash$nest.dens),
                    REL.DATE = mean(data_crash$REL.DATE),
                    lmg.crash = "crash",
                    log.obs = log(3600),
                    fox.year = i)
  
  
  ND1$tranFit <- predict(lmgGamm10, newdata = ND1, type = "response", se.fit = FALSE)
  
  
  lines(ND1$max.temp,
        ND1$tranFit,
        ype = "l",
        lwd = 1,
        col = alpha("darkorange4", 0.25))
  
}
legend("topleft", "(b)", bty = "n", adj = c(1, 0))

#dev.off()

#####  PRECIPITATION ####

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Ann_Random_eff_prec.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

par(mar=c(4.1, 4.1, 0.2, 0.2), mfrow=c(1,2))
## prec - lmg peak
plot(v1,
     newD2$tranFit[newD2$lmg.crash == "noCrash"],
     ylim = c(min(newD2$minIC), max(newD2$maxIC)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Cumulative precipitation (mm)",
     ylab = "Number of fox attacks per hour",
     col = "skyblue4")

polygon(x = c(v1, rev(v1)),
        y = c(newD2$minIC[newD2$lmg.crash == "noCrash"], rev(newD2$maxIC[newD2$lmg.crash == "noCrash"])),
        col = alpha("skyblue4", 0.25),
        border = NA)

re <- as.vector(unique(data_peak$fox.year))
for(i in re){
  ND2 <- data.frame(max.temp = mean(data_peak$max.temp),
                    prec = v1,
                    nest.dens = mean(data_peak$nest.dens),
                    REL.DATE = mean(data_peak$REL.DATE),
                    lmg.crash = "noCrash",
                    log.obs = log(3600),
                    fox.year = i)
  
  ND2$tranFit <- predict(lmgGamm10, newdata = ND2, type = "response", se.fit = FALSE)
  
  
  lines(ND2$prec,
        ND2$tranFit,
        ype = "l",
        lwd = 1,
        col = alpha("skyblue4", 0.25))
  
}
legend("topleft", "(a)", bty = "n", adj = c(1, 0))



## Prec - lmg crash
par(mar = c(4.1, 0.1, 0.1, 0.1))

plot(v1,
     newD2$tranFit[newD2$lmg.crash == "crash"],
     ylim = c(min(newD2$minIC), max(newD2$maxIC)),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Cumulative precipitation (mm)",
     ylab = "",
     yaxt = "n",
     col = "skyblue3")

polygon(x = c(v1, rev(v1)),
        y = c(newD2$minIC[newD2$lmg.crash == "crash"], rev(newD2$maxIC[newD2$lmg.crash == "crash"])),
        col = alpha("skyblue3", 0.25),
        border = NA)

re <- as.vector(unique(data_crash$fox.year))
for(i in re){
  ND2 <- data.frame(max.temp = mean(data_crash$max.temp),
                    prec = v1,
                    nest.dens = mean(data_crash$nest.dens),
                    REL.DATE = mean(data_crash$REL.DATE),
                    lmg.crash = "crash",
                    log.obs = log(3600),
                    fox.year = i)
  
  ND2$tranFit <- predict(lmgGamm10, newdata = ND2, type = "response", se.fit = FALSE)
  
  
  lines(ND2$prec,
        ND2$tranFit,
        ype = "l",
        lwd = 1,
        col = alpha("skyblue3", 0.25))
  
}
legend("topleft", "(b)", bty = "n", adj = c(1, 0))

#dev.off()


#####  REL.DATE ####

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Ann_Random_eff_REL.DATE.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

par(mar=c(4.1, 4.1, 0.2, 0.2), mfrow=c(1,2))
## REL.DATE - lmg peak
# plot(v3,
#      newD3$tranFit[newD3$lmg.crash == "noCrash"],
#      ylim = c(0, 85),
#      type = "l",
#      bty = "n",
#      lwd = 2.5,
#      xlab = "REL.DATE (Julian REL.DATE)",
#      ylab = "Number of fox attacks per hour",
#      col = "plum4")
# 
# polygon(x = c(v3, rev(v3)),
#         y = c(newD3$minIC[newD3$lmg.crash == "noCrash"], rev(newD3$maxIC[newD3$lmg.crash == "noCrash"])),
#         col = alpha("plum4", 0.25),
#         border = NA)
# 
# re <- as.vector(unique(data_peak$fox.year))
# for(i in re){
#   ND3 <- data.frame(max.temp = mean(data_peak$max.temp),
#                     prec = mean(data_peak$prec),
#                     nest.dens = mean(data_peak$nest.dens),
#                     REL.DATE = v3,
#                     lmg.crash = "noCrash",
#                     log.obs = log(3600),
#                     fox.year = i)
#   
#   ND3$tranFit <- predict(lmgGamm10, newdata = ND3, type = "response", se.fit = FALSE)
#   
#   
#   lines(ND3$REL.DATE,
#         ND3$tranFit,
#         ype = "l",
#         lwd = 1,
#         col = alpha("plum4", 0.25))
#   
# }
# legend("topleft", "(a)", bty = "n", adj = c(1, 0))
# 
# 
# 
# ## Prec - lmg crash
# par(mar = c(4.1, 0.1, 0.1, 0.1))
# 
# plot(v3,
#      newD3$tranFit[newD3$lmg.crash == "crash"],
#      ylim = c(0, 85),
#      type = "l",
#      bty = "n",
#      lwd = 2.5,
#      xlab = "REL.DATE (Julian REL.DATE)",
#      ylab = "",
#      yaxt = "n",
#      col = "plum3")
# 
# polygon(x = c(v3, rev(v3)),
#         y = c(newD3$minIC[newD3$lmg.crash == "crash"], rev(newD3$maxIC[newD3$lmg.crash == "crash"])),
#         col = alpha("plum3", 0.25),
#         border = NA)
# 
# re <- as.vector(unique(data_crash$fox.year))
# for(i in re){
#   ND3 <- data.frame(max.temp = mean(data_crash$max.temp),
#                     prec = mean(data_crash$prec),
#                     nest.dens = mean(data_crash$nest.dens),
#                     REL.DATE = v3,
#                     lmg.crash = "crash",
#                     log.obs = log(3600),
#                     fox.year = i)
#   
#   ND3$tranFit <- predict(lmgGamm10, newdata = ND3, type = "response", se.fit = FALSE)
#   
#   
#   lines(ND3$REL.DATE,
#         ND3$tranFit,
#         ype = "l",
#         lwd = 1,
#         col = alpha("plum3", 0.25))
#   
# }
# legend("topleft", "(b)", bty = "n", adj = c(1, 0))

#dev.off()

#####  Goose nest density ####
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Fox predation & climate variables/FOX PRED PAPER/Figures paper/FOX_PAPER_Ann_Random_eff_nestdens.tiff",
#     res=300,
#     width=30,
#     height= 20,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

plot(v2,
     newD5$tranFit[newD5$lmg.crash == "noCrash"],
     ylim = c(0, 40),
     type = "l",
     bty = "n",
     lwd = 2.5,
     xlab = "Goose nest density (nb/ha)",
     ylab = "Number of fox attacks per hour",
     col = "forestgreen")

polygon(x = c(v2, rev(v2)),
        y = c(newD5$minIC, rev(newD5$maxIC)),
        col = alpha("forestgreen", 0.25),
        border = NA)

re <- as.vector(unique(data_test$fox.year))
for(i in re){
  ND5 <- data.frame(max.temp = mean(data_test$max.temp),
                    prec = mean(data_test$prec),
                    nest.dens = v2,
                    REL.DATE = mean(data_test$REL.DATE),
                    lmg.crash = "noCrash",
                    log.obs = log(3600),
                    fox.year = i)
                    
                    ND5$tranFit <- predict(lmgGamm10, newdata = ND5, type = "response", se.fit = FALSE)
                    
                    
                    lines(ND5$nest.dens,
                          ND5$tranFit,
                          ype = "l",
                          lwd = 1,
                          col = alpha("forestgreen", 0.25))
}

#dev.off()
graphics.off()
