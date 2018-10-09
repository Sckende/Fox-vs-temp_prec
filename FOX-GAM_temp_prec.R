getwd()
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
o <- read.table("FOX-functional response V2.txt", h = T, sep = ",", dec = ".")
summary(o)
unique(o$year)


#### Taux d'attaque par individu ####

#### Effet de la température ####
# Température moyenne calculée entre la première et la dernière observation de renard

#### Modele 0 - null ####
require(mgcv)
M0 <- gam(o$atq_rate ~ 1) # k pour knots, k=3 recommandé pour n<10
summary(M0)
AIC(M0)
coef(M0)

# Graphic version
col2rgb(col="blue")
par(bg=NA)#fond de graphique transparent
plot(o$mean_temp,o$atq_rate, xlab = "Mean temperature (°c)", ylab = "Mean attack rate per individual",
  xlim = c(3, 7),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(M0, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
gam.check(M0) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Modele 1 - effet température seule ####
#require(mgcv)
M1 <- gam(o$atq_rate ~ s(o$mean_temp, fx = FALSE, k = 6, bs = "cr")) # k pour knots, k=3 recommandé pour n<10
summary(M1)
AIC(M1)
coef(M1)

# Graphic version
col2rgb(col="blue")
par(bg=NA)#fond de graphique transparent
plot(o$mean_temp,o$atq_rate, xlab = "Mean temperature (°c)", ylab = "Mean attack rate per individual",
  xlim = c(3, 7),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(M1, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
gam.check(M1) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Modele 2 - Ajout lemming en variable quantitative ####
#require(mgcv)
M2 <- gam(o$atq_rate ~ s(o$mean_temp, fx = FALSE, k = 6, bs = "cr") + o$lmg_C2) # k pour knots, k=3 recommandé pour n<10
summary(M2)
AIC(M2)
coef(M2)

# Graphic version
dev.off()
col2rgb(col="blue")
par(bg = NA)#fond de graphique transparent
plot(o$mean_temp, o$atq_rate, xlab = "Mean temperature (°c)", ylab = "Mean attack rate per individual",
  xlim = c(3, 7),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(M2, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
gam.check(M2) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Modele 3 - Ajout lemming en variable qualitative ####
#require(mgcv)
M3 <- gam(o$atq_rate ~ s(o$mean_temp, fx = FALSE, k = 6, bs = "cr") + factor(o$lmg_year)) # k pour knots, k=3 recommandé pour n<10
summary(M3)
AIC(M3)
coef(M3)

# Graphic version
dev.off()
col2rgb(col="blue")
par(bg = NA)#fond de graphique transparent
plot(o$mean_temp, o$atq_rate, xlab = "Mean temperature (°c)", ylab = "Mean attack rate per individual",
  xlim = c(3, 7),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(M3, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
dev.off()
gam.check(M3) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Effet de la précipitation ####
# Précipitation cumulée - Entre le premier et le dernier jour d'observation des reanrds

#### Modele 1 - effet précipitation seule ####
#require(mgcv)
P1 <- gam(o$atq_rate ~ s(o$cumul_prec, fx = FALSE, k = 6, bs = "cr")) # k pour knots, k=3 recommandé pour n<10
summary(P1)
AIC(P1)
coef(P1)

# Graphic version
dev.off()
col2rgb(col="blue")
par(bg=NA)#fond de graphique transparent
plot(o$cumul_prec,o$atq_rate, xlab = "Cumulative precipitation (mm)", ylab = "Mean attack rate per individual",
  xlim = c(0, 115),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(P1, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
gam.check(P1) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Modele 1 a - Idem mais sans valeurs extrêmes de précipitation ####
# Retrait de l'année 2005
#require(mgcv)
o2005 <- o[!o$year == 2005, ]
P1a <- gam(o2005$atq_rate ~ s(o2005$cumul_prec, fx = FALSE, k = 6, bs = "cr")) # k pour knots, k=3 recommandé pour n<10
summary(P1a)
AIC(P1a)
coef(P1a)

# Graphic version
dev.off()
col2rgb(col="blue")
par(bg=NA)#fond de graphique transparent
plot(o2005$cumul_prec, o2005$atq_rate, xlab = "Cumulative precipitation (mm)", ylab = "Mean attack rate per individual",
  xlim = c(0, 60),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(P1a, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
gam.check(P1a) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Modele 2 - ajout lemming en variable quantitative ####
#require(mgcv)
P2 <- gam(o$atq_rate ~ s(o$cumul_prec, fx = FALSE, k = 6, bs = "cr") + o$lmg_C2) # k pour knots, k=3 recommandé pour n<10
summary(P2)
AIC(P2)
coef(P2)

# Graphic version
dev.off()
col2rgb(col="blue")
par(bg=NA)#fond de graphique transparent
plot(o$cumul_prec,o$atq_rate, xlab = "Cumulative precipitation (mm)", ylab = "Mean attack rate per individual",
  xlim = c(0, 115),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(P2, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
gam.check(P2) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Modele 3 - ajout lemming en variable qualitative ####
#require(mgcv)
P3 <- gam(o$atq_rate ~ s(o$cumul_prec, fx = FALSE, k = 6, bs = "cr") + factor(o$lmg_year)) # k pour knots, k=3 recommandé pour n<10
summary(P3)
AIC(P3)
coef(P3)

# Graphic version
dev.off()
col2rgb(col="blue")
par(bg=NA)#fond de graphique transparent
plot(o$cumul_prec,o$atq_rate, xlab = "Cumulative precipitation (mm)", ylab = "Mean attack rate per individual",
  xlim = c(0, 115),ylim = c(0, 0.012), col = "blue", lwd = 4, cex.axis = 1.8, pch = 16, cex = 2) # nuage de points
par(new = T) # pour superposition d'un nouveau graphique
par(bg = NA)
couleur <- rgb(0, 0, 0.5, 0.2) # définition d'une couleur pour l'intervalle de confiance
plot.gam(P3, shade = T, shade.col = couleur, pers = T, xlab = "", yaxt = "n", ylab = "", xaxt = "n", lwd = 4) # plot de la smooth curve et de l'intervalle de confiance à 95% associée
dev.off()

# Evaluation des résidus
gam.check(P3) # évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#### Intéressant de regarder aussi taux d'attaque par jour avec association de température et précipitation au même jour ####