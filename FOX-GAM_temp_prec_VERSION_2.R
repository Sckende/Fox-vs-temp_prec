rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
data <- read.table("FOX-functional response V2.txt", h = T, sep = ",", dec = ".")
summary(data)

#### WARNINGS !!! Need to change lemming index with the corrected one !

library(mgcv)

# Data visualization
plot(data$max_temp, data$atq_rate, bty = "n")
plot(data$cumul_prec, data$atq_rate, bty = "n")
plot(data$lmg_C1, data$atq_rate, bty = "n")
plot(data$year, data$atq_rate, bty = "n")

atq.mod <- list()

atq.mod[[1]] <- gam(atq_rate ~ s(max_temp),
                     data = data)
atq.mod[[2]] <- gam(atq_rate ~ s(cumul_prec),
                    data = data)
m1 <- gam(atq_rate ~ s(max_temp), data = data, method = "REML")
summary(m1)
coef(m1)
plot(atq.mod[[1]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
plot(atq.mod[[2]], residuals = TRUE, se = TRUE,  pch = 1, bty = "n", ylim = c(-0.004, 0.010))
summary(atq.mod[[2]])
coef(atq.mod[[2]])
