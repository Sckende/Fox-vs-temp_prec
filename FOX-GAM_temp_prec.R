getwd()
setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
o <- read.table("FOX-functional response V2.txt", h = T, sep = ",", dec = ".")
summary(o)
unique(o$year)


#### Taux d'attaque par individu ####
# Température moyenne calculée entre la première et la dernière observation de renard
# Précipitation cumulée - Idem