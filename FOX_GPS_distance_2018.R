setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

# Fox data
dist <- read.table("FOX_GPS_distance_2018.txt", h = T, sep = ";")
head(dist)
summary(dist)
utils::View(dist)
# Temperature data

temp <- read.table("TEMP-PondInlet-1_16_JUIL_2018.txt", h = T, sep = "\t", dec = ",")
head(temp)

# Rainfall data
prec <- read.table("PREC_Bylot_2018.txt", h = T, sep = "\t")
head(prec)

# Exploration data
par(mfrow = c(1, 2))
tapply(dist$dist_km, dist$sex, hist)
levels(dist$sex)

par(mfrow = c(1, 1))
hist(dist$dist_km[dist$sex == "F"],col=scales::alpha('red', .5),border=F, breaks = 20)
hist(dist$dist_km[dist$sex == "M"],xlim=c(0,100),col=scales::alpha('skyblue',.5),border=F, breaks = 10, add=T)
legend("topleft", legend = c("female", "male"), col = c(scales::alpha("red", .5), scales::alpha("skyblue", .5)), bty = "n", pch = 18)

# Merging dataframes

head(temp)
head(prec)
head(dist)

daily_temp <- tapply(temp$Temp, temp$Jour, mean)
daily_temp
utils::View(temp)
