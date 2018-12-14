setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
dist <- read.table("FOX_GPS_distance_2018.txt", h = T, sep = ";")
head(dist)
summary(dist)

utils::View(dist)
par(mfrow = c(1, 2))
tapply(dist$dist_km, dist$sex, hist)
levels(dist$sex)


hist(dist$dist_km[dist$sex == "M"],xlim=c(0,100),col='skyblue',border=F, breaks = 10)
hist(dist$dist_km[dist$sex == "F"],add=T,col=scales::alpha('red',.5),border=F, breaks = 20)
