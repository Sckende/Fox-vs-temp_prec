####### Calcul des moyennes d'attaque PAR INDIVIDU PAR OBSERVATION ######
#### Tentative de partir des fichiers brutes de time budget ####

getwd ()
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm( list = ls ())

######################## Fox - 2004 ###########################
# Loading database
rf<-read.table("Fox-2004-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
head(rf)

# Checking ID levels
levels(rf$ID)

# Checking behaviours levels
levels(rf$Behavior)

# Split database depending on Date, ID and, Obs_length
data <- split(rf, paste(rf$Date, rf$ID, rf$Obs_lenght))

# Computation of "all_atq_rate" = atq rate per obs per ind for all items (lemming, goose, eggs, and gosling)
levels(rf$Item[rf$Behavior == "attaque"]) # keep items: couple, egg, lemming, oie, young

data <- lapply(data, function(x){
  for(i in 1:length(x$Behavior)){
    if(x$Behavior[i] == "attaque" & x$Item[i] %in% c("couple", "egg", "lemming", "oie", "young")){
        x$all_atq[i] <- 1
      } else {
        x$all_atq[i] <- 0
    }
  }
  x$all_atq_rate <- sum(x$all_atq)/x$Obs_lenght[1]  
  x
})

# Computation of "goo_atq_rate" = atq rate per obs per ind for goose items (goose, eggs, and gosling)
levels(rf$Item[rf$Behavior == "attaque"]) # keep items: couple, egg, oie, young

data <- lapply(data, function(x){
  for(i in 1:length(x$Behavior)){
    if(x$Behavior[i] == "attaque" & x$Item[i] %in% c("couple", "egg", "oie", "young")){
      x$goo_atq[i] <- 1
    } else {
      x$goo_atq[i] <- 0
      }
  }
  x$goo_atq_rate <- sum(x$goo_atq)/x$Obs_lenght[1]
  x
})

FOX_ATQ <- do.call("rbind", data)
head(FOX_ATQ)

######################## Fox - 2005 ###########################
# Loading database
rf<-read.table("Fox-2005-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
head(rf)

# Checking ID levels
levels(rf$ID)

# Checking behaviours levels
levels(rf$Behavior)

# Split database depending on Date, ID and, Obs_length
data <- split(rf, paste(rf$Date, rf$ID, rf$Obs_lenght))

# Computation of "all_atq_rate" = atq rate per obs per ind for all items (lemming, goose, eggs, and gosling)
levels(rf$Item[rf$Behavior == "attaque"]) # keep items: couple, egg, lemming, oie, young
table(rf$Item[rf$Behavior == "attaque"])

data <- lapply(data, function(x){
  for(i in 1:length(x$Behavior)){
    if(x$Behavior[i] == "attaque" & x$Item[i] %in% c("couple", "egg", "lemming", "oie", "young")){
      x$all_atq[i] <- 1
    } else {
      x$all_atq[i] <- 0
    }
  }
  x$all_atq_rate <- sum(x$all_atq)/x$Obs_lenght[1]  
  x
})

# Computation of "goo_atq_rate" = atq rate per obs per ind for goose items (goose, eggs, and gosling)
levels(rf$Item[rf$Behavior == "attaque"]) # keep items: couple, egg, oie, young

data <- lapply(data, function(x){
  for(i in 1:length(x$Behavior)){
    if(x$Behavior[i] == "attaque" & x$Item[i] %in% c("couple", "egg", "oie", "young")){
      x$goo_atq[i] <- 1
    } else {
      x$goo_atq[i] <- 0
    }
  }
  x$goo_atq_rate <- sum(x$goo_atq)/x$Obs_lenght[1]
  x
})

rf <- do.call("rbind", data)
FOX_ATQ <- rbind(FOX_ATQ, rf) # See for the warning message








######################## Fox - 2005 ###########################
rf<-read.table("Fox-2005-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)

# Récupération de la durée totale des observations par individu
TAB <- NULL
TAB1 <- NULL

for (j in unique(rf$ID)){
  for (i in unique(rf$Date[rf$ID == j])) {#pour diminuer le risque de perdre une 2 durées d'observation identiques pour un même individu
    id <- j
    date <- i
    obs<-sum(unique(rf$Obs_lenght[rf$ID==j & rf$Date == i]))
    
    di <-data.frame(id, date, obs)
    TAB <- rbind(TAB, di)
    
  }}
print(TAB)

for (k in unique(TAB$id)) {
  fox <- k
  tot_obs <- sum(TAB$obs[TAB$id == k])
  bo <- data.frame(fox, tot_obs)
  TAB1 <- rbind(TAB1, bo)
}
print(TAB1)

# Récupération du nombre d'attaque sur oie par individu
levels(rf$Behavior); levels(rf$Item)
rf1<-subset(rf, rf$Behavior == "attaque" & rf$Item == c("couple", "egg", "oie", "young")) #subset uniquement avec les compt d'attaque et les items associés aux oies
rf1<-droplevels(rf1) #retirer les levels == à 0
summary(rf1)

#Nombre d'attaque total par individu
SUM_atq <- table(rf1$ID, rf1$Behavior)
SUM_atq <- as.data.frame(SUM_atq); names(SUM_atq) <- c("ID", "Behaviour", "atq")
SUM_atq

ff <- cbind(rep(2005, dim(TAB1)[1]),rep(min(TAB$date), dim(TAB1)[1]),rep(max(TAB$date), dim(TAB1)[1]),TAB1, SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
names(ff) <- c("year","min_date", "max_date",names(TAB1), "tot_atq")

fox_fonc <- rbind(fox_fonc, ff)

######################## Fox - 1996-1999 ###########################
rf<-read.table("Fox-1996-1999-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
rf$ID <- as.factor(rf$ID)
levels(rf$ID)
rf$PROIE[is.na(rf$PROIE)] <- 0

# Création d'une nouvelle variable Behavior 
for (x in 1:nrow(rf)) {
  if (rf$ATA[x] == 1) {
    rf$Behavior[x] <- "attaque" 
  } else {
    rf$Behavior[x] <- "autre"
  }
}

# Création d'une nouvelle variable Item
for (x in 1:nrow(rf)) {
  if (rf$PROIE[x] == 0) {
    rf$Item[x] <- 0
  } 
  if (rf$PROIE[x] == 1){
    rf$Item[x] <- "oie"
  } 
  if (rf$PROIE[x] == 2){
    rf$Item[x] <- "lemming"
  }
}

rf$Behavior <- as.factor(rf$Behavior)
rf$Item <- as.factor(rf$Item)

# Conversion de la durée d'observation

rf$long_obs <- strptime(rf$long_obs, format = "%H:%M")
rf <- na.omit(rf) #ne fonctionne pas, je sais pas pourquoi
rf <- rf[-c(69,70),] #retrait des deux lignes avec NA pour la longueur observation

require(lubridate)

for (x in 1:nrow(rf)) {
  if (hour(rf$long_obs[x]) == 1) {
    rf$Obs_lenght[x] <- hour(rf$long_obs[x])*3600 + minute(rf$long_obs[x])*60
  } else {
    rf$Obs_lenght[x] <- minute(rf$long_obs[x])*60
  }
}

summary(rf)

# Récupération de la durée totale des observations par individu
TAB <- NULL
TAB1 <- NULL
for (p in unique(rf$Year)){
for (j in unique(rf$ID[rf$Year == p])){
  for (i in unique(rf$Date[rf$ID == j  & rf$Year == p])) {#pour diminuer le risque de perdre une 2 durées d'observation identiques pour un même individu
    Year <- p
    id <- j
    date <- i
    obs<-sum(unique(rf$Obs_lenght[rf$ID==j & rf$Date == i & rf$Year == p]))
    
    di <-data.frame(Year, id, date, obs)
    TAB <- rbind(TAB, di)
    }
  }
}
print(TAB)

for (p in unique(TAB$Year)) {
  for (k in unique(TAB$id[TAB$Year == p])) {
    year <- p
    min_date <- min(TAB$date[TAB$Year == p])
    max_date <- max(TAB$date[TAB$Year == p])
    fox <- k
    tot_obs <- sum(TAB$obs[TAB$id == k & TAB$Year == p])
    bo <- data.frame(year,min_date, max_date, fox, tot_obs)
    TAB1 <- rbind(TAB1, bo)
  }
}
print(TAB1)

# Récupération du nombre d'attaque sur oie par individu
levels(rf$Behavior); levels(rf$Item)
rf1<-subset(rf, rf$Behavior == "attaque" & rf$Item == "oie") #subset uniquement avec les compt d'attaque et les items associés aux oies
rf1<-droplevels(rf1) #retirer les levels == à 0
summary(rf1)

#Nombre d'attaque total par individu

you <- NULL
for (l in unique(rf1$Year)){
 
  SUM_atq <- as.data.frame(table(rf1$ID[rf1$Year == l], rf1$Behavior[rf1$Year == l]))
  names(SUM_atq) <- c("ID", "Behaviour", "atq")
  
  Year <- rep(l,nrow(SUM_atq))
  do <- data.frame(Year, SUM_atq)
  
  you <- rbind(you, do)
} 
print(you)

for (l in unique(TAB1$year)) {
  
  tot_atq <-  you$atq[you$Year == l][match(TAB1$fox[TAB1$year == l],you$ID[you$Year == l])]
  ff <- cbind(TAB1[TAB1$year == l,],tot_atq) 
  
  fox_fonc <- rbind(fox_fonc, ff)
}

######################## Fox - 2015 ###########################
rf<-read.table("Fox-2015-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)

# Récupération de la durée totale des observations par individu
TAB <- NULL
TAB1 <- NULL

for (j in unique(rf$ID)){
  for (i in unique(rf$Date[rf$ID == j])) {#pour diminuer le risque de perdre une 2 durées d'observation identiques pour un même individu
    id <- j
    date <- i
    obs<-sum(unique(rf$Obs_lenght[rf$ID==j & rf$Date == i]))
    
    di <-data.frame(id, date, obs)
    TAB <- rbind(TAB, di)
    
  }}
print(TAB)

for (k in unique(TAB$id)) {
  fox <- k
  tot_obs <- sum(TAB$obs[TAB$id == k])
  bo <- data.frame(fox, tot_obs)
  TAB1 <- rbind(TAB1, bo)
}
print(TAB1)

# Récupération du nombre d'attaque sur oie par individu
levels(rf$Behavior)
rf1<-subset(rf, rf$Behavior == "ATTAQ") #subset uniquement avec les compt d'attaque et les items associés aux oies
rf1<-droplevels(rf1) #retirer les levels == à 0
summary(rf1)

#Nombre d'attaque total par individu
SUM_atq <- table(rf1$ID, rf1$Behavior)
SUM_atq <- as.data.frame(SUM_atq); names(SUM_atq) <- c("ID", "Behaviour", "atq")
SUM_atq

ff <- cbind(rep(2015, dim(TAB1)[1]), rep(min(TAB$date), dim(TAB1)[1]),rep(max(TAB$date), dim(TAB1)[1]),TAB1, SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
names(ff) <- c("year","min_date","max_date", names(TAB1), "tot_atq")
print(ff)

fox_fonc <- rbind(fox_fonc, ff)
print(fox_fonc)


######################## Fox - 2016 ###########################
rf<-read.table("Fox-2016-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)
# conversion des dates en jours juliens
rf$DAY <- strptime(rf$DAY, format = "%Y-%m-%d")
rf$Date <- rf$DAY$yday + 1

# Récupération de la durée totale des observations par individu
TAB <- NULL
TAB1 <- NULL

for (j in unique(rf$ID)){
  for (i in unique(rf$Date[rf$ID == j])) {#pour diminuer le risque de perdre une 2 durées d'observation identiques pour un même individu
    id <- j
    date <- i
    obs<-sum(unique(rf$Obs_lenght[rf$ID==j & rf$Date == i]))
    
    di <-data.frame(id, date, obs)
    TAB <- rbind(TAB, di)
    
  }}
print(TAB)

for (k in unique(TAB$id)) {
  fox <- k
  tot_obs <- sum(TAB$obs[TAB$id == k])
  bo <- data.frame(fox, tot_obs)
  TAB1 <- rbind(TAB1, bo)
}
print(TAB1)

# Récupération du nombre d'attaque sur oie par individu
levels(rf$Behavior)
rf1<-subset(rf, rf$Behavior == "ATTAQ") #subset uniquement avec les compt d'attaque et les items associés aux oies
rf1<-droplevels(rf1) #retirer les levels == à 0
summary(rf1)

#Nombre d'attaque total par individu
SUM_atq <- table(rf1$ID, rf1$Behavior)
SUM_atq <- as.data.frame(SUM_atq); names(SUM_atq) <- c("ID", "Behaviour", "atq")
SUM_atq

ff <- cbind(rep(2016, dim(TAB1)[1]),rep(min(TAB$date), dim(TAB1)[1]),rep(max(TAB$date), dim(TAB1)[1]),TAB1, SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
names(ff) <- c("year", "min_date", "max_date",names(TAB1), "tot_atq")
print(ff)

fox_fonc <- rbind(fox_fonc, ff)
print(fox_fonc)
summary(fox_fonc)

######################## Fox - 2017 ###########################
rf<-read.table("Fox-2017-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)
# conversion des dates en jours juliens
rf$DAY <- strptime(rf$DAY, format = "%Y-%m-%d")
rf$Date <- rf$DAY$yday + 1

# Récupération de la durée totale des observations par individu
TAB <- NULL
TAB1 <- NULL

for (j in unique(rf$ID)){
  for (i in unique(rf$Date[rf$ID == j])) {#pour diminuer le risque de perdre une 2 durées d'observation identiques pour un même individu
    id <- j
    date <- i
    obs<-sum(unique(rf$Obs_lenght[rf$ID==j & rf$Date == i]))
    
    di <-data.frame(id, date, obs)
    TAB <- rbind(TAB, di)
    
  }}
print(TAB)

for (k in unique(TAB$id)) {
  fox <- k
  tot_obs <- sum(TAB$obs[TAB$id == k])
  bo <- data.frame(fox, tot_obs)
  TAB1 <- rbind(TAB1, bo)
}
print(TAB1)

# Récupération du nombre d'attaque sur oie par individu
levels(rf$Behavior)
rf1<-subset(rf, rf$Behavior == "ATTAQ") #subset uniquement avec les compt d'attaque et les items associés aux oies
rf1<-droplevels(rf1) #retirer les levels == à 0
summary(rf1)

#Nombre d'attaque total par individu
SUM_atq <- table(rf1$ID, rf1$Behavior)
SUM_atq <- as.data.frame(SUM_atq); names(SUM_atq) <- c("ID", "Behaviour", "atq")
SUM_atq

ff <- cbind(rep(2016, dim(TAB1)[1]),rep(min(TAB$date), dim(TAB1)[1]),rep(max(TAB$date), dim(TAB1)[1]),TAB1, SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
names(ff) <- c("year", "min_date", "max_date",names(TAB1), "tot_atq")
print(ff)

fox_fonc <- rbind(fox_fonc, ff)
print(fox_fonc)
summary(fox_fonc)


#### Dataframe final #####
# À faire sur le dataframe final de fox_fonc
# retrait des observations = 0
fox_fonc <- fox_fonc[!fox_fonc$tot_obs == 0,]
#remplace les NA par des 0 pour la colone atq
fox_fonc$tot_atq[is.na(fox_fonc$tot_atq)] <- 0
#uniformisation des années
fox_fonc$year[fox_fonc$year == 96] <- 1996
fox_fonc$year[fox_fonc$year == 97] <- 1997
fox_fonc$year[fox_fonc$year == 98] <- 1998
fox_fonc$year[fox_fonc$year == 99] <- 1999
summary(fox_fonc)

#calcul du taux d'attaque par individu
fox_fonc$atq_rate <- fox_fonc$tot_atq/fox_fonc$tot_obs 
summary(fox_fonc)
boxplot(fox_fonc$atq_rate)
plot(fox_fonc$year, fox_fonc$atq_rate)

#outlier
fox_fonc[fox_fonc$atq_rate>=0.04,]

#garder observation supérieure et égale à 3 minutes
ff2 <- fox_fonc[fox_fonc$tot_obs>=180,]
boxplot(ff2$atq_rate)
plot(ff2$year, ff2$atq_rate)
dim(ff2)

#### Ajout des variables biotiques et abiotiques pour analyses de piste ####

####Ajout des autres variables biologiques et météorologiques####
#changement de répertoire
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
fox<-read.csv("Fox_abundance_Chevallier.txt", sep = "\t", dec = ",")
lmg<-read.csv("LEM96-2016.txt", sep = "\t", dec = ",")
AO<-read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
breed<-read.csv("GOOSE_breeding_informations.txt", sep = "\t", dec = ".")

#ajout des données de lmg, AO, démo oies, fox
for(n in 1:nrow(ff2)){
  d<-ff2$year[n]
  ff2$fox_dens[n]<-fox$natal_growth_dens[fox$year == d]
  #fox_dens_timelag<-fox$natal_growth_dens[fox$year == d+ ou - 1]
  ff2$lmg_abun[n]<-lmg$LMG_C2[lmg$YEAR == d]

  ff2$winAO[n]<-AO$winAO[AO$YEAR==d]
  ff2$sprAO[n]<-AO$sprAO[AO$YEAR==d]
  ff2$esumAO[n]<-AO$esumAO[AO$YEAR==d]
  ff2$lsumAO[n]<-AO$lsumAO[AO$YEAR==d]
  
  ff2$nest_density[n]<-breed$NEST_DENSITY[breed$YEAR==d]
  ff2$clutch_size[n]<-breed$CLUTCH_SIZE[breed$YEAR==d]
  ff2$egg_abun[n]<-breed$EGG_ABUN[breed$YEAR==d]
  ff2$ratio_JUVad[n]<-breed$RATIO_YOU_AD[breed$YEAR==d]
  ff2$brood_size[n]<-breed$BROOD_SIZE_BAND[breed$YEAR==d]
  ff2$nest_succ[n]<-breed$NEST_SUCC[breed$YEAR==d]
  
}
summary(ff2)

####ajout des données de températures moyennes et maximales et de précipitations cumulées
#####Températures#####
#chargement des données de températures
temp<-read.table("Tair moy 1989-2016 BYLCAMP.txt", h=T, sep="\t", dec=",")
summary(temp)
#temp<-na.omit(temp)
#transformation date en jour julien#
#install.packages("date")
require(date)

temp$YMD<-paste(temp$YEAR,temp$MONTH,temp$DAY,sep = "-")
temp$YMD<-strptime(temp$YMD, format = "%Y-%m-%d")
temp$JJ<-format(temp$YMD,format = "%j")
temp$JJ<-as.numeric(temp$JJ)

#calcul de la température moyenne entre la première et la dernière date d'observation de renard pour chaque année
#retrait de 2016 car données manquantes pour températures
ff2 <- ff2[!ff2$year == 2016,]
for(n in 1:nrow(ff2)){
  d<-ff2$year[n]
  #mini_jour <- ff2$min_date[n]
  #maxi_jour <- ff2$max_date[n]
  ff2$max_temp[n] <- max(temp$TEMP[temp$JJ >= ff2$min_date[n] & temp$JJ <= ff2$max_date[n] & temp$YEAR == d], na.rm=T)
  ff2$mean_temp[n] <- mean(temp$TEMP[temp$JJ >= ff2$min_date[n] & temp$JJ <= ff2$max_date[n] & temp$YEAR == d], na.rm=T)
  ff2$sd_temp[n] <- sd(temp$TEMP[temp$JJ >= ff2$min_date[n] & temp$JJ <= ff2$max_date[n] & temp$YEAR == d], na.rm=T)
}
summary(ff2)

plot(ff2$mean_temp,ff2$atq_rate)

smoothingSpline = smooth.spline(ff2$max_temp, ff2$atq_rate, spar=0.1)
plot(ff2$max_temp,ff2$atq_rate, type = "p", col = "darkorange", font.axis = 3, las = 1, xaxt = "n", xlab = "Maximal temperature", ylab = "Attack rate") #semble y avoir une tendance avec une température optimale d'attaque pour le renard
# Modification de l'axe des x
xtick<-seq(7, 11, by=1)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
  labels = xtick, srt = 0, pos = 1, xpd = TRUE)
lines(smoothingSpline, col = "orange")

boxplot(ff2$atq_rate~ff2$max_temp, xlab = "Maximal temperature", ylab = "Attack rate")

boxplot(ff2$atq_rate~ff2$mean_temp, xlab = "Maximal temperature", ylab = "Attack rate")
#####Précipitations#####
#calcul précipitation cumulée entre la première et la dernière date d'observation de renard pour chaque année 

prec<-read.table("precipitation_Bylot_1996-2016.txt",h=T, dec = ",", sep = "\t")
summary(prec)

for(n in 1:nrow(ff2)){
  d<-ff2$year[n]
  #mini_jour <- ff2$min_date[n]
  #maxi_jour <- ff2$max_date[n]
  ff2$cumul_prec[n] <- sum(prec$rain[prec$day >= ff2$min_date[n] & prec$day <= ff2$max_date[n] & prec$year == d], na.rm=T)
}
summary(ff2)

plot(ff2$cumul_prec,ff2$atq_rate)
summary(ff2)

####Complete data with C1 and C1/2 lmg abundance####
setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
LG <- read.table("LEM96-2016.txt", sep = "\t", dec = ".", h = T)
summary(LG)

for (i in 1:nrow(ff2)) {
  d <- ff2$year[i]
  ff2$lmg_C12[i] <- LG$LMG_C1_C2[LG$YEAR == d]
  ff2$lmg_C1[i] <- LG$LMG_C1[LG$YEAR == d]
  
}
summary(ff2)
colnames(ff2)[9] <- "lmg_C2"

#write.csv(ff2, "FOX-functional response V2.txt")
