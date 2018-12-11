setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

#####Chargement du fichier comportements renard 2015#####
comp<-read.table("Fox_compt_2015.txt", h=T, sep="\t",dec=",")
summary(comp)
levels(comp$CPT)

#####Regroupement des comportements#####
#comp[(comp$CPT=="INTER_FOX" | comp$CPT=="INTER_FOX_NEG"),]
comp$CPTbis[comp$CPT=="INTER_FOX"|comp$CPT=="INTER_FOX_NEG"|comp$CPT=="CHASS_LAB"|comp$CPT=="CHASS_OIE"|comp$CPT=="INTER_OIE"|comp$CPT=="INTER_LP"|comp$CPT=="INTER_LLQ"|comp$CPT=="INTER_LAB"]<-"INTERACTION"
comp$CPTbis[comp$CPT=="ATTAQ"]<-"ATTAQUE"
comp$CPTbis[comp$CPT=="CACHE"]<-"CACHE"
comp$CPTbis[comp$CPT=="CH"]<-"CHERCHE"
comp$CPTbis[comp$CPT=="DEP"]<-"DEPLACEMENT"
comp$CPTbis[comp$CPT=="SHIT"]<-"MARQUAGE"
comp$CPTbis[comp$CPT=="COUCHE"|comp$CPT=="CREUSE"|comp$CPT=="GROOM"|comp$CPT=="MANGE"|comp$CPT=="PAUSE"|comp$CPT=="RAMASSE"]<-"AUTRES"

#####Ranger dans l'ordre alphabétique les types de comportements pour homogénéiser tous les piecharts#####
comp<-comp[order(comp$CPTbis),]

#####Calcul des fréquences de comportements pour le piechart#####
j<-unique(comp$CPTbis)
TAB<-NULL
for (i in j){
  TOT<-sum(comp$CPTbis==i)
  FREQ<-TOT/nrow(comp)
  r<-data.frame(i,TOT,FREQ)
  
  TAB<-rbind(TAB,r)
}
TAB


require(plotrix)
pie3D(TAB$TOT,labels = TAB$i, explode = 0.15,labelcex=0.7, main="Pie chart of fox behaviour in 2015", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))


#####Chargement du fichier comportements renard 1996-1999#####

#####Chargement du fichier comportements renard 2004-2005#####
comp1<-read.table("Fox_compt_2004-2005.txt", h=T, sep="\t",dec=",")
summary(comp1)

#comp1$CPT[comp1$CPT=="ATTAQ"]="ATTAQUE"#commande qui génère des NAs, ne fonctionne pas sur variable qualitative
levels(comp1$CPT)[1]="ATTAQUE"#pour modification des valeurs qualitatives
#comp1[match("ATTAQ",levels(comp1$CPT))]<-"ATTAQUE”#commande Nicolas


comp1a<-subset(comp1,YEAR==2004);comp1a<-comp1a[order(comp1a$CPT),]
comp1b<-subset(comp1,YEAR==2005);comp1b<-comp1b[order(comp1b$CPT),]



j<-unique(comp1a$CPT)
TAB1a<-NULL
for (i in j){
  TOT1a<-sum(comp1a$CPT==i)
  FREQ1a<-TOT1a/nrow(comp1a)
  r<-data.frame(i,TOT1a,FREQ1a)
  
  TAB1a<-rbind(TAB1a,r)
}
TAB1a
#require(plotrix)
pie3D(TAB1a$TOT1a,labels = TAB1a$i, explode = 0.15,labelcex=0.7, main="Pie chart of fox behaviour in 2004", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))


j<-unique(comp1b$CPT)
TAB1b<-NULL
for (i in j){
  TOT1b<-sum(comp1b$CPT==i)
  FREQ1b<-TOT1b/nrow(comp1b)
  r<-data.frame(i,TOT1b,FREQ1b)
  
  TAB1b<-rbind(TAB1b,r)
}
TAB1b
#require(plotrix)
pie3D(TAB1b$TOT1b,labels = TAB1b$i, explode = 0.15,labelcex=0.7, main="Pie chart of fox behaviour in 2005", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))

# One figure in row 1 and two figures in row 2
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
pie3D(TAB1a$TOT1a,labels = TAB1a$i, explode = 0.15,labelcex=0.7, main="Fréquence des comportements de renard 2004", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))
pie3D(TAB1b$TOT1b,labels = TAB1b$i, explode = 0.15,labelcex=0.7, main="Fréquence des comportements de renard 2005", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))
pie3D(TAB$TOT,labels = TAB$i, explode = 0.15,labelcex=0.7, main="Fréquence des comportements de renard 2015", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
#radial.pie(TAB1a$TOT1a,labels = TAB1a$i,grid.unit = F, labelcex=0.7, main ="Fréquence des comportements de renard 2004", sector.colors = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"), radlab=T)#joli mais à revoir pour l'interprétation
#radial.pie(TAB1a$TOT1a,labels = TAB1a$i,labelcex=0.7, main="Fréquence des comportements de renard 2004", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"), radlab=T)
#radial.pie(TAB$TOT,labels = TAB$i,labelcex=0.7, main="Fréquence des comportements de renard 2015", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"), radlab=T)


################ Compilation pour calcul de taux d'attaque par individu au travers de toutes les années ***####################
####### Calcul des moyennes d'attaque individuelles ######
#### À partir des fichiers brutes de time budget ####

# ICI CREATION DE BDD AVEC UN TAUX D'ATTAQUE PAR INDIVIDU

getwd ()
setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm( list = ls ())

######################## Fox - 2004 ###########################
rf<-read.table("FOX-2004-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)

# Récupération de la durée totale des observations par individu
TAB <- NULL
TAB1 <- NULL

for (j in unique(rf$ID)){
  for (i in unique(rf$Date[rf$ID == j])) {#pour diminuer le risque de perdre une durées d'observation identiques pour un même individu
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

fox_fonc <- cbind(rep(2004, dim(TAB1)[1]),rep(min(TAB$date), dim(TAB1)[1]),rep(max(TAB$date), dim(TAB1)[1]),TAB1, SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
names(fox_fonc) <- c("year","min_date","max_date",names(TAB1), "tot_atq")

######################## Fox - 2005 ###########################
rf<-read.table("FOX-2005-fonct-essai.txt", sep = "\t", h = T)
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
rf<-read.table("FOX-1996-1999-fonct-essai.txt", sep = "\t", h = T)
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
rf<-read.table("FOX-2015-fonct-essai.txt", sep = "\t", h = T)
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
rf<-read.table("FOX-2016-fonct-essai.txt", sep = "\t", h = T)
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

#garder observation supérieure et égale à 3 minutes (voir papier Careau)
ff2 <- fox_fonc[fox_fonc$tot_obs>=180,]
boxplot(ff2$atq_rate)
plot(ff2$year, ff2$atq_rate)
dim(ff2)

#### Ajout des variables biotiques et abiotiques pour analyses de piste ####

####Ajout des autres variables biologiques et météorologiques####
#changement de répertoire
setwd("/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
fox<-read.csv("FOX_abundance_Chevallier.txt", sep = "\t", dec = ",")
lmg<-read.csv("LEM_1993-2017.txt", sep = "\t", dec = ",")
AO<-read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
breed<-read.csv("GOOSE_breeding_informations.txt", sep = "\t", dec = ".")
PP <- read.csv("VEG_Prod_prim_camp_2.txt", sep = "\t", dec = ",")

#ajout des données de lmg, AO, démo oies, fox
for(n in 1:nrow(ff2)){
  d<-ff2$year[n]
  ff2$prop_fox_dens[n]<-fox$prop_natal_dens[fox$year == d]
  #ff2$prop_fox_dens_timelag[n]<-fox$prop_natal_dens[fox$year == d + 1]
  ff2$lmg_C2[n]<-lmg$LMG_C2[lmg$YEAR == d]
  ff2$lmg_C12[n] <- lmg$LMG_C1_C2[lmg$YEAR == d]
  ff2$lmg_C1[n] <- lmg$LMG_C1[lmg$YEAR == d]
  
  ff2$winAO[n]<-AO$winAO[AO$YEAR==d]
  ff2$sprAO[n]<-AO$sprAO[AO$YEAR==d]
  ff2$esumAO[n]<-AO$esumAO[AO$YEAR==d]
  ff2$lsumAO[n]<-AO$lsumAO[AO$YEAR==d]
  ff2$sumAO[n]<-AO$sumAO[AO$YEAR==d]
  ff2$AOnidif[n]<-AO$AOnidif[AO$YEAR==d]
  
  ff2$nest_density[n]<-breed$NEST_DENSITY[breed$YEAR==d]
  ff2$clutch_size[n]<-breed$CLUTCH_SIZE[breed$YEAR==d]
  ff2$egg_abun[n]<-breed$EGG_ABUN[breed$YEAR==d]
  ff2$ratio_JUVad[n]<-breed$RATIO_YOU_AD[breed$YEAR==d]
  ff2$brood_size[n]<-breed$BROOD_SIZE_BAND[breed$YEAR==d]
  ff2$nest_succ[n]<-breed$NEST_SUCC[breed$YEAR==d]
  
}
summary(ff2)

#ajout des types d'annees de lemmings (peak, crash or intermediaire)
ff2$lmg_year <- lmg$LMG_YEAR[match(ff2$year, lmg$YEAR)]
#questionner quelles annees sont pic ou crash ou inter
unique(ff2$year[which(ff2$lmg_year == "crash")]) # "peak" / "inter"

####ajout des données de températures moyennes et maximales et de précipitations cumulées
#####Températures#####
#chargement des données de températures
temp<-read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", h=T, sep="\t", dec=",")
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

for(n in 1:nrow(ff2)){
  d<-ff2$year[n]
  #mini_jour <- ff2$min_date[n]
  #maxi_jour <- ff2$max_date[n]
  ff2$max_temp[n] <- max(temp$TEMP[temp$JJ >= ff2$min_date[n] & temp$JJ <= ff2$max_date[n] & temp$YEAR == d], na.rm=T)
  ff2$min_temp[n] <- min(temp$TEMP[temp$JJ >= ff2$min_date[n] & temp$JJ <= ff2$max_date[n] & temp$YEAR == d], na.rm=T)
  ff2$mean_temp[n] <- mean(temp$TEMP[temp$JJ >= ff2$min_date[n] & temp$JJ <= ff2$max_date[n] & temp$YEAR == d], na.rm=T)
  ff2$sd_temp[n] <- sd(temp$TEMP[temp$JJ >= ff2$min_date[n] & temp$JJ <= ff2$max_date[n] & temp$YEAR == d], na.rm=T)
}
summary(ff2)

plot(ff2$mean_temp,ff2$atq_rate)

smoothingSpline = smooth.spline(ff2$max_temp, ff2$atq_rate, spar=0.5)
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

prec<-read.table("PREC_precipitation_Bylot_1996-2016.txt",h=T, dec = ",", sep = "\t")
summary(prec)

for(n in 1:nrow(ff2)){
  d<-ff2$year[n]
  #mini_jour <- ff2$min_date[n]
  #maxi_jour <- ff2$max_date[n]
  ff2$cumul_prec[n] <- sum(prec$RAIN[prec$JJ >= ff2$min_date[n] & prec$JJ <= ff2$max_date[n] & prec$YEAR == d], na.rm=T)
}
summary(ff2)

plot(ff2$cumul_prec,ff2$atq_rate)
lines(smooth.spline(ff2$cumul_prec,ff2$atq_rate, spar = 0.6))
summary(ff2)

fff <- ff2[ff2$cumul_prec <= 60,]
plot(fff$cumul_prec, fff$atq_rate)
lines(smooth.spline(fff$cumul_prec,fff$atq_rate, spar = 0.6))
#Rajout des variables de productivite primaire et de time lag prop_fox
#creation dataframe pour prop_fox_dens_lag (decalage d'une annee par rapport à l'abondance de lmg)
foxylag <- data.frame("year" = rep(NA, time = length(fox$year)), "prop_repro" = rep(NA, time = length(fox$year)))
for (k in 1:nrow(fox)){
  foxylag$year[k] <- fox$year[k]
  foxylag$prop_repro[k] <- fox$prop_natal_dens[k + 1]
}
#View(foxylag)
ff2$fox_lag <- foxylag$prop_repro[match(ff2$year, foxylag$year)]
ff2$prim_prod <- PP$PROD_INDICE_C2[match(ff2$year, PP$YEAR)]

#write.csv(ff2, "FOX-functional response V2.txt")




