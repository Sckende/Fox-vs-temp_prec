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

# Computation of "goo_atq_rate" = atq rate per obs per ind for goose items (goose, eggs, and gosling) & "AD_atq_rate" = atq rate only on nests with adult
levels(rf$Item[rf$Behavior == "attaque"]) # keep items: couple, egg, oie, young

data <- lapply(data, function(x){
  for(i in 1:length(x$Behavior)){
    if(x$Behavior[i] == "attaque" & x$Item[i] %in% c("couple", "egg", "oie", "young")){
      x$goo_atq[i] <- 1
    } else {
      x$goo_atq[i] <- 0
    }
    if(x$Behavior[i] == "attaque" & x$Item[i] %in% c("couple", "oie")){
      x$AD_atq[i] <- 1
    } else {
      x$AD_atq[i] <- 0
    }
  }
  x$goo_atq_rate <- sum(x$goo_atq)/x$Obs_lenght[1]
  x$AD_atq_rate <- sum(x$AD_atq)/x$Obs_lenght[1]
  x
})

FOX_ATQ <- do.call("rbind", data)
head(FOX_ATQ)
summary(FOX_ATQ)
FOX_ATQ$Behavior <- as.character(FOX_ATQ$Behavior)
FOX_ATQ$Item <- as.character(FOX_ATQ$Item)
FOX_ATQ$Habitat <- as.character(FOX_ATQ$Habitat)
FOX_ATQ$Cache <- as.character(FOX_ATQ$Cache)
FOX_ATQ$ID <- as.character(FOX_ATQ$ID)

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
    if(x$Behavior[i] == "attaque" & x$Item[i] %in% c("couple", "oie")){
      x$AD_atq[i] <- 1
    } else {
      x$AD_atq[i] <- 0
    }
  }
  x$goo_atq_rate <- sum(x$goo_atq)/x$Obs_lenght[1]
  x$AD_atq_rate <- sum(x$AD_atq)/x$Obs_lenght[1]
  x
})

rf <- do.call("rbind", data)

summary(rf)
rf$Behavior <- as.character(rf$Behavior)
rf$Item <- as.character(rf$Item)
rf$Habitat <- as.character(rf$Habitat)
rf$Cache <- as.character(rf$Cache)
rf$ID <- as.character(rf$ID)

FOX_ATQ <- rbind(FOX_ATQ, rf)
names(FOX_ATQ)
FOX_ATQ <- FOX_ATQ[,-c(1, 6, 8:10)]

######################## Fox - 1996-1999 ###########################
rf<-read.table("FOX-1996-1999-fonct-essai_V2.txt", sep = "\t", h = T)
head(rf)
names(rf)
summary(rf)
rf$ID <- as.factor(rf$ID)

# Conversion of OBS_LENGTH in second
rf$OBS_LENGTH <- as.numeric(substr(rf$OBS_LENGTH, 4, 5))
rf$OBS_LENGTH <- rf$OBS_LENGTH*60


# Checking ID levels
levels(rf$ID)

# Checking behaviours levels
levels(rf$BEHAV)

# Checking items levels
levels(rf$SP_PREDAT)

# Split database depending on year, Date, ID and, Obs_length
data <- split(rf, paste(rf$AN, rf$DATE, rf$ID, rf$OBS_LENGTH))

# Computation of "all_atq_rate", "goo_atq_rate", and "AD_atq_rate"
levels(rf$BEHAV)
levels(rf$SP_PREDAT[rf$BEHAV == "attaque" | rf$BEHAV == "attaque_mult"]) # keep items: egg, lmg, oie
table(rf$SP_PREDAT[rf$BEHAV == "attaque" | rf$BEHAV == "attaque_mult"], useNA = "always")

data <- lapply(data, function(x){
  for(i in 1:length(x$BEHAV)){
    if(x$BEHAV[i] %in% c("attaque", "attaque_mult") & x$SP_PREDAT[i] %in% c("egg", "lmg", "oie")){
      x$all_atq[i] <- 1
    } else {
      x$all_atq[i] <- 0
    }
    if(x$BEHAV[i] %in% c("attaque", "attaque_mult") & x$SP_PREDAT[i] %in% c("egg", "oie")){
      x$goo_atq[i] <- 1
    } else {
      x$goo_atq[i] <- 0
    }
    if(x$BEHAV[i] %in% c("attaque", "attaque_mult") & x$SP_PREDAT[i] == "oie"){
      x$AD_atq[i] <- 1
    } else {
      x$AD_atq[i] <- 0
    }
  }
  x$all_atq_rate <- sum(x$all_atq)/x$OBS_LENGTH[1] 
  x$goo_atq_rate <- sum(x$goo_atq)/x$OBS_LENGTH[1] 
  x$AD_atq_rate <- sum(x$AD_atq)/x$OBS_LENGTH[1]
  x
})

rf <- do.call("rbind", data)

names(FOX_ATQ)
FOX_ATQ <- cbind(Bloc = NA, FOX_ATQ, Prec = NA)
names(rf)
rf.2 <- as.data.frame(cbind(Bloc = as.character(rf$BLOC),
                            Year = rf$AN,
                            Date = rf$DATE,
                            Cache = NA,
                            ID = rf$ID,
                            Obs_lenght = rf$OBS_LENGTH,
                            Habitat = as.character(rf$HAB),
                            Behavior = as.character(rf$BEHAV),
                            Item = as.character(rf$SP_PREDAT),
                            all_atq = rf$all_atq,
                            all_atq_rate = as.numeric(rf$all_atq_rate),
                            goo_atq = rf$goo_atq,
                            goo_atq_rate = as.numeric(rf$goo_atq_rate),
                            AD_atq = rf$AD_atq,
                            AD_atq_rate = as.numeric(rf$AD_atq_rate),
                            Prec = rf$PREC))

head(rf.2)
rf.2$all_atq_rate <- as.numeric(as.character(rf.2$all_atq_rate))
rf.2$goo_atq_rate <- as.numeric(as.character(rf.2$goo_atq_rate))
rf.2$AD_atq_rate <- as.numeric(as.character(rf.2$AD_atq_rate))
summary(rf.2)
names(rf.2)

FOX_ATQ <- rbind(FOX_ATQ, rf.2)
summary(FOX_ATQ)


######################## Fox - 2015- 2017 ###########################
rf <- read.table("FOX-2015-2017-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$Cache)
levels(rf$CPT)
rf$Item <- as.character(rf$Item)
table(rf$Item)

# Checking ID levels
table(rf$new_ID)

# Checking behaviours levels
levels(rf$CPT)

# Checking items levels when attacking
table(rf$Item[rf$CPT == "ATTAQ"])

# Split database depending on year, Date, ID and, Obs_length
data <- split(rf, paste(rf$YEAR, rf$Date, rf$new_ID, rf$OBS_LENGHT))

# Computation of "all/goo/AD_atq_rate"
levels(rf$CPT)
table(rf$Item[rf$CPT == "ATTAQ"]) # keep items: LMG, DEAD_EGG, EGG, OIE, YOUNG 
table(rf$Item[rf$CPT == "ATTAQ"], useNA = "always")

data <- lapply(data, function(x){
  for(i in 1:length(x$CPT)){
    if(x$CPT[i] ==  "ATTAQ"){
      x$all_atq[i] <- 1
    } else {
      x$all_atq[i] <- 0
    }
    if(x$CPT[i] == "ATTAQ" & x$Item[i] %in% c("egg", "OIE", "YOUNG", "DEAD_EGG")){
      x$goo_atq[i] <- 1
    } else {
      x$goo_atq[i] <- 0
    }
    if(x$CPT[i] == "ATTAQ" & x$Item[i] == "OIE"){
      x$AD_atq[i] <- 1
    } else {
      x$AD_atq[i] <- 0
    }
  }
  x$all_atq_rate <- sum(x$all_atq)/x$OBS_LENGHT[1] 
  x$goo_atq_rate <- sum(x$goo_atq)/x$OBS_LENGHT[1] 
  x$AD_atq_rate <- sum(x$AD_atq)/x$OBS_LENGHT[1]
  x
})

rf <- do.call("rbind", data)

# Check point
rf[rf$CPT == "ATTAQ" & rf$Item == "egg",]

names(FOX_ATQ)
names(rf)
rf.2 <- as.data.frame(cbind(Bloc = as.character(rf$Bloc),
                            Year = rf$YEAR,
                            Date = rf$Date,
                            Cache = as.character(rf$Cache),
                            ID = as.character(rf$new_ID),
                            Obs_lenght = rf$OBS_LENGHT,
                            Habitat = as.character(rf$Habitat),
                            Behavior = as.character(rf$CPT),
                            Item = as.character(rf$Item),
                            all_atq = rf$all_atq,
                            all_atq_rate = rf$all_atq_rate,
                            goo_atq = rf$goo_atq,
                            goo_atq_rate = rf$goo_atq_rate,
                            AD_atq = rf$AD_atq,
                            AD_atq_rate = rf$AD_atq_rate,
                            Prec = rf$PRECIPITATION))

head(rf.2)
rf.2$all_atq_rate <- as.numeric(as.character(rf.2$all_atq_rate))
rf.2$goo_atq_rate <- as.numeric(as.character(rf.2$goo_atq_rate))
summary(rf.2)
names(rf.2)

FOX_ATQ <- rbind(FOX_ATQ, rf.2)
summary(FOX_ATQ)

# Clean the database to have one row per atq rate per observation
head(FOX_ATQ)

FOX <- split(FOX_ATQ, paste(FOX_ATQ$Year, FOX_ATQ$Date, FOX_ATQ$ID, FOX_ATQ$Obs_lenght))

FOX[1:10]
FOX <- lapply(FOX, function(x){
  x <- x[1,]
})

# Add climatic variables

x11()
par(mfrow = c(1, 2))
boxplot(FOX_ATQ$all_atq_rate)
boxplot(FOX_ATQ$goo_atq_rate)









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
