getwd ()
setwd(dir = "/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm( list = ls ())

######################## PREDATION FOX-LEMMING #####################
######################## Fox - 2004 ###########################
rf<-read.table("FOX-2004-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
rf$Duration <- as.numeric(rf$Duration)
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

# Récupération des actions avec lemmings
levels(rf$Behavior); levels(rf$Item)

rf1 <- subset(rf, rf$Item == "lemming") #subset uniquement avec les differents comportements du renard sur les lemmings
rf1<-droplevels(rf1) #retirer les levels == à 0
summary(rf1)
table(rf1$ID, rf1$Behavior)

#Nombre d'attaque total par individu
ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == "attaque"); ff <- droplevels(ff)
SUM_atq <- table(ff$ID, ff$Behavior)
SUM_atq <- as.data.frame(SUM_atq); names(SUM_atq) <- c("ID", "Behaviour", "atq")

fox_fonc_2004 <- cbind(2004, rep(min(TAB$date), dim(TAB1)[1]), rep(max(TAB$date), dim(TAB1)[1]),TAB1, "lemming", SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
names(fox_fonc_2004) <- c("year","min_date","max_date",names(TAB1), "item", "tot_atq")

#Nombre total de type de manipulation par individu
  table(rf1$ID, rf1$Behavior)

# Duree de consommation moyen par individu
    i <- "mange"
    ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
    MOY <- tapply(ff$Duration, ff$ID, mean)
    MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
    
    fox_fonc_2004$MEAN_CONSO_DURATION <- MOY$mange[match(fox_fonc_2004$fox, MOY$ID)]

    
# Duree de transport moyen par individu
  i <- "transport"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2004$MEAN_TRANSP_DURATION <- MOY$transport[match(fox_fonc_2004$fox, MOY$ID)]

# Temps de cache moyen par individu
  i <- "cache"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2004$MEAN_CACHE_DURATION <- MOY$cache[match(fox_fonc_2004$fox, MOY$ID)]
  
  
  ######################## PREDATION FOX-LEMMING #####################
  ######################## Fox - 2005 ###########################
  rf<-read.table("FOX-2005-fonct-essai.txt", sep = "\t", h = T)
  summary(rf)
  rf$Duration <- as.numeric(rf$Duration)
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
  
  # Récupération des actions avec lemmings
  levels(rf$Behavior); levels(rf$Item)
  
  rf1 <- subset(rf, rf$Item == "lemming") #subset uniquement avec les differents comportements du renard sur les lemmings
  rf1<-droplevels(rf1) #retirer les levels == à 0
  summary(rf1)
  table(rf1$ID, rf1$Behavior)
  
  #Nombre d'attaque total par individu
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == "attaque"); ff <- droplevels(ff)
  SUM_atq <- table(ff$ID, ff$Behavior)
  SUM_atq <- as.data.frame(SUM_atq); names(SUM_atq) <- c("ID", "Behaviour", "atq")
  
  fox_fonc_2005 <- cbind(2005, rep(min(TAB$date), dim(TAB1)[1]), rep(max(TAB$date), dim(TAB1)[1]),TAB1, "lemming", SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
  names(fox_fonc_2005) <- c("year","min_date","max_date",names(TAB1), "item", "tot_atq")
  
  #Duree totale de manipulation par individu
  table(rf1$ID, rf1$Behavior)
  
  # Duree de consommation moyen par individu
  i <- "mange"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2005$MEAN_CONSO_DURATION <- MOY$mange[match(fox_fonc_2005$fox, MOY$ID)]
  
  # Duree de transport moyen par individu
  i <- "transport"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2005$MEAN_TRANSP_DURATION <- MOY$transport[match(fox_fonc_2005$fox, MOY$ID)]
  
  # Temps de cache moyen par individu
  i <- "cache"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2005$MEAN_CACHE_DURATION <- MOY$cache[match(fox_fonc_2005$fox, MOY$ID)]
  
  ######################## PREDATION FOX-LEMMING #####################
  ######################## Fox - 2015 ###########################
  rf<-read.table("FOX-2015-fonct-essai.txt", sep = ",", h = T)
  summary(rf)
  

  ###################HERE I AM##########################################
  #Traitement variable CPT_LENGHT
  require(lubridate)
  rf$CPT_LENGHT <- as.POSIXct(rf$CPT_LENGHT, tz = "America/Toronto", format = "%H:%M:%S")
  rf$Duration <- as.numeric(rf$Duration)
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
  
  # Récupération des actions avec lemmings
  levels(rf$Behavior); levels(rf$Item)
  
  rf1 <- subset(rf, rf$Item == "lemming") #subset uniquement avec les differents comportements du renard sur les lemmings
  rf1<-droplevels(rf1) #retirer les levels == à 0
  summary(rf1)
  table(rf1$ID, rf1$Behavior)
  
  #Nombre d'attaque total par individu
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == "attaque"); ff <- droplevels(ff)
  SUM_atq <- table(ff$ID, ff$Behavior)
  SUM_atq <- as.data.frame(SUM_atq); names(SUM_atq) <- c("ID", "Behaviour", "atq")
  
  fox_fonc_2005 <- cbind(2005, rep(min(TAB$date), dim(TAB1)[1]), rep(max(TAB$date), dim(TAB1)[1]),TAB1, "lemming", SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)]) #SUM_atq$atq[match(TAB1$fox,SUM_atq$ID)] #Match les valeurs du nombre d'attaque par les ID
  names(fox_fonc_2005) <- c("year","min_date","max_date",names(TAB1), "item", "tot_atq")
  
  #Duree totale de manipulation par individu
  table(rf1$ID, rf1$Behavior)
  
  # Duree de consommation moyen par individu
  i <- "mange"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2005$MEAN_CONSO_DURATION <- MOY$mange[match(fox_fonc_2005$fox, MOY$ID)]
  
  # Duree de transport moyen par individu
  i <- "transport"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2005$MEAN_TRANSP_DURATION <- MOY$transport[match(fox_fonc_2005$fox, MOY$ID)]
  
  # Temps de cache moyen par individu
  i <- "cache"
  
  ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
  MOY <- tapply(ff$Duration, ff$ID, mean)
  MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- i
  
  fox_fonc_2005$MEAN_CACHE_DURATION <- MOY$cache[match(fox_fonc_2005$fox, MOY$ID)]
  
  
#### BOUCLE NON FONCTIONNELLE ####

  for (i in c("cache", "transport", "mange")) {
    ff <- subset(rf, rf$Item == "lemming" & rf$Behavior == i ); ff <- droplevels(ff)
    MOY <- tapply(ff$Duration, ff$ID, mean)
    MOY <- data.frame(ID = names(MOY), i = MOY); names(MOY)[2] <- "behav"
    
    #assign(paste("MEAN_", i, "_DURATION", sep = ""), MOY)
    
    fox_fonc_2004 <- cbind(fox_fonc_2004, MOY$behav[match(fox_fonc_2004$fox, MOY$ID)])
    print(i)
    
  }