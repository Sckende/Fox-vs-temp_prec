################ Compilation pour calcul de taux d'attaque par individu PAR JOUR au travers de toutes les années ***####################
####### Calcul des moyennes d'attaque individuelles ######
#### À partir des fichiers brutes de time budget ####

# ICI CREATION DE BDD AVEC UN TAUX D'ATTAQUE PAR INDIVIDU PAR JOUR

getwd ()
setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm( list = ls ())

######################## Fox - 2004 ###########################
rf<-read.table("FOX-2004-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)
levels(rf$Behavior)
levels(rf$Item)

rf2 <- rf[rf$Behavior =="attaque",]
levels(rf2$Item)

# Garder les items couple, egg, oie & young
rf2 <- rf2[rf2$Item == "oie"| rf2$Item == "egg"|rf2$Item == "couple"|rf2$Item == "young",]
rf2 <- droplevels(rf2); summary(rf2)
View(rf2)


# Récupération de la durée totale des observations par individu
TAB <- NULL


for (j in unique(rf2$ID)){
  for (i in unique(rf2$Date[rf2$ID == j])) {
    for (k in unique(rf2$Obs_lenght[rf2$ID == j & rf2$Date == i])) {
    YEAR <- 2004  
    id <- j
    date <- i
    observation <- k
    rf2$attq[rf2$ID==j & rf2$Date == i & rf2$Obs_lenght == k] <- 1 
    tot_attq <- sum(rf2$attq[rf2$ID==j & rf2$Date == i & rf2$Obs_lenght == k])
    
    di <-data.frame(YEAR, id, date, observation, tot_attq)
    TAB <- rbind(TAB, di)
    
  }}}
print(TAB)
View(rf2[rf2$Date == 183,])

#### Ajout des variables biotiques et abiotiques pour analyses de piste ####

####Ajout des autres variables biologiques et météorologiques####
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

TAB$YJJ <- paste(TAB$YEAR, TAB$date, sep = "-")
temp$YJJ <- paste(temp$YEAR, temp$JJ, sep = "-")
TAB$temp <- temp$TEMP[match(TAB$YJJ, temp$YJJ)]
head(TAB)

TAB$atq_rate <- TAB$tot_attq/TAB$observation

plot(TAB$temp, TAB$atq_rate)
lines(smooth.spline(TAB$temp, TAB$atq_rate, df = 2))

# Retrait observations <1800 s
TAB3min <- TAB[TAB$observation >= 1800,]
plot(TAB3min$temp, TAB3min$atq_rate)
lines(smooth.spline(TAB3min$temp, TAB3min$atq_rate, df = 2))


######################## Fox - 2005 ###########################
rf<-read.table("FOX-2005-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)
levels(rf$Behavior)
levels(rf$Item)

rf2 <- rf[rf$Behavior =="attaque",]
levels(rf2$Item)

# Garder les items couple, egg, oie & young
rf2 <- rf2[rf2$Item == "oie"| rf2$Item == "egg"|rf2$Item == "couple"|rf2$Item == "young",]
rf2 <- droplevels(rf2); summary(rf2)
View(rf2)


# Récupération de la durée totale des observations par individu
TAB <- NULL


for (j in unique(rf2$ID)){
  for (i in unique(rf2$Date[rf2$ID == j])) {
    for (k in unique(rf2$Obs_lenght[rf2$ID == j & rf2$Date == i])) {
      YEAR <- 2005  
      id <- j
      date <- i
      observation <- k
      rf2$attq[rf2$ID==j & rf2$Date == i & rf2$Obs_lenght == k] <- 1 
      tot_attq <- sum(rf2$attq[rf2$ID==j & rf2$Date == i & rf2$Obs_lenght == k])
      
      di <-data.frame(YEAR, id, date, observation, tot_attq)
      TAB <- rbind(TAB, di)
      
    }}}
print(TAB)
View(rf2[rf2$Date == 183,])

#### Ajout des variables biotiques et abiotiques pour analyses de piste ####

####Ajout des autres variables biologiques et météorologiques####
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

TAB$YJJ <- paste(TAB$YEAR, TAB$date, sep = "-")
temp$YJJ <- paste(temp$YEAR, temp$JJ, sep = "-")
TAB$temp <- temp$TEMP[match(TAB$YJJ, temp$YJJ)]
head(TAB)

TAB$atq_rate <- TAB$tot_attq/TAB$observation

plot(TAB$temp, TAB$atq_rate)
lines(smooth.spline(TAB$temp, TAB$atq_rate, df = 2))

# Retrait observations <1800 s
TAB3min <- TAB[TAB$observation >= 1800,]
plot(TAB3min$temp, TAB3min$atq_rate)
lines(smooth.spline(TAB3min$temp, TAB3min$atq_rate, df = 2))

######################## Fox - 2015 ###########################
rf<-read.table("FOX-2015-fonct-essai.txt", sep = "\t", h = T)
summary(rf)
levels(rf$ID)
levels(rf$Behavior)

rf2 <- rf[rf$Behavior =="ATTAQ",]
levels(rf2$Item)

# Récupération de la durée totale des observations par individu
TAB <- NULL


for (j in unique(rf2$ID)){
  for (i in unique(rf2$Date[rf2$ID == j])) {
    for (k in unique(rf2$Obs_lenght[rf2$ID == j & rf2$Date == i])) {
      YEAR <- 2004  
      id <- j
      date <- i
      observation <- k
      rf2$attq[rf2$ID==j & rf2$Date == i & rf2$Obs_lenght == k] <- 1 
      tot_attq <- sum(rf2$attq[rf2$ID==j & rf2$Date == i & rf2$Obs_lenght == k])
      
      di <-data.frame(YEAR, id, date, observation, tot_attq)
      TAB <- rbind(TAB, di)
      
    }}}
print(TAB)
View(rf2[rf2$Date == 183,])

#### Ajout des variables biotiques et abiotiques pour analyses de piste ####

####Ajout des autres variables biologiques et météorologiques####
#####Températures#####

TAB$YJJ <- paste(TAB$YEAR, TAB$date, sep = "-")
TAB$temp <- temp$TEMP[match(TAB$YJJ, temp$YJJ)]
head(TAB)

TAB$atq_rate <- TAB$tot_attq/TAB$observation

plot(TAB$temp, TAB$atq_rate)
lines(smooth.spline(TAB$temp, TAB$atq_rate, df = 2))

# Retrait observations <1800 s
TAB3min <- TAB[TAB$observation >= 1800,]
plot(TAB3min$temp, TAB3min$atq_rate)
lines(smooth.spline(TAB3min$temp, TAB3min$atq_rate, df = 2))
# AUCUNE OBS > 3 minutes