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
rf.2$AD_atq_rate <- as.numeric(as.character(rf.2$AD_atq_rate))
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

FOX <- do.call("rbind", FOX)
summary(FOX)

FOX[-c(3, 6, 11, 14, 15)] <- lapply(FOX[-c(3, 6, 11, 14, 15)], as.factor)
FOX[c(3, 6, 11, 14, 15)] <- lapply(FOX[c(3, 6, 11, 14, 15)], as.numeric)
str(FOX)

FOX$Year <- as.character(FOX$Year)
FOX$Year[FOX$Year == "98"] <- "1998"
FOX$Year[FOX$Year == "99"] <- "1999"
FOX$Year[FOX$Year == "96"] <- "1996"
FOX$Year[FOX$Year == "97"] <- "1997"

table(FOX$Cache)
FOX$Cache <- as.character(FOX$Cache)
FOX$Cache[FOX$Cache == "-"] <- NA
FOX$Cache[FOX$Cache == "abbitibi"] <- "ABI"
FOX$Cache[FOX$Cache == "goose garden" | FOX$Cache == "GOOSE_GARDEN"] <- "GG"
FOX$Cache[FOX$Cache == "peksek" | FOX$Cache == "PEKSEK"] <- "PK"

table(as.character(FOX$Habitat), useNA = "always")
FOX$Habitat[FOX$Habitat == "LIM HM" | FOX$Habitat == "H et M" | FOX$Habitat == "" | FOX$Habitat == "neige"] <- NA
FOX$Habitat[FOX$Habitat == "H" | FOX$Habitat == "wetland" | FOX$Habitat == "prairie humide" | FOX$Habitat == "polygones"] <- "WET"
FOX$Habitat[FOX$Habitat == "M" | FOX$Habitat == "mes" | FOX$Habitat == "mesic" | FOX$Habitat == "mesique"] <- "MES"
# Add climatic variables

FOX[c("Year", "Cache", "Habitat")] <- lapply(FOX[c("Year", "Cache", "Habitat")] , as.factor)
FOX <- droplevels(FOX)
summary(FOX)

#### Addition of other variables ####

lmg <- read.csv("LEM_1993-2017.txt", sep = "\t", dec = ",")
breed <- read.csv("GOOSE_breeding_informations_1995_2017.txt", sep = "\t", dec = ",", h = T)
clim.day <- read.table("FOX_climate_data_daily_range.txt", sep = "\t", h = T)


head(lmg)
FOX$lmg <- lmg$LMG_C1_CORR[match(FOX$Year, lmg$YEAR)]
FOX$lmg.year <- lmg$LMG_YEAR[match(FOX$Year, lmg$YEAR)]

head(breed)
FOX$nest.density <- breed$NEST_DENSITY[match(FOX$Year, breed$YEAR)]

head(clim.day)
FOX$Year <- as.numeric(as.character(FOX$Year))
FOX <- merge(FOX, clim.day[,c(1,2, 8:11)], all.x = TRUE, by.x = c("Year", "Date"), by.y = c("year", "date"))

head(FOX)
summary(FOX)

#write.table(FOX, "FOX_atq.per.obs_clim.txt", sep = "\t")
