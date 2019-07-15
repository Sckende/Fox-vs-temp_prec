####### Calcul des moyennes d'attaque PAR INDIVIDU PAR OBSERVATION ######
#### Tentative de partir des fichiers brutes de time budget ####

getwd ()
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
library("reshape2")
library(dplyr)
rm( list = ls ())
list.files()


######################## Fox - 2004 ###########################
# Loading database
comp.2004 <- read.table("FOXBASE_BEHAV_2004_2005.txt", sep = "\t", h = T) %>%
  filter(YEAR == 2004) %>%
  droplevels

summary(comp.2004)
head(comp.2004)

# Check if needed informations about observation hours are present
lapply(comp.2004[, c(6, 7, 11, 12)], table, useNA = "always") # NAs for START.OBS & END.OBS
comp.2004[is.na(comp.2004$START.OBS),] # Observation in day 175 - BLOC 22h00/00h00

# Addition of an approximative hour for the missing data
comp.2004$START.OBS <- as.character(comp.2004$START.OBS)
comp.2004$END.OBS <- as.character(comp.2004$END.OBS)

comp.2004$START.OBS[is.na(comp.2004$START.OBS)] <- "22h00"
comp.2004$END.OBS[is.na(comp.2004$END.OBS)] <- "22h10"

# Checking ID levels
levels(comp.2004$FOX.ID)

# Checking behaviours levels
levels(comp.2004$BEHAV)

# Checking duration of observation
table(comp.2004$OBS.LENGTH, useNA = "always")

# Checking items which are predated and selection of which ones I want to keep
levels(comp.2004$ITEM[comp.2004$BEHAV == "attaque"]) # keep items: couple, egg, lemming, oie, young
table(comp.2004$ITEM[comp.2004$BEHAV == "attaque"], useNA = "always")



# -------- #

comp.2004$BEHAV <- as.character(comp.2004$BEHAV)
comp.2004$BEHAV[comp.2004$BEHAV == "attaque" & comp.2004$ITEM %in% c("couple", "egg", "oie", "young")] <- "ATQ"
table(comp.2004$ITEM[comp.2004$BEHAV == "ATQ"], useNA = "always")
atq.all.2004 <- comp.2004 %>% 
  group_by(YEAR, CACHE, DATE, FOX.ID, OBS.LENGTH, OBSERVER) %>%
  summarise(start.bloc = unique(START.BLOC),
            end.bloc = unique(END.BLOC),
            bloc.length = unique(BLOC.LENGTH),
            start.obs = unique(START.OBS),
            atq.all.number = sum(BEHAV == "ATQ", na.rm = T),
            atq.all.rate = sum(BEHAV == "ATQ", na.rm = T)/unique(OBS.LENGTH)) %>% 
  arrange(DATE)


comp.2004$BEHAV[comp.2004$BEHAV == "ATQ" & !(comp.2004$ITEM %in% c("couple", "oie"))] <- "attaque"
table(comp.2004$ITEM[comp.2004$BEHAV == "ATQ"], useNA = "always")
atq.goo.2004 <- comp.2004 %>% 
  group_by(YEAR, CACHE, DATE, FOX.ID, OBS.LENGTH, OBSERVER) %>%
  summarise(start.bloc = unique(START.BLOC),
            end.bloc = unique(END.BLOC),
            bloc.length = unique(BLOC.LENGTH),
            start.obs = unique(START.OBS),
            atq.goo.number = sum(BEHAV == "ATQ", na.rm = T),
            atq.goo.rate = sum(BEHAV == "ATQ", na.rm = T)/unique(OBS.LENGTH)) %>% 
  arrange(DATE)

sum(atq.all.2004$atq.all.number)
sum(atq.goo.2004$atq.goo.number)

# -------- #
atq.2004 <- left_join(atq.all.2004, atq.goo.2004, by = c("YEAR", "CACHE", "DATE", "FOX.ID", "OBS.LENGTH", "OBSERVER", "start.bloc", "end.bloc", "start.obs", "bloc.length"))
summary(atq.2004)
#X11()
par(mfrow = c(1, 2))
plot(atq.2004$DATE, atq.2004$atq.all.rate, bty = "n")
plot(atq.2004$DATE, atq.2004$atq.goo.rate, bty = "n")

# ------- BLOC LENGTH ------ #
atq.2004$new.bloc <- NA

atq.2004$new.bloc[as.numeric(substring(atq.2004$start.obs, 1, 2)) < 4] <- "00h00-04h00"
atq.2004$new.bloc[as.numeric(substring(atq.2004$start.obs, 1, 2)) >= 4 & as.numeric(substring(atq.2004$start.obs, 1, 2)) < 8] <- "04h00-08h00"
atq.2004$new.bloc[as.numeric(substring(atq.2004$start.obs, 1, 2)) >= 8 & as.numeric(substring(atq.2004$start.obs, 1, 2)) < 12] <- "08h00-12h00"
atq.2004$new.bloc[as.numeric(substring(atq.2004$start.obs, 1, 2)) >= 12 & as.numeric(substring(atq.2004$start.obs, 1, 2)) < 16] <- "12h00-16h00"
atq.2004$new.bloc[as.numeric(substring(atq.2004$start.obs, 1, 2)) >= 16 & as.numeric(substring(atq.2004$start.obs, 1, 2)) < 20] <- "16h00-20h00"
atq.2004$new.bloc[as.numeric(substring(atq.2004$start.obs, 1, 2)) >= 20] <- "20h00-00h00"
# Here, I consider that all observation blocs = 4 h
atq.2004$bloc.length <- 4
atq.2004$start.bloc <- substring(atq.2004$new.bloc, 1, 5)
atq.2004$end.bloc <- substring(atq.2004$new.bloc, 7, 11)
atq.2004 <- atq.2004[, 1:14]
utils::View(atq.2004)


######################## Fox - 2005 ###########################
# Loading database
comp.2005 <- read.table("FOXBASE_BEHAV_2004_2005.txt", sep = "\t", h = T) %>%
  filter(YEAR == 2005) %>%
  droplevels()
summary(comp.2005)

# Checking ID levels
levels(comp.2005$FOX.ID)
# Checking behaviours levels
levels(comp.2005$BEHAV)
# Checking duration of observation
table(comp.2005$OBS.LENGTH, useNA = "always")
# Checking items which are predated and selection of which ones I want to keep
table(comp.2005$ITEM[comp.2005$BEHAV == "attaque"], useNA = "always") # keep items: couple, egg, lemming, oie, young
table(comp.2005$ITEM,useNA = "always")

# -------- #

comp.2005$BEHAV <- as.character(comp.2005$BEHAV)
comp.2005$BEHAV[comp.2005$BEHAV == "attaque" & comp.2005$ITEM %in% c("couple", "egg", "oie", "young")] <- "ATQ"
table(comp.2005$ITEM[comp.2005$BEHAV == "ATQ"], useNA = "always")
atq.all.2005 <- comp.2005 %>% 
  group_by(YEAR, CACHE, DATE, FOX.ID, OBS.LENGTH, OBSERVER) %>%
  summarise(start.bloc = unique(START.BLOC),
            end.bloc = unique(END.BLOC),
            bloc.length = unique(BLOC.LENGTH),
            start.obs = NA,
            atq.all.number = sum(BEHAV == "ATQ", na.rm = T),
            atq.all.rate = sum(BEHAV == "ATQ", na.rm = T)/unique(OBS.LENGTH)) %>% 
  arrange(DATE)


comp.2005$BEHAV[comp.2005$BEHAV == "ATQ" & !(comp.2005$ITEM %in% c("oie", "couple"))] <- "attaque"
table(comp.2005$ITEM[comp.2005$BEHAV == "ATQ"], useNA = "always")
atq.goo.2005 <- comp.2005 %>% 
  group_by(YEAR, CACHE, DATE, FOX.ID, OBS.LENGTH, OBSERVER) %>%
  summarise(start.bloc = unique(START.BLOC),
            end.bloc = unique(END.BLOC),
            bloc.length = unique(BLOC.LENGTH),
            start.obs = NA,
            atq.goo.number = sum(BEHAV == "ATQ", na.rm = T),
            atq.goo.rate = sum(BEHAV == "ATQ", na.rm = T)/unique(OBS.LENGTH)) %>% 
  arrange(DATE)

sum(atq.all.2005$atq.all.number)
sum(atq.goo.2005$atq.goo.number)

# -------- #
atq.2005 <- left_join(atq.all.2005, atq.goo.2005, by = c("YEAR", "CACHE", "DATE", "FOX.ID", "OBS.LENGTH", "OBSERVER", "start.bloc", "end.bloc", "bloc.length", "start.obs"))
#utils::View(atq.2005)
summary(atq.2005)
#X11()
par(mfrow = c(1, 2))
plot(atq.2005$DATE, atq.2005$atq.all.rate, bty = "n")
plot(atq.2005$DATE, atq.2005$atq.goo.rate, bty = "n")

#### ---- ATQ DB ---- #### 
atq <- rbind(atq.2004, atq.2005) # warnings due to factor levels
atq$YEAR <- as.factor(atq$YEAR)
summary(atq)

#### ---- DEAL WITH BLOCS > 4H & < 4H ---- #### 
table(atq$bloc.length)
hist(atq$bloc.length)

# Blocs > 4H
atq$YEAR <- as.numeric(as.character(atq$YEAR))
nrow(atq[atq$bloc.length > 6,])
nrow(atq[atq$bloc.length < 6,])

atq <- atq[atq$bloc.length < 6,]
table(atq$YEAR, atq$bloc.length)

# Blocs < 4h

test <- split(atq, atq$bloc.length < 4)

test[[2]]$new.bloc <- NA

test[[2]]$new.bloc[as.numeric(substring(test[[2]]$start.bloc, 1, 2)) < 4] <- "00h00-04h00"
test[[2]]$new.bloc[as.numeric(substring(test[[2]]$start.bloc, 1, 2)) >= 4 & as.numeric(substring(test[[2]]$start.bloc, 1, 2)) < 8] <- "04h00-08h00"
test[[2]]$new.bloc[as.numeric(substring(test[[2]]$start.bloc, 1, 2)) >= 8 & as.numeric(substring(test[[2]]$start.bloc, 1, 2)) < 12] <- "08h00-12h00"
test[[2]]$new.bloc[as.numeric(substring(test[[2]]$start.bloc, 1, 2)) >= 12 & as.numeric(substring(test[[2]]$start.bloc, 1, 2)) < 16] <- "12h00-16h00"
test[[2]]$new.bloc[as.numeric(substring(test[[2]]$start.bloc, 1, 2)) >= 16 & as.numeric(substring(test[[2]]$start.bloc, 1, 2)) < 20] <- "16h00-20h00"
test[[2]]$new.bloc[as.numeric(substring(test[[2]]$start.bloc, 1, 2)) >= 20] <- "20h00-00h00"

# Here, I consider that all observation blocs = 4 h
test[[2]]$bloc.length <- 4
test[[2]]$start.bloc <- substring(test[[2]]$new.bloc, 1, 5)
test[[2]]$end.bloc <- substring(test[[2]]$new.bloc, 7, 11)
test[[2]] <- test[[2]][, 1:14]

atq <- do.call("rbind", test)
summary(atq)
hist(atq$bloc.length)

#### ------- graphical exploration -------- ####
X11()
atq$YEAR <- as.factor(atq$YEAR)
par(mfrow = c(1, 2))
plot(atq$DATE, atq$atq.all.rate, col = c("blue", "red")[as.numeric(atq$YEAR)] ,bty = "n")
plot(atq$DATE, atq$atq.goo.rate, col = c("blue", "red")[as.numeric(atq$YEAR)] ,bty = "n")

i <- hist(atq$atq.all.rate[atq$YEAR == "2004"], plot = F)
j <- hist(atq$atq.goo.rate[atq$YEAR == "2005"], plot = F)

x11()
#par(mfrow = c(1, 2))
hist(atq$atq.all.rate[atq$YEAR == "2004"], col = rgb(1,0,0,alpha=0.5), xlim = c(0, 0.020),breaks = seq(0,0.02, 0.0005))
hist(atq$atq.goo.rate[atq$YEAR == "2005"], col = rgb(0,0,1,alpha=0.5), breaks = seq(0,0.02, 0.0005), add = T)

#### ---- ASSOCIATION WITH MEAN TEMPERATURE PER BLOC ---- #### 

















######################## Fox - 1996-1999 ###########################
