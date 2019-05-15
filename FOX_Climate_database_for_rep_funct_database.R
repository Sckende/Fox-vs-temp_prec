#### Script for the pre-treatment of climate variables ####
# Dataframe creation with
    # Max temperature per day
    # Min temperature per day
    # Mean temperature per day
    # Max wind speed per day
    # Max humidity per day
    # Min humidity per day

    # Precipitation per day

rm(list = ls())
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

#### Temperature bloc ####

temp <- read.table("TEMP_5min_temp_1993_2018.txt", sep = "\t", h = T)
head(temp)
temp$date <- as.Date(paste(temp$day, temp$month, temp$year, sep = "-"), format = "%d-%m-%Y")
temp$date <- as.integer(format(temp$date, "%j"))
temp <- temp[temp$year %in% c(1996:1999, 2004, 2005, 2015:2017) & temp$date %in% c(158:206),]
summary(temp)
temp$temp <- as.numeric(as.character(temp$temp))

# *** WARNINGS ! Missing values in 1998 between 158 to 181. Fox observations were done between 170 to 182. POssibility to use the Pond Inlet data only for this observation period *** #

# Here is for 8 years without 1998
temp <- temp[!temp$year == 1998,]
temp.var <- split(temp, paste(temp$year, temp$month, temp$day))
temp.var <- lapply(temp.var, function(x){
  station <- x$station[1]
  lat <- x$lat[1]
  long <- x$long[1]
  year <- x$year[1]
  month <- x$month[1]
  day <- x$day[1]
  date <- x$date[1]
  max.daily.temp <- max(x$temp, na.rm = TRUE)
  min.daily.temp <- min(x$temp, na.rm = TRUE)
  
  x <- c(station = as.character(station),
         lat = lat,
         long = long,
         year = year,
         month = month,
         day = day,
         date = date,
         max.daily.temp = max.daily.temp,
         min.daily.temp = min.daily.temp)
})

all.clim <- do.call("rbind", temp.var)
all.clim <- as.data.frame(all.clim)
head(all.clim)
all.clim[2:9] <- lapply(all.clim[2:9], function (x){
  x <- as.numeric(as.character(x))
})
summary(all.clim)

# Addition of temperature data for 1998, from Pond Inlet station
t.98 <- read.table("TEMP_PondInlet_JUIN_JUIL_1998.csv", sep = ";", h = T)
head(t.98)
names(t.98) <- c("year", "month", "day", "time", "temp")
t.98$date <- as.Date(paste(t.98$day, t.98$month, t.98$year, sep = "-"), format = "%d-%m-%Y")
t.98$date <- as.integer(format(t.98$date, "%j"))
summary(t.98)
t.98 <- t.98[t.98$date %in% c(158:206),]

t.98.var <- split(t.98, paste(t.98$year, t.98$month, t.98$day))
t.98.var <- lapply(t.98.var, function(x){
  year <- x$year[1]
  month <- x$month[1]
  day <- x$day[1]
  date <- x$date[1]
  max.daily.temp <- max(x$temp, na.rm = TRUE)
  min.daily.temp <- min(x$temp, na.rm = TRUE)
  
  x <- c(year = year,
         month = month,
         day = day,
         date = date,
         max.daily.temp = max.daily.temp,
         min.daily.temp = min.daily.temp)
})

t.98.var <- as.data.frame(do.call("rbind", t.98.var))
summary(t.98.var)
t.98.var$station <- "PONDINL"
t.98.var$lat <- 7269000
t.98.var$long <- -7797000

all.clim <- rbind(all.clim, t.98.var)
summary(all.clim)

#### Wind bloc ####

wind <- read.table("WIND_5min_speed_1993_2018.txt", sep = "\t", h = T)
wind$date <- as.Date(paste(wind$day, wind$month, wind$year, sep = "-"), format = "%d-%m-%Y")
wind$date <- as.integer(format(wind$date, "%j"))

wind <- wind[wind$year %in% c(1996:1999, 2004, 2005, 2015:2017) & wind$date %in% c(158:206),]
wind$wind.speed <- as.numeric(as.character(wind$wind.speed))
summary(wind)
wind[is.na(wind$wind.speed),]

wind.max <- split(wind, paste(wind$year, wind$month, wind$day))

wind.max <- lapply(wind.max, function(x){
  station <- as.character(x$station[1])
  lat <- x$lat[1]
  long <- x$long[1]
  year <- x$year[1]
  month <- x$month[1]
  day <- x$day[1]
  date <- x$date[1]
  max.daily.speed <- max(x$wind.speed, na.rm = TRUE)
  
  x <- c(station = station,
         lat = lat,
         long = long,
         year = year,
         month = month,
         day = day,
         date = date,
         max.daily.speed = max.daily.speed)
  
})
wind.max[[1]]

wind.max <- as.data.frame(do.call("rbind", wind.max))
head(wind.max)
wind.max[2:8] <- lapply(wind.max[2:8], function (x){
  x <- as.numeric(as.character(x))
})
summary(wind.max)

all.clim <- merge(all.clim, wind.max[,c(4, 7, 8)], all.x = TRUE, by.x = c("year", "date"), by.y = c("year", "date")) # automatic merge depending on the same columns (with the same name) / all.x = TRUE to keep all possible combinaisons with x and creation of NA if values don't exist 
summary(all.clim)


#### Humidity bloc ####
# Max & min humidity per day
# 
# hum <- read.table("HUM_hourly_BYLCAMP_1993_2018.txt", sep = "\t", h = T)
# head(hum)
# hum$date <- as.Date(paste(hum$day, hum$month, hum$year, sep = "-"), format = "%d-%m-%Y")
# hum$date <- as.integer(format(hum$date, "%j"))
# 
# hum <- hum[hum$year %in% c(1996:1999, 2004, 2005, 2015:2017) & hum$date %in% c(158:206),]
# 
# summary(hum)
# hum$hum <- as.numeric(as.character(hum$hum))
# 
# hum.var <- split(hum, paste(hum$year, hum$date))
# 
# hum.var <- lapply(hum.var, function(x){
#   station <- as.character(x$station[1])
#   lat <- x$lat[1]
#   long <- x$long[1]
#   year <- x$year[1]
#   month <- x$month[1]
#   day <- x$day[1]
#   date <- x$date[1]
#   max.hum <- max(x$hum, na.rm = TRUE)
#   min.hum <- min(x$hum, na.rm = TRUE)
#   
#   x <- c(station = station,
#          lat = lat,
#          long = long,
#          year = year,
#          month = month,
#          day = day,
#          date = date,
#          max.hum = max.hum,
#          min.hum = min.hum)
#   
# })
# 
# hum.var <- as.data.frame(do.call("rbind", hum.var))
# hum.var[2:9] <- lapply(hum.var[2:9], function(x){
#   x <- as.numeric(as.character(x))
# })
# summary(hum.var)
# 
# all.clim <- merge(all.clim, hum.var, all = TRUE)
# summary(all.clim)


#### Precipitation bloc ####

prec <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", h = T, dec = ",")
summary(prec)

head(prec)
names(prec)[c(1, 3)] <- c("year", "rain")
prec <- prec[prec$year %in% c(1996:1999, 2004, 2005, 2015:2017),]


head(all.clim)
all.clim <- merge(all.clim, prec, all.x = TRUE, by.x = c("year", "date"), by.y = c("year", "JJ"))
summary(all.clim)


#write.table(all.clim, "FOX_climate_data_daily_range.txt", sep = "\t")
