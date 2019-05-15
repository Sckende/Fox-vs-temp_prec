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
temp <- temp[temp$year %in% c(1996:1999, 2004, 2005, 2015:2017) & temp$month %in% c(7, 8),]
summary(temp)
temp$temp <- as.numeric(as.character(temp$temp))

temp.var <- split(temp, paste(temp$year, temp$month, temp$day))
temp.var <- lapply(temp.var, function(x){
  station <- x$station[1]
  lat <- x$lat[1]
  long <- x$long[1]
  year <- x$year[1]
  month <- x$month[1]
  day <- x$day[1]
  max.daily.temp <- max(x$temp, na.rm = TRUE)
  min.daily.temp <- min(x$temp, na.rm = TRUE)
  
  x <- c(station = as.character(station),
         lat = lat,
         long = long,
         year = year,
         month = month,
         day = day,
         max.daily.temp = max.daily.temp,
         min.daily.temp = min.daily.temp)
})

all.clim <- do.call("rbind", temp.var)
all.clim <- as.data.frame(all.clim)
head(all.clim)
all.clim[2:8] <- lapply(all.clim[2:8], function (x){
  x <- as.numeric(as.character(x))
})
summary(all.clim)

#### Wind bloc ####

wind <- read.table("WIND_5min_speed_1993_2018.txt", sep = "\t", h = T)
wind <- wind[wind$year %in% c(1996:1999, 2004, 2005, 2015:2017) & wind$month %in% c(7, 8),]
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
  max.daily.speed <- max(x$wind.speed, na.rm = TRUE)
  
  x <- c(station = station,
         lat = lat,
         long = long,
         year = year,
         month = month,
         day = day,
         max.daily.speed = max.daily.speed)
  
})
wind.max[[1]]

wind.max <- as.data.frame(do.call("rbind", wind.max))
head(wind.max)
wind.max[2:7] <- lapply(wind.max[2:7], function (x){
  x <- as.numeric(as.character(x))
})
summary(wind.max)

all.clim <- merge(all.clim, wind.max, all = TRUE) # automatic merge depending on the same columns (with the same name) / all = TRUE to keep all possible combinaisons and creation of NA if values don't exist 
summary(all.clim)


#### Humidity bloc ####
# Max & min humidity per day

hum <- read.table("HUM_hourly_BYLCAMP_1993_2018.txt", sep = "\t", h = T)
head(hum)
hum <- hum[hum$year %in% c(1996:1999, 2004, 2005, 2015:2017) & hum$month %in% c(7, 8),]
summary(hum)
hum$hum <- as.numeric(as.character(hum$hum))

hum.var <- split(hum, paste(hum$year, hum$month, hum$day))

hum.var <- lapply(hum.var, function(x){
  station <- as.character(x$station[1])
  lat <- x$lat[1]
  long <- x$long[1]
  year <- x$year[1]
  month <- x$month[1]
  day <- x$day[1]
  max.hum <- max(x$hum)#, na.rm = TRUE)
  min.hum <- min(x$hum)#, na.rm = TRUE)
  
  x <- c(station = station,
         lat = lat,
         long = long,
         year = year,
         month = month,
         day = day,
         max.hum = max.hum,
         min.hum = min.hum)
  
})

hum.var <- as.data.frame(do.call("rbind", hum.var))
hum.var[2:8] <- lapply(hum.var[2:8], function(x){
  x <- as.numeric(as.character(x))
})
summary(hum.var)

all.clim <- merge(all.clim, hum.var, all = TRUE)
summary(all.clim)


#### Precipitation bloc ####

prec <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", h = T, dec = ",")
summary(prec)

head(prec)
names(prec)[c(1, 3)] <- c("year", "rain")
prec <- prec[prec$year %in% c(1996:1999, 2004, 2005, 2015:2017),]


head(all.clim)
all.clim$JJ <- paste(all.clim$day, all.clim$month, all.clim$year, sep = "-")
all.clim$JJ <- as.Date(all.clim$JJ, format = "%d-%m-%Y")
all.clim$JJ <- as.integer(format(all.clim$JJ, "%j"))


all.clim <- merge(all.clim, prec, all.x = TRUE, by.x = c("year", "JJ"), by.y = c("year", "JJ"))
summary(all.clim)


#write.table(all.clim, "FOX_climate_data_daily_range.txt", sep = "\t")
