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

max.temp <- read.table("TEMP_daily_maxi_BYLCAMP_1993_2018.txt", sep = "\t", h = T)
summary(max.temp)

min.temp <- read.table("TEMP_daily_mini_BYLCAMP_1993_2018.txt", sep = "\t", h = T)
summary(min.temp)

mean.temp <- read.table("TEMP_daily_moy_BYLCAMP_1994_2018.txt", sep = "\t", h = T)
summary(mean.temp)

wind <- read.table("WIND_5min_speed_1993_2018.txt", sep = "\t", h = T)
summary(wind)
wind$wind_speed[wind$wind_speed == "RE-99999.00000"] <- NA
wind$wind_speed <- as.numeric(as.character(wind$wind_speed))

hum <- read.table("HUM_hourly_BYLCAMP_1993_2018.txt", sep = "\t", h = T)
summary(hum)
hum$hum[hum$hum == "RE-99999.00000"] <- NA
hum$hum[hum$hum == "M-99999.00000"] <- NA
hum$hum <- as.numeric(as.character(hum$hum))

prec <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", h = T, dec = ",")
summary(prec)

#### Temperature bloc ####

max.temp <- max.temp[max.temp$year %in% c(1996:1999, 2004, 2005, 2015:2017) & max.temp$month %in% c(7, 8),]
min.temp <- min.temp[min.temp$year %in% c(1996:1999, 2004, 2005, 2015:2017) & min.temp$month %in% c(7, 8),]
mean.temp <- mean.temp[mean.temp$year %in% c(1996:1999, 2004, 2005, 2015:2017) & mean.temp$month %in% c(7, 8),]

all.clim <- merge(max.temp, min.temp, all = TRUE) # automatic merge depending on the same columns (with the same name) / all = TRUE to keep all possible combinaisons and creation of NA if values don't exist 

all.clim <- merge(all.clim, mean.temp, all = TRUE)

#### Wind bloc ####
head(wind)
wind <- wind[wind$year %in% c(1996:1999, 2004, 2005, 2015:2017) & wind$month %in% c(7, 8),]


wind.max <- split(wind, paste(wind$year, wind$month, wind$day))

wind.max <- lapply(wind.max, function(x){
  station <- as.character(x$station[1])
  lat <- x$lat[1]
  long <- x$long[1]
  year <- x$year[1]
  month <- x$month[1]
  day <- x$day[1]
  max.speed <- max(x$wind_speed)
  
  x <- c(station = station,
                     lat = lat,
                     long = long,
                     year = year,
                     month = month,
                     day = day,
                     max.speed = max.speed)
  
})
wind.max[[1]]

wind.max <- as.data.frame(do.call("rbind", wind.max))
head(wind.max)
wind.max[2:7] <- lapply(wind.max[2:7], function (x){
  x <- as.numeric(as.character(x))
})
summary(wind.max)

all.clim <- merge(all.clim, wind.max, all = TRUE)
summary(all.clim)

#### Humidity bloc ####
    # Max & min humidity per day
head(hum)
hum <- hum[hum$year %in% c(1996:1999, 2004, 2005, 2015:2017) & hum$month %in% c(7, 8),]


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
head(prec)
names(prec)[c(1, 3)] <- c("year", "rain")
prec <- prec[prec$YEAR %in% c(1996:1999, 2004, 2005, 2015:2017),]

head(all.clim)
all.clim$JJ <- paste(all.clim$day, all.clim$month, all.clim$year, sep = "-")
all.clim$JJ <- as.Date(all.clim$JJ, format = "%d-%m-%Y")
all.clim$JJ <- as.integer(format(all.clim$JJ, "%j"))


all.clim <- merge(all.clim, prec, all = TRUE)
summary(all.clim)


#write.table(all.clim, "FOX_climate_data.txt")
