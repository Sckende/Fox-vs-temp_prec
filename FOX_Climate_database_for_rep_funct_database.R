#### Script for the pre-treatment of climate variables ####
# Dataframe creation with
    # Max temperature per day
    # Min temperature per day
    # Mean temperature per day
    # Max wind speed per day
    # Max humidity per day
    # Min humidity per day

    # Precipitation per day

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
