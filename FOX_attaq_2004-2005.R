## Script to compute attack rate of foxes in 2004-2005
## Data intented for path analysis

getwd()
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")

rm(list = ls())

#read.csv(file.choose())
fo <- read.table("fox_attaq_2004-2005.txt", h = T, sep = "\t")
summary(fo)
levels(fo$fox)
levels(fo$item)

# "cache" in item variable means foxes targeted ("attacked") food caches
# selection of attack on goose items only ("couple", "egg", "oie", "young")
fo1 <- fo[(fo$item == "couple" | fo$item == "egg" | fo$item == "oie" | fo$item == "young"),]
summary(fo1)
fo1$year_date <- paste (fo1$year, fo1$date, sep = "_"); fo1$year_date <- as.factor(fo1$year_date)

date <- unique(fo1$year_date)
count <- 0
TAB <- NULL

  for (i in unique(date)) {
    j <- unique(fo1$fox[fo1$year_date == i])
    if (fo1$behav[fo1$year_fox == j] == "attaque") {
      count <- count + 1
      print(count)
    }
  }
levels(fo1$year_fox)
################################# BROUILLON ####################################
fo2 <- fo1[fo1$year_date =="2004_183",]
summary(fo2)
