getwd()
setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")

atq<-read.table("attaq_rate_1996-2016.txt", h=T, sep="\t",dec=",")
summary(atq)

unique(atq$year)

####Data exploration####
#dotchart(atq$attack, ylab = "order of observation",xlab ="attack", main = "Cleveland dotplot")
#dotchart(atq$attack,groups = factor(atq$year), ylab = "years",xlab ="attack", main = "Cleveland dotplot")

#pairs(atq)

#boxplot(attack~factor(year),varwidth = TRUE, xlab = "year",main = "Boxplot of attack", ylab = "attack", data = atq)

#library(lattice)
#xyplot(attack~date | factor(year), type = "l",xlab = "date", col = 1,ylab = "observed attack number",
         #strip = function(bg = 'white', ...)
          # strip.default(bg = 'white', ...),data = atq)


####attack rate compute####
atq$atq_rate<-(atq$attack/atq$obs_length)*(3600*24)#Daily attack rate
summary(atq)
#retrait des obsertions courtes < 3 minutes 
atq<-atq[atq$obs_length>=180,]
summary(atq)

#dotchart(atq$atq_rate, ylab = "order of observation",xlab ="attack rate", main = "Cleveland dotplot")
#dotchart(atq$attack,groups = factor(atq$year), ylab = "years",xlab ="attack", main = "Cleveland dotplot")
#pairs(atq)

#boxplot(atq_rate~factor(year),varwidth = TRUE, xlab = "year",
        #main = "Boxplot of attack", ylab = "attack rate", data = atq)

#library(lattice)
#xyplot(atq_rate~date | factor(year), type = "l",xlab = "date", col = 1,ylab = "observed attack number",
       #strip = function(bg = 'white', ...)
#strip.default(bg = 'white', ...),data = atq)

####Extraction de données pour comparaison interannuelle####
#extraction des jours d'observations min et max par année

require(plotrix)
j<-unique(atq$year)#ici j'ai enlevé l'année 2016 car il manque les données météo correspondantes
TAB<-NULL
for (i in j){
  h<-subset(atq,atq$year==i)
  YEAR<-i
  MinDay_atq<-min(h$date)
  MaxDay_atq<-max(h$date)
  MoyAtq<-mean(h$atq_rate)
  SEAtq<-std.error(h$atq_rate)
  r<-data.frame(YEAR,MinDay_atq,MaxDay_atq,MoyAtq,SEAtq)

  TAB<-rbind(TAB,r)
}

TABa<-TAB[TAB$YEAR<=1999,]
TABb<-TAB[TAB$YEAR==2004 |TAB$YEAR==2005,]
TABc<-TAB[TAB$YEAR>=2015,]
summary(TABb)
par(bg="transparent")
plot(TABa$YEAR,TABa$MoyAtq, type = "l",ylim = c(155,495), xlim=c(1996,2016))
plot(TABb$YEAR,TABb$MoyAtq, type = "l",ylim = c(155,495), xlim=c(1996,2016))
plot(TABc$YEAR,TABc$MoyAtq, type = "l",ylim = c(155,495), xlim=c(1996,2016))

#TAB<-TAB[-8,]#car pas toutes les temperatures pour l'annee 2016 :-(

#temp<-read.table("temperature_pond_inlet_1996-2015.txt", h=T, sep="\t", dec=",")
temp<-read.table("TEMP-BYLCAMP.txt", h=T, sep="\t", dec=".")
summary(temp)

temp[temp$YEAR==2016,]
#temp<-na.omit(temp)

#transformation date en jour julien#
install.packages("date")
require(date)

temp$YMD<-paste(temp$YEAR,temp$MONTH,temp$DAY,sep = "-")
temp$YMD<-strptime(temp$YMD, format = "%Y-%m-%d")
temp$JJ<-format(temp$YMD,format = "%j")
temp$JJ<-as.numeric(temp$JJ)


#extraction des températures correspondantes aux jours d'observation par année et moyenne
#attention, ici j'ai utilisé les températures de la station de Bylot et non pas celles de Pond Inlet
#comme c'était le cas pour les précédents GAM effectués
#require(plotrix)
atq2<-atq[atq$year<2016,]
b<-unique(atq2$year)

TAB1<-NULL
for (i in b){
YEAR<-i
k<-subset(temp,temp$YEAR==i)
mini<-TAB$MinDay_atq[TAB$YEAR==i]
maxi<-TAB$MaxDay_atq[TAB$YEAR==i]
Temperature<-mean(k$TEMP[k$JJ>=mini & k$JJ<=maxi])
SETemperature<-std.error(k$TEMP[k$JJ>=mini & k$JJ<=maxi])
MinTemp<-min(k$TEMP[k$JJ>=mini & k$JJ<=maxi])
MaxTemp<-max(k$TEMP[k$JJ>=mini & k$JJ<=maxi])

p<-data.frame(YEAR,mini, maxi, Temperature, SETemperature, MinTemp, MaxTemp)

TAB1<-rbind(TAB1,p)
}
TAB1

#####extraction des précipitations correspondantes aux jours d'observation par année et moyenne#####
prec<-read.table("precipitation_Bylot_1996-2016.txt", h=T, dec=",", sep="\t")
summary(prec)


TAB2<-NULL
for (i in j){
  YEAR<-i
  ka<-subset(prec,prec$year==i)
  mini<-TAB$MinDay_atq[TAB$YEAR==i]
  maxi<-TAB$MaxDay_atq[TAB$YEAR==i]
  Precipitation<-mean(ka$rain[ka$day>=mini & ka$day<=maxi])
  SEPrecipitation<-std.error(ka$rain[ka$day>=mini & ka$day<=maxi])
  MinPrec<-min(ka$rain[ka$day>=mini & ka$day<=maxi])
  MaxPrec<-max(ka$rain[ka$day>=mini & ka$day<=maxi])
  
  pa<-data.frame(YEAR,mini, maxi, Precipitation, SEPrecipitation,MinPrec,MaxPrec)
  
  TAB2<-rbind(TAB2,pa)
}
TAB2

min(atq$date[atq$year==1998])
max(atq$date[atq$year==1998])
#####acquisition rate vs precipitation######
#min(TAB2$Precipitation);max(TAB2$Precipitation);min(TAB$MoyAtq);max(TAB$MoyAtq)#pour limite de mes axes
#par(mfrow=c(2,2))
#plot(TAB2$Precipitation,TAB$MoyAtq,xlab = "Mean rainfall (mm)", ylab = "Mean daily attack rate",
     #xlim = c(0, 2.5),ylim = c(230, 590))

require(mgcv)
M3 <- gam(TAB$MoyAtq ~ s(TAB2$Precipitation, fx = FALSE, k=6,bs = "cr"))#k pour knots, k=3 recommandé pour n<10
summary(M3);AIC(M3)
coef(M3)

col2rgb(col="blue")


par(bg=NA)#fond de graphique transparent
plot(TAB2$Precipitation,TAB$MoyAtq,xlab = "Mean rainfall (mm)", ylab = "Mean daily attack rate",
xlim = c(0, 2.5),ylim = c(230, 590),col="blue",lwd=4,cex.axis=1.8,pch=16,cex=2)#nuage de points
par(new=T)#pour superposition d'un nouveau graphique
par(bg=NA)
couleur<-rgb(0,0,0.5,0.2)#définition d'une couleur pour l'intervalle de confiance
plot.gam(M3,shade = T,shade.col =couleur,pers = T,xlab = "",yaxt="n",ylab="",xaxt="n",lwd=4)#plot de la smooth curve et de l'intervalle de confiance à 95% associée


#plot(M3, se = TRUE,xlab = "",yaxt="n",ylab="",xaxt="n",col="blue",lwd=4)#deuxième commande pour plot de smooth curve


gam.check(M3)#évaluer la normalité, l'homogénéité et valeurs fittées vs valeurs observées

#Troisième méthode
M3pred <- predict(M3, se = TRUE, type = "response")
M3pred
require(ggplot2)
par(bg=NA);
plot(TAB2$Precipitation, TAB$MoyAtq, type = "p",xlab = "Mean rainfall (mm)", ylab = "Mean daily attack rate",
      xlim = c(0, 2.5),ylim = c(230, 590),cex.axis=1.8,cex.lab=1.8,cex=2,col="blue")
#qplot(TAB2$Precipitation, TAB$MoyAtq,xlab = "Mean rainfall (mm)", ylab = "Mean daily attack rate",
     #xlim = c(0, 2.5),ylim = c(230, 590),par(bg=NA))
I1 <- order(TAB2$Precipitation)#to avoid a spaghetti plot
par(bg=NA);lines(TAB2$Precipitation[I1], M3pred$fit[I1], lty=1, col="blue",lwd=3)
par(bg=NA);lines(TAB2$Precipitation[I1], M3pred$fit[I1]+2*M3pred$se[I1],lty=2,col="blue",lwd=3)
par(bg=NA);lines(TAB2$Precipitation[I1], M3pred$fit[I1]-2*M3pred$se[I1],lty=2,col="blue",lwd=3)
#max(M3pred$fit[I1]+2*M3pred$se[I1])
#min(M3pred$fit[I1]-2*M3pred$se[I1])

#####attack rate vs temperature######
#min(TAB1$Temperature);max(TAB1$Temperature);min(TAB$MoyAtq);max(TAB$MoyAtq)#pour limite de mes axes
#par(mfrow=c(2,2))
#plot(TAB1$Temperature,TAB$MoyAtq,xlab = "Mean temperature (°c)", ylab = "Mean daily attack rate",
     #xlim = c(5, 8.5),ylim = c(230, 590))

require(mgcv)
M4 <- gam(TAB$MoyAtq ~ s(TAB1$Temperature, fx = FALSE, k=5,bs = "cr"))#meilleur AIC pour K=5, mais différence très légère
AIC(M4)
coef(M4)
summary(M4)

par(bg=NA)
par(mfrow=c(2,1))

plot(TAB1$Temperature, TAB$MoyAtq, type = "p",xlab = "Mean temperature (°c)", ylab = "Mean daily attack rate",
     xlim = c(2, 8.5),ylim = c(230, 590),col="brown2",cex.axis=1.8,cex.lab=1.8,cex=2,lwd=3,pch=16)
par(new=T)
plot(M4, se = TRUE,xlab = "",ylab = "",xaxt="n",yaxt="n",col="brown2",lwd=3)


M4pred <- predict(M4, se = TRUE, type = "response")
par(bg=NA);I1 <- order(TAB1$Temperature)#to avoid a spaghetti plot
par(bg=NA);lines(TAB1$Temperature[I1], M4pred$fit[I1], lty=1,col="brown2",lwd=3)
par(bg=NA);lines(TAB1$Temperature[I1], M4pred$fit[I1]+2*M4pred$se[I1],lty=2,col="brown2",lwd=3)
par(bg=NA);lines(TAB1$Temperature[I1], M4pred$fit[I1]-2*M4pred$se[I1],lty=2,col="brown2",lwd=3)

#max(M4pred$fit[I1]+2*M4pred$se[I1])
#min(M4pred$fit[I1]-2*M4pred$se[I1])







#Smooth curve method 1
plot(TAB1$Temperature,TAB$MoyAtq,xlab = "Température moyenne", ylab = "Taux d'attaque moyen",
     xlim = c(4,8),
     ylim = c(195, 675))

#require(Hmisc)
#TABB<-cbind(TAB,TAB1$Temperature)
#names(TABB)[6]<-"Temperature";names(TABB)
with ( data=TABB,expr = errbar(Temperature, MoyAtq,MoyAtq+SEAtq, 
                     MoyAtq-SEAtq, add=T, cap=.01))

lines(pp<-predict(SmoothingSpline),col="black")

SmoothingSpline1<-smooth.spline(TAB1$Temperature,TAB$MoyAtq,spar=0.5);lines(SmoothingSpline1,col="firebrick1")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.3);lines(SmoothingSpline, col="darkorange")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.4);lines(SmoothingSpline, col="gold")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.5);lines(SmoothingSpline, col="firebrick1")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.6);lines(SmoothingSpline, col="cyan4")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.7);lines(SmoothingSpline, col="red")

#Valeurs prédites
plot(TAB1$Temperature,TAB$MoyAtq,xlab = "Annual temperature", ylab = "Annual attack rate",
     xlim = c(4,8),
     ylim = c(195, 675))
#require(Hmisc)
with ( data=TABB,expr = errbar(Temperature, MoyAtq,MoyAtq+SEAtq, 
                               MoyAtq-SEAtq, add=T, cap=.01))
#require(splines)
spli<-interpSpline(TAB1$Temperature,TAB$MoyAtq);spli
#names(spli)
points(TAB1$Temperature,spli$coefficients[,1],col="red",pch=6)#plot des valeurs prédites
spli$coefficients

#Smooth curve method 3
require(ggplot2)
## Default loess smooth, with SE bands added.
#quickplot(TAB1$YEAR, TAB1$Temperature,data=TAB1,  geom=c("point","smooth"))
#plot(TAB1$Temperature,TAB$MoyAtq)
#abline(test0, col="green")
#test0<-lm(TAB$MoyAtq~TAB1$Temperature)
#summary(test0)
#sans le point de 2015

#plot(TAB1$Temperature[-7],TAB$MoyAtq[-7])
#abline(lm(TAB$MoyAtq[-7]~TAB1$Temperature[-7]), col="pink")
#test<-lm(TAB$MoyAtq[-7]~TAB1$Temperature[-7])
#summary(test)
#test$coefficients
#plot(test$residuals)

#Mixed effect modelling for nested data analysis#
#ATTENTION, À FAIRE AVEC LE JEUX DE DONNÉES INITIALES AVEC VARIABLE INDIVIDUS (=nested)
TABB<-cbind(TAB,TAB1$Temperature)
names(TABB)[6]<-"Temperature";names(TABB)
#Step 1:linear regression
mlm<-lm(MoyAtq~Temperature, data = TABB);mlm
par(mfrow=c(2,2));plot(mlm, select=c(1))#residuals graphical analysis
#Step 2:Fit the model with GLS, to compare linear regression model with mixed effects model
require(nlme)
form<-formula(MoyAtq~Temperature, data=TABB)
mgls<-gls(form, data=TABB);mgls#identical to that of the lm function
#Step 3:Choose a variance structure=finding optimal variance structure in terms of heterogeneity
#Step 4:fit the model
m1lme<-lme(form, random=~1|Nest, method="REML",data=TABB)



#####Plot atq_rate vs précipitation#####
#min(TAB2$Precipitation);max(TAB2$Precipitation);min(TAB$MoyAtq);max(TAB$MoyAtq)#pour limite de mes axes
plot(TAB2$Precipitation,TAB$MoyAtq,xlab = "Mean rainfall (mm)", ylab = "Attack rate",
     xlim = c(0, 2.5),
     ylim = c(0.002, 0.007))

SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.3);lines(SmoothingSpline, col="darkorange")
SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.4);lines(SmoothingSpline, col="gold")
SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.5);lines(SmoothingSpline, col="blue")
SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.6);lines(SmoothingSpline, col="coral1")


#plot(TAB2$Precipitation,TAB$MoyAtq)
#test1<-lm(TAB$MoyAtq~TAB2$Precipitation);summary(test1)
#abline(test1, col="violet")

#####Plot recapitulatif des courbes taux attaque vs températures/précipitations#####
par(mfrow=c(2,2))



qplot(TAB2$Precipitation,TAB$MoyAtq,geom = c("point","smooth"),xlab="Mean precipitation",ylab = "Attack rate")#, span=0.2)
qplot(TAB1$Temperature,TAB$MoyAtq,colour="red",geom = c("point","smooth"),xlab="Mean temperature",ylab = "Attack rate")#, span=0.2)


plot(TAB1$Temperature,TAB$MoyAtq,xlab = "Température moyenne", ylab = "Taux d'attaque moyen",
     xlim = c(5, 8),
     ylim = c(0.003, 0.007))


SmoothingSpline1<-smooth.spline(TAB1$Temperature,TAB$MoyAtq,spar=0.5);lines(SmoothingSpline1,col="firebrick1")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.3);lines(SmoothingSpline, col="darkorange")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.4);lines(SmoothingSpline, col="gold")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.5);lines(SmoothingSpline, col="firebrick1")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.6);lines(SmoothingSpline, col="coral1")
SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAtq, spar = 0.7);lines(SmoothingSpline, col="cyan4")

plot(TAB2$Precipitation,TAB$MoyAtq,xlab = "Précipitation moyenne", ylab = "Taux d'attaque moyen",
     xlim = c(0, 2.5),
     ylim = c(0.002, 0.007))

SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.3);lines(SmoothingSpline, col="darkorange")
SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.4);lines(SmoothingSpline, col="gold")
SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.5);lines(SmoothingSpline, col="firebrick1")
SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAtq, spar = 0.6);lines(SmoothingSpline, col="coral1")

#####Utilisation du package qqplot######
require(ggplot2)
qplot(TAB2$Precipitation,TAB$MoyAtq, geom = c("point","smooth"))#,span=0.5)

require(stats)
model<-glm(TAB$MoyAtq~(TAB1$Temperature*TAB2$Precipitation))
summary(model)


################daily attack rate vs daily temperature################################
setwd(dir = "/Users/nicolas/Desktop/Claire/R_analysis/Data")
atq<-read.table("attaq_rate_1996-2015.txt", h=T, sep="\t",dec=",")
atq$Y_JJ<-paste(atq$year,"_",atq$date)

temp<-read.table("temperature_pond_inlet_1996-2015.txt", h=T, sep="\t", dec=",")
summary(temp)


#transformation date en jour julien#
require(date)
class(temp$Date)
c<-temp$Date; c
c<-as.Date(c,format = "%d/%m/%Y")
b<-strptime(c, format = "%Y-%m-%d")
d<-format(b,format = "%j")

temp$JJ<-d
temp$JJ<-as.numeric(temp$JJ)
summary(temp)
temp$Y_JJ<-paste(temp$Y,"_",temp$JJ)

summary(atq)
atq$atq_rate<-atq$attack/atq$obs_length
dim(atq)
ind<-c(1:218)
atq<-cbind(atq,ind)
head(atq)
summary(atq)


TAB_all<-NULL
j<-atq$Y_JJ
for(i in j){
  JJ<-i
  temperMax<-temp$MaxTemp[temp$Y_JJ==i]
  temperMin<-temp$MinTemp[temp$Y_JJ==i]
  temperMoy<-temp$MoyTemp[temp$Y_JJ==i]
  indiv<-atq$ind[atq$Y_JJ==i]
  atq_R<-atq$atq_rate[atq$Y_JJ==i]
  chou<-data.frame(JJ,temperMin,temperMax,temperMoy,indiv,atq_R)
  
  TAB_all<-rbind(TAB_all,chou)
}
TAB_all
dim(TAB_all)



require(mgcv)
M4 <- gam(TAB_all$atq_R ~ s(TAB_all$temperMoy, fx = FALSE, k=6,bs = "cr"))#k=3 recommandé pour n<10
couleur<-rgb(0,0,0.5,0.2)
par(bg=NA)
plot.gam(M4,shade = T,shade.col =couleur,pers = T,xlab = "",yaxt="n",ylab="",xaxt="n",lwd=4) #étrange....
par(new=T)
plot(TAB_all$temperMoy,TAB_all$atq_R)

detach("package:mgcv")
require(gam)
M1 <- gam(TAB_all$atq_R~lo(TAB_all$temperMoy, span = 0.5))
plot(M1, se = TRUE)


detach("package:gam")
require(mgcv)
M3 <- gam(TAB_all$atq_R~s(TAB_all$temperMoy, fx = FALSE, k=-1,bs = "cr"))
plot(M3, se = TRUE)

################daily attack rate vs daily precipitation################################
#setwd(dir = "/Users/nicolas/Desktop/Claire/R_analysis/Data")
#atq<-read.table("attaq_rate_1996-2015.txt", h=T, sep="\t",dec=",")
#atq$Y_JJ<-paste(atq$year,"_",atq$date)

prec<-read.table("precipitation_Bylot_1996-2015.txt", h=T, sep="\t", dec=",")
summary(prec)

prec$Y_JJ<-paste(prec$year,"_",prec$day)

#summary(atq)
#atq$atq_rate<-atq$attack/atq$obs_length
#dim(atq)
#ind<-c(1:218)
#atq<-cbind(atq,ind)
#head(atq)
#summary(atq)


TAB_all2<-NULL
j<-atq$Y_JJ
for(i in j){
  JJ<-i
precMoy<-prec$rain[prec$Y_JJ==i]
  indiv<-atq$ind[atq$Y_JJ==i]
  atq_R<-atq$atq_rate[atq$Y_JJ==i]
  chou<-data.frame(JJ,precMoy,indiv,atq_R)
  
  TAB_all2<-rbind(TAB_all2,chou)
}
TAB_all2
dim(TAB_all2)



require(mgcv)
M4 <- gam(TAB_all2$atq_R ~ s(TAB_all2$precMoy, fx = FALSE, k=6,bs = "cr"))#k=3 recommandé pour n<10
couleur<-rgb(0,0,0.5,0.2)
par(bg=NA)
plot.gam(M4,shade = T,shade.col =couleur,pers = T,xlab = "",yaxt="n",ylab="",xaxt="n",lwd=4) #étrange....
par(new=T)
plot(TAB_all2$precMoy,TAB_all2$atq_R)

detach("package:mgcv")
require(gam)
M1 <- gam(TAB_all$atq_R~lo(TAB_all$temperMoy, span = 0.5))
plot(M1, se = TRUE)


detach("package:gam")
require(mgcv)
M3 <- gam(TAB_all$atq_R~s(TAB_all$temperMoy, fx = FALSE, k=-1,bs = "cr"))
plot(M3, se = TRUE)


#################################ZONE DE BROUILLON####################################
####Corrélation avec température####

temp<-read.table("temperature_pond_inlet.txt", h=T, sep="\t", dec=",")
summary(temp)
temp<-na.omit(temp)
library(lattice)
xyplot(MoyTemp~date | factor(year), rm.na = T, type = "l",
       xlab = "date", col = 1,
       ylab = "observed attack number",
       strip = function(bg = 'white', ...)
         strip.default(bg = 'white', ...),
       data = atq)

#transformation date en jour julien#
require(date)
class(temp$Date)
c<-temp$Date; c
c<-as.Date(c,format = "%d/%m/%Y")
b<-strptime(c, format = "%Y-%m-%d")
j<-format(b,format = "%j")

temp$JJ<-j
summary(temp)
temp$JJ<-as.numeric(temp$JJ)
