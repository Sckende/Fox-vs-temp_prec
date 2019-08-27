getwd()
setwd("/Users/nicolas/Desktop/Claire/R_analysis/Data")

####Chargement du fichier de temperatures de Pond Inlet####
temp<-read.table("temperature_pond_inlet_1996-2015.txt", h=T, sep="\t",dec=",")
summary(temp)
temp<-na.omit(temp)
#transformation date en jour julien#
require(date)
c<-temp$Date; c
c<-as.Date(c,format = "%d/%m/%Y")
b<-strptime(c, format = "%Y-%m-%d")
d<-format(b,format = "%j")

temp$JJ<-d
temp$JJ<-as.numeric(temp$JJ)
summary(temp)

####Chargement des donnees pour le calcul des taux d'acquisition####
acq<-read.table("acq_rate.txt",h=T,dec=",",sep="\t")
summary(acq)
unique(acq$year)
#calcul des taux d'acquisition pour chaque observation
acq$acq_rate<-(acq$egg/acq$obs_lenght)*(24*3600)
summary(acq)

#extraction des jours d'observations min et max par annee et moyenne acq rate par année
require(plotrix)
j<-unique(acq$year)
TAB<-NULL
for (i in j){
  h<-subset(acq,acq$year==i)
  YEAR<-i
  MinDay_acq<-min(h$date)
  MaxDay_acq<-max(h$date)
  MoyAcq<-mean(h$acq_rate)
  SEAcq<-std.error(h$acq_rate)
  r<-data.frame(YEAR,MinDay_acq,MaxDay_acq,MoyAcq,SEAcq)

  TAB<-rbind(TAB,r)
}
TAB
plot(TAB$YEAR,TAB$MoyAcq)

#extraction des temperatures correspondantes aux jours d'observation par annee et moyenne
TAB1<-NULL
for (i in j){
  YEAR<-i
  k<-subset(temp,temp$Y==i)
  mini<-TAB$MinDay_acq[TAB$YEAR==i]
  maxi<-TAB$MaxDay_acq[TAB$YEAR==i]
  Temperature<-mean(k$MoyTemp[k$JJ>=mini & k$JJ<=maxi])
  SETemperature<-std.error(k$MoyTemp[k$JJ>=mini & k$JJ<=maxi])

  p<-data.frame(YEAR,mini, maxi, Temperature,SETemperature)

  TAB1<-rbind(TAB1,p)
}
TAB1

#extraction des precipitations correspondantes aux jours d'observation par annee et moyenne
prec<-read.table("precipitation_Bylot_1996-2015.txt", h=T, dec=",", sep="\t")
prec<-na.omit(prec)
summary(prec)


TAB2<-NULL
for (i in j){
  YEAR<-i
  ka<-subset(prec,prec$year==i)
  mini<-TAB$MinDay_acq[TAB$YEAR==i]
  maxi<-TAB$MaxDay_acq[TAB$YEAR==i]
  Precipitation<-mean(ka$rain[ka$day>=mini & ka$day<=maxi])
  SEPrecipitation<-std.error(ka$rain[ka$day>=mini & ka$day<=maxi])

  pa<-data.frame(YEAR,mini, maxi, Precipitation,SEPrecipitation)

  TAB2<-rbind(TAB2,pa)
}
TAB2

#####acquisition rate vs precipitation######
#min(TAB2$Precipitation);max(TAB2$Precipitation);min(TAB$MoyAcq);max(TAB$MoyAcq)#pour limite de mes axes
#par(mfrow=c(2,2))
#plot(TAB2$Precipitation,TAB$MoyAcq,xlab = "Mean rainfall (mm)", ylab = "Mean daily acquisition rate",
      #xlim = c(0, 2.3),ylim = c(80, 405))

require(mgcv)
M3 <- gam(TAB$MoyAcq ~ s(TAB2$Precipitation, fx = FALSE, k=5,bs = "cr"))#meilleur AIC pour k=5 mais au dixième près
AIC(M3)
plot(M3, se = TRUE)
M3pred <- predict(M3, se = TRUE, type = "response")
par(bg=NA)
plot(TAB2$Precipitation, TAB$MoyAcq, type = "p",xlab = "Mean rainfall (mm)", ylab = "Mean daily acquisition rate",
     xlim = c(0, 2.3),ylim = c(80, 405),col="blue",cex.axis=1.8,cex.lab=1.8,cex=2)
I1 <- order(TAB2$Precipitation)#to avoid a spaghetti plot
par(bg=NA);lines(TAB2$Precipitation[I1], M3pred$fit[I1],col="blue", lty=1,lwd=3)
par(bg=NA);lines(TAB2$Precipitation[I1], M3pred$fit[I1]+2*M3pred$se[I1],col="blue",lty=2,lwd=3)
par(bg=NA);lines(TAB2$Precipitation[I1], M3pred$fit[I1]-2*M3pred$se[I1],lty=2,col="blue",lwd=3)
#max(M3pred$fit[I1]+2*M3pred$se[I1])
#min(M3pred$fit[I1]-2*M3pred$se[I1])

#####acquisition rate vs temperature######
#min(TAB1$Temperature);max(TAB1$Temperature);min(TAB$MoyAcq);max(TAB$MoyAcq)#pour limite de mes axes
#par(mfrow=c(2,2))
par(new=T)
par(bg=NA)
plot(TAB1$Temperature,TAB$MoyAcq,xlab = "Mean rainfall (mm)", ylab = "Mean daily acquisition rate",
     xlim = c(5, 8.5),ylim = c(80, 405),col="brown2",cex.axis=1.8,cex.lab=1.8,cex=2)

require(mgcv)
M4 <- gam(TAB$MoyAcq ~ s(TAB1$Temperature, fx = FALSE, k=4,bs = "cr"))#meilleur AIC avec k=4, à une unité près
AIC(M4)
coef(M4)
M4
par(mar=c(5,4,4,5)+.1)
par(new=TRUE)
par(bg=NA)
plot(M4, se = TRUE,col="brown2",lwd=3,xaxt="n",yaxt="n",xlab="",ylab="")
points(TAB1$Temperature,TAB$MoyAcq,xlab = "Mean rainfall (mm)", ylab = "Mean daily acquisition rate",
     xlim = c(5, 8.5),ylim = c(80, 405),col="brown2",cex.axis=1.8,cex.lab=1.8,cex=2)
M4pred <- predict(M4, se = TRUE, type = "response")

par(bg=NA)
plot(TAB1$Temperature, TAB$MoyAcq, type = "p",xlab = "Mean temperature (°c)", ylab = "Mean daily acquisition rate",
     xlim = c(5, 8.5),ylim = c(80, 430),col="brown2",cex.axis=1.8,cex.lab=1.8,cex=2)
I1 <- order(TAB1$Temperature)#to avoid a spaghetti plot
#par(bg=NA);lines(TAB1$Temperature[I1], M4pred$fit[I1], lty=1,col="brown2",lwd=3)
par(bg=NA);lines(TAB1$Temperature[I1], M4pred$fit[I1]+2*M4pred$se[I1],lty=2,col="brown2",lwd=3)
par(bg=NA);lines(TAB1$Temperature[I1], M4pred$fit[I1]-2*M4pred$se[I1],lty=2,col="brown2",lwd=3)
#max(M4pred$fit[I1]+2*M4pred$se[I1])
#min(M4pred$fit[I1]-2*M4pred$se[I1])


#test1<-lm(TAB$MoyAcq~TAB2$Precipitation);summary(test1);abline(test1, col="violet")#bof

#require(ggplot2)
## Default loess smooth, with SE bands added.
#quickplot(TAB2$Precipitation,TAB$MoyAcq,  geom=c("point","smooth"),span=0.5, se=T)
#qplot(TAB2$Precipitation,TAB$MoyAcq, geom = c("point","smooth"))#, span=0.2)
#qplot(TAB1$Temperature,TAB$MoyAcq,colour="red",geom = c("point","smooth"),xlab="Mean temperature",ylab = "Acquisition rate")#, span=0.2)

#Smoothing line#
#Smooth<-loess(TAB$MoyAcq~TAB2$Precipitation);Fit<-fitted(Smooth)#utile pour les deux commandes suivantes qui ne fonctionnent pas
#lines(TAB2$Precipitation,Fit)#ne fonctionne pas, pas de courbe produite
#lines(predict(Smooth),col="red")#ne fonctionne pas, courbe décalée

#SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAcq, spar = 0.3);lines(SmoothingSpline, col="darkorange")
#SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAcq, spar = 0.4);lines(SmoothingSpline, col="gold")
#SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAcq, spar = 0.5);lines(SmoothingSpline, col="blue")
#SmoothingSpline<-smooth.spline(TAB2$Precipitation,TAB$MoyAcq, spar = 0.6);lines(SmoothingSpline, col="coral1")


#####acquisition rate vs temperature######
#min(TAB1$Temperature);max(TAB1$Temperature);min(TAB$MoyAcq);max(TAB$MoyAcq)#pour limite de mes axes
#plot(TAB1$Temperature,TAB$MoyAcq,xlab = "Mean temperature (°c)", ylab = "Acquisition rate",
#     xlim = c(4.5, 8.5),
#     ylim = c(0, 0.005))
#test2<-lm(TAB$MoyAcq~TAB1$Temperature);summary(test2);abline(test2, col="violet")#bof

#smoothing line#
#Smooth1<-loess(TAB1$Temperature~TAB$MoyAcq)#commande pour obtenir une smooth line
#Fit1<-fitted(Smooth1)#récupération des valeurs prédites
#lines(TAB1$Temperature, Fit1)#plot des valeurs prédites=>ne fonctionne pas

#SmoothingSpline1<-smooth.spline(TAB1$Temperature,TAB$MoyAcq,spar=0.5);lines(SmoothingSpline1,col="firebrick1")
#SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAcq, spar = 0.3);lines(SmoothingSpline, col="darkorange")
#SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAcq, spar = 0.4);lines(SmoothingSpline, col="gold")
#SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAcq, spar = 0.5);lines(SmoothingSpline, col="firebrick1")
#SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAcq, spar = 0.6);lines(SmoothingSpline, col="red")
#SmoothingSpline<-smooth.spline(TAB1$Temperature,TAB$MoyAcq, spar = 0.7);lines(SmoothingSpline, col="cyan4")


###########################################################################################################

####Selection des mois de juin et juillet correspondant aux observations####
#temp_obs<-temp[(temp$M==6),]
#summary(temp_obs)
#unique(temp_obs$Y)
#temp_obs<-na.omit(temp_obs)
#plot(temp_obs$Date[temp_obs$Y==2005], temp_obs$MoyTemp[temp_obs$Y==2005],type="b")
#temp_obs$Date[temp_obs$Y==2005]
#temp_obs$MoyTemp[temp_obs$Y==2005]



####Boucle pour calcul acquisition rate par annee####
#j<-unique(acq$year)
#TAB<-NULL
#for (i in j){
 # h<-subset(acq,acq$year==i)
#  YEAR<-i
#  MoyAcq<-mean(h$acq_rate)
#  r<-data.frame(YEAR,MoyAcq)

 # TAB<-rbind(TAB,r)
#}
#TAB


####Boucle pour calcul temperature juin-juillet par an####
summary(temp_obs)
k<-unique(temp_obs$Y)
TAB1<-NULL
for (l in k){

  h<-subset(temp_obs,temp_obs$Y==l)
  YEAR<-l
  MoyTemp<-mean(h$MoyTemp,na.rm = T)
  r<-data.frame(YEAR,MoyTemp)

  TAB1<-rbind(TAB1,r)
}
TAB1


#Regroupement des donn?es de temperatures moyennes et de taux d'acq moyen dans un meme tableau
TempAcq<-cbind(TAB,TAB1)
TempAcq<-TempAcq[,-3]
summary(TempAcq)

#plot
par(mfrow=c(2,1))
plot(x = TempAcq$YEAR,y = TempAcq$MoyAcq, type="l")
plot(x = TempAcq$YEAR, y = TempAcq$MoyTemp, type = "l")
plot(TempAcq$MoyAcq,TempAcq$MoyTemp)

#test de correlation
require(stats)
correl<-cor.test(TempAcq$MoyTemp,TempAcq$MoyAcq,method = "pearson")
summary(correl)
correl
