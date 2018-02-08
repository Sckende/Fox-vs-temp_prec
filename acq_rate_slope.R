
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
acqui<-read.table("acq_rate_1996-2016.txt", h=T, sep="\t", dec=",")
attaq<-read.table("attaq_rate_1996-2016.txt",h=T,sep="\t",dec = ",")

head(attaq)

####rajout du time lag (=lag) dans les données attaque
attaq$lag[attaq$year==1996 | attaq$year==2004]<-0
attaq$lag[attaq$year==1997 | attaq$year==2005 | attaq$year==2015]<-1
attaq$lag[attaq$year==1998 | attaq$year==2016]<-2
attaq$lag[attaq$year==1999]<-3

summary(attaq)
subset(attaq,attaq$species=="LMG")
attaq<-attaq[-c(222,233),]


#####Calcul acquisition & attack rates (nb attaq/oeufs par jour)
attaq$atq_rate<-(attaq$attack/attaq$obs_length)*(3600*24)
summary(attaq)

acqui$acq_rate<-(acqui$egg/acqui$obs_lenght)*(3600*24)
summary(acqui)

#####Plot des taux en fonction des lags de lemmings####
require(stats)
require(plotrix)

###Attack rate per year###
atqLEMy<-tapply(attaq$atq_rate,attaq$year,mean);atqLEMy
std.errY<-tapply(attaq$atq_rate,attaq$year,std.error);std.errY

TABy<-cbind(atqLEMy,std.errY)
TABy <- as.data.frame(TABy)

d<-NULL

j<-unique(attaq$year)
for (i in j){
lagY <- attaq$lag[attaq$year == i]
c<-data.frame(i,lagY)

d<-rbind(d, c)
}

G<-aggregate(d,list(d$i,d$lagY), mean); G<-G[-9,]; G
G<-G[order(G$i),];G

TABy$lag<-G$lagY
TABy$year<-G$i

dim(TABy)
names(TABy)

###Attack rate per time lag###
atqLEM<-tapply(attaq$atq_rate,attaq$lag,mean);atqLEM
std.err<-tapply(attaq$atq_rate,attaq$lag,std.error);std.err

TAB1<-cbind(atqLEM,std.err)
TAB1 <- as.data.frame(TAB1)
lag <- c(0:3)
TAB1$lag <- cbind(lag)
TAB1
dim(TAB1)
names(TAB1)

#par(bg=NA)
plot(TAB1$atqLEM, xlab = "year after lemming peak", ylab = "Mean daily attack rate (attack/hour)", xlim = c(1,4), ylim = c(150,700),
     cex.axis=1.8,cex.lab=1.8,cex=2,pch=16,col="forestgreen",type="b",lwd=3)
#une seule année en lag 2 et 3, deux années en lag 0 et 3 années en lag 1
require(Hmisc)
with (data = TAB1, expr = errbar(lag,atqLEM,atqLEM+std.err,atqLEM-std.err, add=T,lwd=3, cap=.01,col="forestgreen"),xlim = c(1,4), ylim = c(150,700))#ajout des barres de standard error
#par(new=T)

###Acquisition rate###
acqLEM<-tapply(acqui$acq_rate,acqui$lag,mean);acqLEM
std.err1<-tapply(acqui$acq_rate,acqui$lag,std.error);std.err1

TAB2<-cbind(acqLEM,std.err1)
TAB2 <- as.data.frame(TAB2)
lag <- c(0:3)
TAB2$lag<- cbind(lag)
TAB2
dim(TAB2)
names(TAB2)

plot(acqLEM, xlab = "year after lemming peak", ylab = "acquisition rate (egg/day)", xlim = c(1,4), ylim = c(120,320))
#require(Hmisc)
with (data = TAB2, expr = errbar(lag,acqLEM,acqLEM+std.err,acqLEM-std.err, add=T,lwd=3, cap=.01,col="forestgreen"))#ajout des barres de standard error
#par(new=T)

#########################zone de brouillon################
hist(acqui$obs_lenght)
hist(acqui$egg)
plot(x = acqui$obs_lenght,y=acqui$egg)

reg<-lm(acqui$egg~acqui$obs_lenght)
summary(reg)

plot(x=temp$obs_lenght,y=temp$acq_rate)#moche....

require(stats)
acqLEM<-tapply(temp$acq_rate,temp$lag,mean)
plot(acqLEM, xlab = "year after lemming peak", ylab = "acquisition rate (egg/hour)", xlim = c(0,4), ylim = c(0,15))



reg1<-lm(temp$acq_rate~temp$obs_lenght)
abline(reg1)

abline(reg)
summary(reg)
reg$coefficients#Vincent'slope=-0,44 et P<0.01, ici slope=-0,002566224
plot(reg$residuals)
plot(reg$fitted.values)

tempBIS<-temp[temp$obs_lenght>=600,]#Vincent'n=96, ici n=97


tempBIS<-temp[temp$obs_lenght<=3000,]#Vincent'n=96, ici n=97
plot(tempBIS$obs_lenght,tempBIS$acq_rate)
regBIS<-lm(tempBIS$acq_rate~scale(tempBIS$obs_lenght))
abline(regBIS)
summary(regBIS)
regBIS$coefficients#Vincent'slope=-0,44 et P<0.01, ici slope=-0,002566224
plot(regBIS$residuals)
plot(regBIS$fitted.values)

temp0<-subset(temp,temp$lag==0)
summary(temp0)
plot(temp0$obs_lenght,temp0$acq_rate)
reg0<-lm(temp0$acq_rate~temp0$obs_lenght)
abline(reg0)
summary(reg0)

################################BROUILLON###############################
