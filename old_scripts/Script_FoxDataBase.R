getwd()
setwd("C:/Users/Toya/Desktop/Script_R")

#####chargement du tableau de donn�es principales � compl�ter#####
fox<-read.table("FoxDataBase.txt", h=T, sep="\t")
summary(fox)
is.na(fox$year)
fox<-fox[-298,]#retrait de la derni�re ligne inutile
summary(fox)

#####chargement du tableau de valeurs de jour moyen de ponte et d'�closion#####
JrMoy<-read.table("jour_moy_ponte_eclos.txt", h=T, sep="\t")
summary(JrMoy)

#####Association du jour moyen de ponte et d'�closion en fonction des ann�es#####
#mini boucle ouh yeah ! pour faire tout en un clic
for(i in JrMoy$year){
    fox$lay_moy[fox$year==i]<-JrMoy$moy_ponte[JrMoy$year==i]
    fox$hatch_moy[fox$year==i]<-JrMoy$moy_eclos[JrMoy$year==i]  }

summary(fox)#v�rification

#####Calcul des �carts entre jour d'observation et jour moyen de ponte et d'�closion pour chaque observation#####

#fox$lay=jour d'obs - date moyenne de ponte
fox$lay<-fox$date-fox$lay_moy
#fox$hatch=jour d'obs-date moyenne d'�closion
fox$hatch<-fox$date-fox$hatch_moy

summary(fox)#v�rification ==> valeur identique � Vincent pour moyenne des �carts � la ponte et diff�rence de 0.5 pour moyenne des �carts au jour moyen de d'�closion

#####Etablissemnet de la variable fox$intra ==> a si fox$date +/- 5 de fox$lay_moy, b si fox$date � +6/+15 de fox$lay_moy, c si fox$date � -11/-6 de fox$hatch_moy et d si fox$date � -5/+5 de fox$hatch_moy

fox$intra[fox$date>= fox$lay_moy-5 & fox$date<=fox$lay_moy+5]<-"a"
fox$intra[fox$date>= fox$lay_moy+6 & fox$date<=fox$lay_moy+15]<-"b"
fox$intra[fox$date>=fox$hatch_moy-11 & fox$date<=fox$hatch_moy-6]<-"c"
fox$intra[fox$date>=fox$hatch_moy-5 & fox$date<=fox$hatch_moy+5]<-"d"

#apr�s observations des lignes contenant NA pour intra, NA correspond � des jours d'observations qui sortent de l'intervalle de d (soit +/- 5 jours /t date hatch_moy)
is.na(fox$intra)
fox$intra[is.na(fox$intra)]<-"d"

#####Rajout des valeurs d'indice d'abondance de lemming et de densit� de nids d'oies#####

lem<-read.table("density_abundance.txt", h=T, sep="\t", dec=",")
#mini boucle ouh yeah ! pour faire tout en un clic
for(i in lem$year){
  fox$index[fox$year==i]<-lem$index_C1[lem$year==i]
  fox$nest_density[fox$year==i]<-lem$nest_density[lem$year==i]  }

summary(fox)

#####Calcul du lag (nombre d'ann�es apr�s un pic de lem), les ann�es de pics sont 1996, 2000 et 2004#####

fox$lag[fox$year<2000]<-fox$year[fox$year<2000]-1996
fox$lag[fox$year>=2000 & fox$year<2004]<-fox$year[fox$year>=2000 & fox$year<2004]-2000
fox$lag[fox$year>=2004]<-fox$year[fox$year>=2004]-2004

#####Calcul du acquisition rate#####
acq<-fox[-2]#retrait de la colone "inter" qui ne m'int�resse pas

#essai<-subset(acq,!(acq$egg_total==0 | is.na(acq$egg_total)) )
#summary(essai)###bon nombre de lignes attendues mais pas la m�me moyenne

acq<-acq[!(acq$egg_total==0),]#retrait des valeurs nulles de nombre d'oeufs pour le calcul de acquisition rate
acq<-acq[!is.na(acq$egg_total),]#retrait des NA
summary(acq)
acq$acq_rate<-acq$egg_total/(acq$obs_lenght/3600)


for(i in acq$obs_lenght) & (j in acq$date){
  fox$acq_rate[fox$obs_lenght==i & fox$date==j]<-acq$acq_rate[acq$obs_lenght==i & acq$date==j]
   }###marche pas voir les boucle � deux valeurs


######################################################################################################
#zone de brouillon




is.na(fox$lag)
mode(fox$lag)
fox$lag[fox$year<2000]
fox$lag[fox$year<2000]-1996
fox$lag[fox$year<2000]-1996
library(MASS)
fox$intra
table(fox$intra)#donne l'occurence de chaque level
barplot(table(fox$intra), col=c("red","pink", "violet", "purple"))#histogramme de la fr�quence des levels d'intra
pie(table(fox$intra), col=c("red","pink", "violet", "purple"))#diagramme en cercle
summary(fox$intra)
pairs(fox)

write.table(fox,"DataFoxR.txt", sep="\t",dec=",", col.names=NA)

Nba<-fox[(fox$intra=="a"),];
Nba<-Nba[!is.na(Nba$intra),]
Nbb<-fox[(fox$intra=="b"),];Nbb<-Nbb[!is.na(Nbb$intra),]
Nbc<-fox[(fox$intra=="c"),];Nbc<-Nbc[!is.na(Nbc$intra),]
Nbd<-fox[(fox$intra=="d"),];Nbd<-Nbd[!is.na(Nbd$intra),]

summary(fox)
fox1<-fox[!is.na(fox$intra),c(1,8,10,11)]
summary(fox1)
plot(fox1)

plot(obs_lenght~year,data=fox)#fonctionne
hist(fox$year)
mode(fox$intra)
plot(fox$year~fox$intra,na.rm=T)#fonctionne pas
barplot(table(fox[,"intra"]))#fonctionne


as.character(fox$intra)
vecteur<-c(fox$item,fox$fate)
for(i in vecteur){
  as.character(i)
}
