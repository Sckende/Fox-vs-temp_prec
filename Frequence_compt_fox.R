setwd("/Users/nicolas/Desktop/Claire/R_analysis/Data")

#####Chargement du fichier comportements renard 2015#####
comp<-read.table("Fox_compt_2015.txt", h=T, sep="\t",dec=",")
summary(comp)
levels(comp$CPT)

#####Regroupement des comportements#####
#comp[(comp$CPT=="INTER_FOX" | comp$CPT=="INTER_FOX_NEG"),]
comp$CPTbis[comp$CPT=="INTER_FOX"|comp$CPT=="INTER_FOX_NEG"|comp$CPT=="CHASS_LAB"|comp$CPT=="CHASS_OIE"|comp$CPT=="INTER_OIE"|comp$CPT=="INTER_LP"|comp$CPT=="INTER_LLQ"|comp$CPT=="INTER_LAB"]<-"INTERACTION"
comp$CPTbis[comp$CPT=="ATTAQ"]<-"ATTAQUE"
comp$CPTbis[comp$CPT=="CACHE"]<-"CACHE"
comp$CPTbis[comp$CPT=="CH"]<-"CHERCHE"
comp$CPTbis[comp$CPT=="DEP"]<-"DEPLACEMENT"
comp$CPTbis[comp$CPT=="SHIT"]<-"MARQUAGE"
comp$CPTbis[comp$CPT=="COUCHE"|comp$CPT=="CREUSE"|comp$CPT=="GROOM"|comp$CPT=="MANGE"|comp$CPT=="PAUSE"|comp$CPT=="RAMASSE"]<-"AUTRES"

#####Ranger dans l'ordre alphabétique les types de comportements pour homogénéiser tous les piecharts#####
comp<-comp[order(comp$CPTbis),]

#####Calcul des fréquences de comportements pour le piechart#####
j<-unique(comp$CPTbis)
TAB<-NULL
for (i in j){
  TOT<-sum(comp$CPTbis==i)
  FREQ<-TOT/nrow(comp)
  r<-data.frame(i,TOT,FREQ)
  
  TAB<-rbind(TAB,r)
}
TAB


require(plotrix)
pie3D(TAB$TOT,labels = TAB$i, explode = 0.15,labelcex=0.7, main="Pie chart of fox behaviour in 2015", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))

#####Chargement du fichier comportements renard 1996-1999#####

#####Chargement du fichier comportements renard 2004-2005#####
comp1<-read.table("Fox_compt_2004-2005.txt", h=T, sep="\t",dec=",")
summary(comp1)

#comp1$CPT[comp1$CPT=="ATTAQ"]="ATTAQUE"#commande qui génère des NAs, ne fonctionne pas sur variable qualitative
levels(comp1$CPT)[1]="ATTAQUE"#pour modification des valeurs qualitatives
#comp1[match("ATTAQ",levels(comp1$CPT))]<-"ATTAQUE”#commande Nico


comp1a<-subset(comp1,YEAR==2004);comp1a<-comp1a[order(comp1a$CPT),]
comp1b<-subset(comp1,YEAR==2005);comp1b<-comp1b[order(comp1b$CPT),]



j<-unique(comp1a$CPT)
TAB1a<-NULL
for (i in j){
  TOT1a<-sum(comp1a$CPT==i)
  FREQ1a<-TOT1a/nrow(comp1a)
  r<-data.frame(i,TOT1a,FREQ1a)
  
  TAB1a<-rbind(TAB1a,r)
}
TAB1a
#require(plotrix)
pie3D(TAB1a$TOT1a,labels = TAB1a$i, explode = 0.15,labelcex=0.7, main="Pie chart of fox behaviour in 2004", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))


j<-unique(comp1b$CPT)
TAB1b<-NULL
for (i in j){
  TOT1b<-sum(comp1b$CPT==i)
  FREQ1b<-TOT1b/nrow(comp1b)
  r<-data.frame(i,TOT1b,FREQ1b)
  
  TAB1b<-rbind(TAB1b,r)
}
TAB1b
#require(plotrix)
pie3D(TAB1b$TOT1b,labels = TAB1b$i, explode = 0.15,labelcex=0.7, main="Pie chart of fox behaviour in 2005", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))

# One figure in row 1 and two figures in row 2
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
pie3D(TAB1a$TOT1a,labels = TAB1a$i, explode = 0.15,labelcex=0.7, main="Fréquence des comportements de renard 2004", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))
pie3D(TAB1b$TOT1b,labels = TAB1b$i, explode = 0.15,labelcex=0.7, main="Fréquence des comportements de renard 2005", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))
pie3D(TAB$TOT,labels = TAB$i, explode = 0.15,labelcex=0.7, main="Fréquence des comportements de renard 2015", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"))

#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
#radial.pie(TAB1a$TOT1a,labels = TAB1a$i,grid.unit = F, labelcex=0.7, main ="Fréquence des comportements de renard 2004", sector.colors = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"), radlab=T)#joli mais à revoir pour l'interprétation
#radial.pie(TAB1a$TOT1a,labels = TAB1a$i,labelcex=0.7, main="Fréquence des comportements de renard 2004", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"), radlab=T)
#radial.pie(TAB$TOT,labels = TAB$i,labelcex=0.7, main="Fréquence des comportements de renard 2015", col = c("seagreen1","salmon","red2","olivedrab2","orange","navy","bisque"), radlab=T)


################ Compilation pour calcul de taux d'attaque ####################
# ICI UN TAUX ATTAQUE PAR ANNEE 
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
acqui<-read.table("acq_rate_1996-2016.txt", h=T, sep="\t", dec=",")
attaq<-read.table("attaq_rate_1996-2016.txt",h=T,sep="\t",dec = ",")

head(attaq)

####rajout du time lag (=lag) dans les données attaque
attaq$lag[attaq$year==1996 | attaq$year==2004]<-0
attaq$lag[attaq$year==1997 | attaq$year==2005 | attaq$year==2015]<-1
attaq$lag[attaq$year==1998 | attaq$year==2016]<-2
attaq$lag[attaq$year==1999]<-3

#####Calcul acquisition & attack rates (nb attaq/oeufs par jour)
attaq$atq_rate<-(attaq$attack/attaq$obs_length)*(3600*24)
summary(attaq)

acqui$acq_rate<-(acqui$egg/acqui$obs_lenght)*(3600*24)
summary(acqui)

#####Attack rate per year######
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

########################################################
##### Ajout des variables biotiques et abiotiques #####
######################################################
temp<-read.table("Tair moy 1989-2016 BYLCAMP.txt", h=T, sep="\t", dec=",")
prec<-read.table("precipitation_Bylot_1996-2016.txt",h=T, dec = ",", sep = "\t")
fox<-read.csv("Fox_abundance_Chevallier.txt", sep = "\t", dec = ",")
lmg<-read.csv("LEM96-2016.txt", sep = "\t", dec = ",")
prod<-read.csv("Prod_prim_camp_2.txt", sep = "\t", dec = ",")
AO<-read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
breed<-read.csv("GOOSE_breeding_informations.txt", sep = "\t", dec = ".")


##### Températures #####
summary(temp)
require(date)

temp$YMD<-paste(temp$YEAR,temp$MONTH,temp$DAY,sep = "-")
temp$YMD<-strptime(temp$YMD, format = "%Y-%m-%d")
temp$JJ<-format(temp$YMD,format = "%j")
temp$JJ<-as.numeric(temp$JJ)

j<-TABy$year
TAB_temp<-NULL

for(i in j){
  a<-min(attaq$date[attaq$year == i])
  b<-max(attaq$date[attaq$year == i])
  
  moy_Tc<-mean(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])
  sd_Tc<-sd(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])
  min_Tc<-min(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])
  max_Tc<-max(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])
  
  #min temp coolest day during spring    
  SPRmin<-min(temp$TEMP[temp$JJ>=140 & temp$JJ<=171 & temp$YEAR==i])
  #max temp coolest day during spring    
  SPRmax<-max(temp$TEMP[temp$JJ>=140 & temp$JJ<=171 & temp$YEAR==i])
  #min temp coolest day during early summer  
  eSUMmin<-min(temp$TEMP[temp$JJ>=172 & temp$JJ<=196 & temp$YEAR==i])
  #max temp warmest day during early summer
  eSUMmax<-max(temp$TEMP[temp$JJ>=172 & temp$JJ<=196 & temp$YEAR==i])
  
  
  r<-data.frame(a,b,moy_Tc,sd_Tc,min_Tc,max_Tc,SPRmin,SPRmax,eSUMmin,eSUMmax)
  
  TAB_temp<-rbind(TAB_temp,r)
}

TABy<-cbind(TABy,TAB_temp)


##### Précipitations #####
summary(prec)

j<-TABy$year
TAB_prec<-NULL

for(i in j){
  a<-min(attaq$date[attaq$year == i])
  b<-max(attaq$date[attaq$year == i])
  
  moy_pr<-mean(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  sd_pr<-sd(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  min_pr<-min(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  max_pr<-max(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  
  #min prec during spring    
  SPRmin_prec<-min(prec$rain[prec$day>=140 & prec$day<=171 & prec$year==i], na.rm = T)
  #max prec during spring    
  SPRmax_prec<-max(prec$rain[prec$day>=140 & prec$day<=171 & prec$year==i], na.rm = T)
  #min prec during early summer  
  eSUMmin_prec<-min(prec$rain[prec$day>=172 & prec$day<=196 & prec$year==i], na.rm = T)
  #max prec during early summer
  eSUMmax_prec<-max(prec$rain[prec$day>=172 & prec$day<=196 & prec$year==i], na.rm = T)
  
  
  s<-data.frame(moy_pr,sd_pr,min_pr,max_pr,SPRmin_prec,SPRmax_prec,eSUMmin_prec,eSUMmax_prec)
  
  TAB_prec<-rbind(TAB_prec,s)
}

TABy<-cbind(TABy,TAB_prec)

##### Autres variables #####
c<-TABy$year
suite<-NULL

for(d in c){
  
  fox_dens<-fox$natal_growth_dens[fox$year == d]
  #fox_dens_timelag<-fox$natal_growth_dens[fox$year == d+ ou - 1]
  lmg_abun<-lmg$LMG_C2[lmg$YEAR == d]
  prim_prod<-prod$PROD_INDICE_C2[prod$YEAR == d]
  
  winAO<-AO$winAO[AO$YEAR==d]
  sprAO<-AO$sprAO[AO$YEAR==d]
  esumAO<-AO$esumAO[AO$YEAR==d]
  lsumAO<-AO$lsumAO[AO$YEAR==d]
  
  nest_density<-breed$NEST_DENSITY[breed$YEAR==d]
  clutch_size<-breed$CLUTCH_SIZE[breed$YEAR==d]
  egg_abun<-breed$EGG_ABUN[breed$YEAR==d]
  ratio_JUVad<-breed$RATIO_YOU_AD[breed$YEAR==d]
  brood_size<-breed$BROOD_SIZE_BAND[breed$YEAR==d]
  nest_succ<-breed$NEST_SUCC[breed$YEAR==d]
  
  s<-data.frame(fox_dens, lmg_abun, prim_prod, winAO, sprAO, esumAO, lsumAO,nest_density,clutch_size,egg_abun,ratio_JUVad,brood_size,nest_succ)
  suite<-rbind(suite,s)
}

TABy<-cbind(TABy,suite)
summary(TABy)

write.csv(TABy,"Path analysis_data 4.txt")


