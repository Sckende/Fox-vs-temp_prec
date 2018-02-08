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

