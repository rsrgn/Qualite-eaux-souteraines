#----Cleaning memory----
rm(list = ls())

#----Loading libraries----

library(tidyverse)
library(reshape)

#Cartographie
library(sf)
library(SpatialPosition)
library(cartography)


#----Setting Working Directory----

#PORTABLE RAPH
#setwd("C:/Users/serou/Documents/cours/MEDAS/DATAVIZ/projet")
#PC BUREAU
setwd("X:/Raphael/ADHERENT/07 - FICHIERS/ANALYSES/PROJET2/projet")

#----Loading data----


pesticides  <- read.csv2(file="./data/pesticides.csv")
stations  <- read.csv2(file="./data/stations.csv")

ma_qp_fm_ttres_pesteso_2007 <- read.csv2(file="./data/ma_qp_fm_ttres_pesteso_2007.csv")
ma_qp_fm_ttres_pesteso_2008 <- read.csv2(file="./data/ma_qp_fm_ttres_pesteso_2008.csv")
ma_qp_fm_ttres_pesteso_2009 <- read.csv2(file="./data/ma_qp_fm_ttres_pesteso_2009.csv")
ma_qp_fm_ttres_pesteso_2010 <- read.csv2(file="./data/ma_qp_fm_ttres_pesteso_2010.csv")
ma_qp_fm_ttres_pesteso_2011 <- read.csv2(file="./data/ma_qp_fm_ttres_pesteso_2011.csv")
ma_qp_fm_ttres_pesteso_2012 <- read.csv2(file="./data/ma_qp_fm_ttres_pesteso_2012.csv")

moy_tot_quantif_2007 <- read.csv2(file="./data/moy_tot_quantif_2007.csv")
moy_tot_quantif_2008 <- read.csv2(file="./data/moy_tot_quantif_2008.csv")
moy_tot_quantif_2009 <- read.csv2(file="./data/moy_tot_quantif_2009.csv")
moy_tot_quantif_2010 <- read.csv2(file="./data/moy_tot_quantif_2010.csv")
moy_tot_quantif_2011 <- read.csv2(file="./data/moy_tot_quantif_2011.csv")
moy_tot_quantif_2012 <- read.csv2(file="./data/moy_tot_quantif_2012.csv")

# autres dataset

regions <- read.csv2(file="./data/Reg2016.csv")
departements <- read.csv2(file="./data/depts2016.csv")


#cartes avec sf

pathAE <- "./data/ADMIN-EXPRESS-COG_1-0__SHP__FRA_2017-06-19/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2017-06-19/ADE-COG_1-0_SHP_LAMB93_FR"

dept2 <- st_read(dsn = pathAE, layer="DEPARTEMENT", stringsAsFactors=FALSE)




#----Pr�paration des donn�es----

#ajout de l'ann�e aux fichiers des mesures
ma_qp_fm_ttres_pesteso_2007$ann�e <- "2007"
ma_qp_fm_ttres_pesteso_2008$ann�e <- "2008"
ma_qp_fm_ttres_pesteso_2009$ann�e <- "2009"
ma_qp_fm_ttres_pesteso_2010$ann�e <- "2010"
ma_qp_fm_ttres_pesteso_2011$ann�e <- "2011"
ma_qp_fm_ttres_pesteso_2012$ann�e <- "2012"

#fusion des mesure dans un df unique
mesures <- rbind(ma_qp_fm_ttres_pesteso_2007,
                 ma_qp_fm_ttres_pesteso_2008,
                 ma_qp_fm_ttres_pesteso_2009,
                 ma_qp_fm_ttres_pesteso_2010,
                 ma_qp_fm_ttres_pesteso_2011,
                 ma_qp_fm_ttres_pesteso_2012)

#calcul de l'�cart par rapport � la norme pour toutes les mesures
mesures$Ecart_norme <- mesures$MA_MOY - mesures$NORME_DCE

#fusion de toutes les moyennes dans un df unique
moyennes <- rbind(na.omit(moy_tot_quantif_2007),
                  na.omit(moy_tot_quantif_2008),
                  na.omit(moy_tot_quantif_2009),
                  na.omit(moy_tot_quantif_2010),
                  na.omit(moy_tot_quantif_2011),
                  na.omit(moy_tot_quantif_2012))

#Ajout des noms de regions aux fichiers des moyennes
moyennes <- left_join(moyennes, stations, by= "CD_STATION")
departements <- left_join(departements,regions,by="REGION")
moyennes <- left_join(moyennes, departements, by=c("NUM_DEP"="DEP"))
moyennes <- moyennes %>% dplyr::rename(NOM_REG = NCC.y)

#exploration des donn�es de moyennes par stations
summary(moyennes$MOYPTOT)
quantile(moyennes$MOYPTOT,0.9992)
moyennes %>% filter(MOYPTOT > 50) %>% select(ANNEE, MOYPTOT, NOM_REG)

#graphique barplot des moyennes de mesures par stations
p1 <- ggplot(data=moyennes, aes(x=NOM_REG, y=MOYPTOT, group=NOM_REG) )  
p1 <- p1 +  geom_boxplot() #+ facet_wrap(~ NOM_REG)
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1

#suppression des 4 valeurs sup�rieurs � 50
moyennes2 <- moyennes %>% filter(MOYPTOT < 50) 
grouped_moyennes <- group_by(moyennes2,ANNEE, NOM_REG)
per_region <- summarise(grouped_moyennes, n_region=n())
par_region <- na.omit(summarise(grouped_moyennes, moy_reg=mean(MOYPTOT)))

#recherches d'informations sur les mesures ab�rantes du d�partement 45
moyennes %>% filter(MOYPTOT > 50) 
moyennes %>% filter(CD_STATION == "03288X0042/P") 
mesures %>% filter(CD_STATION == "03288X0042/P") 
mesures %>% filter(CODE_FAMILLE == "Autres �l�ments min�raux") 

#V�rif r�gion Corse
moyennes %>% filter(REGION == 94) 

#----graphique evolution mesure pesticides par regions----
dev.off() 
png(paste("./graph/graph1.png",sep=""),  width= 2000 , height= 1200 ,res=250)

p1 <- ggplot(data=par_region, aes(y=moy_reg, x=ANNEE, group=NOM_REG, colour=NOM_REG ) )  
p1 <- p1 +  geom_line(size=1.2)+ facet_wrap(~ NOM_REG)
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1 <- p1 + ggtitle("Evolution des mesures de pesticides par r�gions et par ann�es\ndans les eaux souteraines en France")
p1 <- p1 + xlab("Ann�es") + ylab("Quantit� de pesticides d�tect�e (en �g/l)")
p1 <- p1 + labs(caption = "Source: Minist�re de la Transition �cologique et solidaire")
p1
dev.off()

#----Contour France----

france <- st_union(dept2)
france <- as(france,"Spatial")

#Param�tres graphiques

larg <- 7500
haut <- 4000
resolution <- 900


#----praparation des donn�es carte 2012----

stations$CD_STATION <- as.character(stations$CD_STATION) 
ma_qp_fm_ttres_pesteso_2012$CD_STATION <- as.character(ma_qp_fm_ttres_pesteso_2012$CD_STATION)

#ajout des donn�es des stations dans les mesures
ma_qp_fm_ttres_pesteso_2012 <- left_join(x = ma_qp_fm_ttres_pesteso_2012,y =  stations, by= "CD_STATION")
ma_qp_fm_ttres_pesteso_2012 <- left_join(x=ma_qp_fm_ttres_pesteso_2012,y=pesticides,by="LB_PARAMETRE")

grouped_mesures_2012 <- group_by(ma_qp_fm_ttres_pesteso_2012, NUM_DEP, CODE_FAMILLE)
mesures_par_dept_2012 <- na.omit(summarise(grouped_mesures_2012, moy_dept=mean(MA_MOY)))

#supressions des caract�res speciaux dans les titres de colonnes
mesures_par_dept_2012$CODE_FAMILLE <- gsub("�","e",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("�","e",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("�","e",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("�","a",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("�","i",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub(" ","_",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("\\(","",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub(")","",mesures_par_dept_2012$CODE_FAMILLE)

mesures_par_dept_2012_2 <- cast(data = mesures_par_dept_2012, NUM_DEP ~ CODE_FAMILLE )

#ajout des mesures par pesticide dans la carte des departements.
dept_pest_2012 <- left_join(x=dept2 , y=mesures_par_dept_2012_2, by=c("INSEE_DEP"="NUM_DEP") )

#vecteur de titres des cartes

familles_2012 <- data.frame(noms=unique(ma_qp_fm_ttres_pesteso_2012$CODE_FAMILLE))
familles_2012$noms2 <- familles_2012$noms

familles_2012$noms2 <- gsub("�","e",familles_2012$noms2)
familles_2012$noms2 <- gsub("�","e",familles_2012$noms2)
familles_2012$noms2 <- gsub("�","e",familles_2012$noms2)
familles_2012$noms2 <- gsub("�","a",familles_2012$noms2)
familles_2012$noms2 <- gsub("�","i",familles_2012$noms2)
familles_2012$noms2 <- gsub(" ","_",familles_2012$noms2)
familles_2012$noms2 <- gsub("\\(","",familles_2012$noms2)
familles_2012$noms2 <- gsub(")","",familles_2012$noms2)

familles_2012 <- familles_2012[order(familles_2012$noms2),]
familles_2012 

#----map 2012 - liss�es----


png(paste("./map/carte_lissee_Moy_FamillesPesticide_2012.png",sep=""),  width= larg , height= haut ,res=resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
    smoothLayer(x = dept_pest_2012, var = familles_2012$noms2[i],
              span = 75000, beta = 2, #breaks = bks,
              mask = france, border = NA,
              col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
              legend.pos = "topleft", legend.values.rnd = 3,
              legend.title.txt = "Qt�. en �g/l"
              )
  layoutLayer(title = familles_2012$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()


#----map 2012 - par d�partements ----


png(paste("./map/carte_dept_Moy_FamillesPesticide_2012.png",sep=""),  width= larg , height= haut ,res=resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
  bks <- getBreaks(v=mesures_par_dept_2012_2[,familles_2012$noms2[i]], method = "quantile", nclass = 8)
  choroLayer(x = dept_pest_2012, var = familles_2012$noms2[i],
              breaks = bks,
              col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
              legend.pos = "bottomleft", legend.values.rnd = 4,
              legend.title.txt = "Qt�. en �g/l",
              border = NA
  )
  
  plot(france, border="grey15", add = TRUE , lwd = 0.5)

  layoutLayer(title = familles_2012$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()




#----praparation des donn�es cartes 2007----


#ajout des donn�es des stations dans les mesures
ma_qp_fm_ttres_pesteso_2007 <- left_join(x = ma_qp_fm_ttres_pesteso_2007,y =  stations, by = "CD_STATION")
ma_qp_fm_ttres_pesteso_2007 <- left_join(x = ma_qp_fm_ttres_pesteso_2007,y = pesticides,by = "LB_PARAMETRE")

grouped_mesures_2007 <- group_by(ma_qp_fm_ttres_pesteso_2007, NUM_DEP, CODE_FAMILLE)
mesures_par_dept_2007 <- na.omit(summarise(grouped_mesures_2007, moy_dept=mean(MA_MOY)))

#supressions des caract�res speciaux dans les titres de colonnes
mesures_par_dept_2007$CODE_FAMILLE <- gsub("�","e",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("�","e",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("�","e",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("�","a",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("�","i",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub(" ","_",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("\\(","",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub(")","",mesures_par_dept_2007$CODE_FAMILLE)

mesures_par_dept_2007_2 <- cast(data = mesures_par_dept_2007, NUM_DEP ~ CODE_FAMILLE )

#ajout des mesures par pesticide dans la carte des departements.
dept_pest_2007 <- left_join(x=dept2 , y=mesures_par_dept_2007_2, by=c("INSEE_DEP"="NUM_DEP") )

#vecteur de titres des cartes

familles_2007 <- data.frame(noms=unique(ma_qp_fm_ttres_pesteso_2007$CODE_FAMILLE))
familles_2007$noms2 <- familles_2007$noms

familles_2007$noms2 <- gsub("�","e",familles_2007$noms2)
familles_2007$noms2 <- gsub("�","e",familles_2007$noms2)
familles_2007$noms2 <- gsub("�","e",familles_2007$noms2)
familles_2007$noms2 <- gsub("�","a",familles_2007$noms2)
familles_2007$noms2 <- gsub("�","i",familles_2007$noms2)
familles_2007$noms2 <- gsub(" ","_",familles_2007$noms2)
familles_2007$noms2 <- gsub("\\(","",familles_2007$noms2)
familles_2007$noms2 <- gsub(")","",familles_2007$noms2)

familles_2007 <- familles_2007[order(familles_2007$noms2),]
familles_2007

#----map 2017 liss�e ----


png(paste("./map/carte_lissee_Moy_FamillesPesticide_2007.png",sep=""),  width= larg , height= haut ,res=resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
  smoothLayer(x = dept_pest_2007, var = familles_2007$noms2[i],
              span = 75000, beta = 2, #breaks = bks,
              mask = france, border = NA,
              col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
              legend.pos = "topleft", legend.values.rnd = 3,
              legend.title.txt = "Qt�. en �g/l"
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = familles_2007$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()


#----map 2007 - par d�partements ----

png(paste("./map/carte_dept_Moy_FamillesPesticide_2007.png",sep=""),  width= larg , height= haut ,res= resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
  bks <- getBreaks(v=mesures_par_dept_2007_2[,familles_2007$noms2[i]], method = "quantile", nclass = 8)
  choroLayer(x = dept_pest_2007, var = familles_2007$noms2[i],
             breaks = bks,
             col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
             legend.pos = "bottomleft", legend.values.rnd = 3,
             legend.title.txt = "Qt�. en �g/l" , border = NA
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = familles_2007$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()



#----map 2007 VS 2012 - par d�partements ----
``
larg <- 3750 # 7500
haut <- 8000 # 4000
resolution <- 900

?choroLayer

png(paste("./map/carte_dept_Moy_FamillesPesticide_2007_2012_P1.png",sep=""),  width= larg , height= haut ,res= resolution)
opar <- par(mfrow = c(8,2), mar = c(0,0,1.2,0))

i<-1
for(i in 1:8){
  
  #echelle commune pour comparer 2007 � 2012
  c_2007_2012 <- c(mesures_par_dept_2007_2[,familles_2007$noms2[i]],mesures_par_dept_2012_2[,familles_2012$noms2[i]])
  bks <- getBreaks(v=c_2007_2012, method = "quantile", nclass = 8)
  
  #map 2007
  choroLayer(x = dept_pest_2007, var = familles_2007$noms2[i],
             breaks = bks,
             col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
             legend.pos = "bottomleft", legend.values.rnd = 3,
             legend.title.txt = "Qt�. en �g/l" , border = NA
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = paste(familles_2007$noms[i]," 2007"), 
              author = "", sources = "", scale=NULL , col="grey40")
  #map 2012
  choroLayer(x = dept_pest_2012, var = familles_2012$noms2[i],
             breaks = bks,
             col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
             legend.pos = "n", legend.values.rnd = 3,
             legend.title.txt = "Qt�. en �g/l" , border = NA
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = paste(familles_2012$noms[i]," 2012"), 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()

png(paste("./map/carte_dept_Moy_FamillesPesticide_2007_2012_P2.png",sep=""),  width= larg , height= haut ,res= resolution)
opar <- par(mfrow = c(8,2), mar = c(0,0,1.2,0))

i<-1
for(i in 9:16){
  
  #echelle commune pour comparer 2007 � 2012
  c_2007_2012 <- c(mesures_par_dept_2007_2[,familles_2007$noms2[i]],mesures_par_dept_2012_2[,familles_2012$noms2[i]])
  bks <- getBreaks(v=c_2007_2012, method = "quantile", nclass = 8)
  
  #map 2007
  choroLayer(x = dept_pest_2007, var = familles_2007$noms2[i],
             breaks = bks,
             col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
             legend.pos = "bottomleft", legend.values.rnd = 3,
             legend.title.txt = "Qt�. en �g/l" , border = NA
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = paste(familles_2007$noms[i]," 2007"), 
              author = "", sources = "", scale=NULL , col="grey40")
  #map 2012
  choroLayer(x = dept_pest_2012, var = familles_2012$noms2[i],
             breaks = bks,
             col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
             legend.pos = "n", legend.values.rnd = 3,
             legend.title.txt = "Qt�. en �g/l" , border = NA
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = paste(familles_2012$noms[i]," 2012"), 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()

#----preparation des donn�es normes----




#----preparation des donn�es normes----


mesures$Ecart_norme <- mesures$MA_MOY - mesures$NORME_DCE

#ajout des donn�es des stations dans les mesures
mesures <- left_join(x = mesures,y =  stations, by= "CD_STATION")
mesures <- left_join(x=mesures,y=pesticides,by="LB_PARAMETRE")

grouped_mesures <- group_by(mesures, NUM_DEP, ann�e)
grouped_mesures$respect_norme <- ifelse(grouped_mesures$Ecart_norme > 0,"norme depass�e","norme respect�e")

#calcul du taux de d�passement de la norme par d�partement 
#il s'agit du nombres de mesure d�passant la norme sur le nombre de mesures total
mesures_par_dept <- grouped_mesures %>% summarise(N_respect_norme = sum(respect_norme == "norme respect�e"),
                                      N_depass_norme = sum(respect_norme == "norme depass�e"), 
                                      N_mesures = n())

mesures_par_dept$Tx_D�passement_Norme <- mesures_par_dept$N_depass_norme / mesures_par_dept$N_mesures
mesures_par_dept


mesures_par_dept_2 <- cast(data = mesures_par_dept, formula = NUM_DEP ~ ann�e , value = "Tx_D�passement_Norme") 
mesures_par_dept_3 <- cast(data = mesures_par_dept, formula = NUM_DEP ~ ann�e , value = "N_mesures") 

mesures_par_dept_2 <- mesures_par_dept_2 %>% dplyr::rename("Tx_D�passement_Norme_2007" = "2007",
                                                           "Tx_D�passement_Norme_2008" = "2008",
                                                           "Tx_D�passement_Norme_2009" = "2009",
                                                           "Tx_D�passement_Norme_2010" = "2010",
                                                           "Tx_D�passement_Norme_2011" = "2011",
                                                           "Tx_D�passement_Norme_2012" = "2012")

mesures_par_dept_3 <- mesures_par_dept_3 %>% dplyr::rename("N_mesures_2007" = "2007",
                                                           "N_mesures_2008" = "2008",
                                                           "N_mesures_2009" = "2009",
                                                           "N_mesures_2010" = "2010",
                                                           "N_mesures_2011" = "2011",
                                                           "N_mesures_2012" = "2012")

#ajout des mesures par pesticide dans la carte des departements.
dept_pest <- left_join(x=dept2 , y=mesures_par_dept_2, by=c("INSEE_DEP"="NUM_DEP") )
dept_pest <- left_join(x=dept_pest , y=mesures_par_dept_3, by=c("INSEE_DEP"="NUM_DEP") )

#----carte avec les normes-----


png(paste("./map/carte_dept_Tx_Dep_Norme.png",sep=""),  width= larg , height= haut ,res= resolution)

opar <- par(mfrow = c(2,3), mar = c(0,0,1.2,0))
bks <- getBreaks(v=mesures_par_dept$Tx_D�passement_Norme, method = "quantile", nclass = 4)
ann�e <- c("2007","2008","2009","2010","2011","2012")

i <- 1
for(i in 1:6){
  
  choroLayer(x = dept_pest, var = paste("Tx_D�passement_Norme_",ann�e[i],sep=""),
             breaks = bks,
             col = carto.pal(pal1 = "sand.pal", n1 = 4),
             legend.pos = "bottomleft", legend.values.rnd = 5,
             legend.title.txt = "Taux" , border = NA
  )
  plot(france, border="grey10", add = TRUE , lwd = 0.3)
  layoutLayer(title = paste("Taux de d�passement de la norme DCE en ",ann�e[i],sep=""), 
              author = "", source="" , scale=NULL , col="grey40")
              #sources = "Source : Minist�re de la Transition �cologique et solidaire \nM�thode : Nombre de mesures d�passant la norme DCE divis� par le nombre de mesures total \npar d�partement", scale=NULL , col="grey40")
}

dev.off()
