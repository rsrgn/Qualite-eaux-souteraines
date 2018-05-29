#----Cleaning memory----
rm(list = ls())

#----Loading libraries----

library(tidyverse)
library(reshape)

#Cartographie
library(sf)
library(SpatialPosition)
library(cartography)


library(gridExtra)

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




#----Préparation des données----

#ajout de l'année aux fichiers des mesures
ma_qp_fm_ttres_pesteso_2007$année <- "2007"
ma_qp_fm_ttres_pesteso_2008$année <- "2008"
ma_qp_fm_ttres_pesteso_2009$année <- "2009"
ma_qp_fm_ttres_pesteso_2010$année <- "2010"
ma_qp_fm_ttres_pesteso_2011$année <- "2011"
ma_qp_fm_ttres_pesteso_2012$année <- "2012"

#fusion des mesure dans un df unique
mesures <- rbind(ma_qp_fm_ttres_pesteso_2007,
                 ma_qp_fm_ttres_pesteso_2008,
                 ma_qp_fm_ttres_pesteso_2009,
                 ma_qp_fm_ttres_pesteso_2010,
                 ma_qp_fm_ttres_pesteso_2011,
                 ma_qp_fm_ttres_pesteso_2012)

#calcul de l'écart par rapport à la norme pour toutes les mesures
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

#exploration des données de moyennes par stations
summary(moyennes$MOYPTOT)
quantile(moyennes$MOYPTOT,0.9992)
moyennes %>% filter(MOYPTOT > 50) %>% select(ANNEE, MOYPTOT, NOM_REG)

#graphique barplot des moyennes de mesures par stations
p1 <- ggplot(data=moyennes, aes(x=NOM_REG, y=MOYPTOT, group=NOM_REG) )  
p1 <- p1 +  geom_boxplot() #+ facet_wrap(~ NOM_REG)
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1

#suppression des 4 valeurs supérieurs à 50
moyennes2 <- moyennes %>% filter(MOYPTOT < 50) 
grouped_moyennes <- group_by(moyennes2,ANNEE, NOM_REG)
per_region <- summarise(grouped_moyennes, n_region=n())
par_region <- na.omit(summarise(grouped_moyennes, moy_reg=mean(MOYPTOT)))

#----graphique evolution mesure pesticides par regions----
dev.off() 
png(paste("./graph/graph1.png",sep=""),  width= 2000 , height= 1200 ,res=250)

p1 <- ggplot(data=par_region, aes(y=moy_reg, x=ANNEE, group=NOM_REG, colour=NOM_REG ) )  
p1 <- p1 +  geom_line(size=1.2)+ facet_wrap(~ NOM_REG)
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + theme(axis.text.x=element_text(angle=90))
p1 <- p1 + ggtitle("Evolution des mesures de pesticides par régions et par années\ndans les eaux souteraines en France")
p1 <- p1 + xlab("Années") + ylab("Quantité de pesticides détectée (en µg/l)")
p1 <- p1 + labs(caption = "Source: Ministère de la Transition écologique et solidaire")
p1
dev.off()

#----Contour France----

france <- st_union(dept2)
france <- as(france,"Spatial")

#Paramètres graphiques

larg <- 7500
haut <- 4000
resolution <- 900


#----praparation des données carte 2012---

stations$CD_STATION <- as.character(stations$CD_STATION) 
ma_qp_fm_ttres_pesteso_2012$CD_STATION <- as.character(ma_qp_fm_ttres_pesteso_2012$CD_STATION)

#ajout des données des stations dans les mesures
ma_qp_fm_ttres_pesteso_2012 <- left_join(x = ma_qp_fm_ttres_pesteso_2012,y =  stations, by= "CD_STATION")
ma_qp_fm_ttres_pesteso_2012 <- left_join(x=ma_qp_fm_ttres_pesteso_2012,y=pesticides,by="LB_PARAMETRE")

grouped_mesures_2012 <- group_by(ma_qp_fm_ttres_pesteso_2012, NUM_DEP, CODE_FAMILLE)
mesures_par_dept_2012 <- na.omit(summarise(grouped_mesures_2012, moy_dept=mean(MA_MOY)))

#supressions des caractères speciaux dans les titres de colonnes
mesures_par_dept_2012$CODE_FAMILLE <- gsub("é","e",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("è","e",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("ê","e",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("à","a",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("ï","i",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub(" ","_",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub("\\(","",mesures_par_dept_2012$CODE_FAMILLE)
mesures_par_dept_2012$CODE_FAMILLE <- gsub(")","",mesures_par_dept_2012$CODE_FAMILLE)

mesures_par_dept_2012_2 <- cast(data = mesures_par_dept_2012, NUM_DEP ~ CODE_FAMILLE )

#ajout des mesures par pesticide dans la carte des departements.
dept_pest_2012 <- left_join(x=dept2 , y=mesures_par_dept_2012_2, by=c("INSEE_DEP"="NUM_DEP") )

#vecteur de titres des cartes

familles_2012 <- data.frame(noms=unique(ma_qp_fm_ttres_pesteso_2012$CODE_FAMILLE))
familles_2012$noms2 <- familles_2012$noms

familles_2012$noms2 <- gsub("é","e",familles_2012$noms2)
familles_2012$noms2 <- gsub("è","e",familles_2012$noms2)
familles_2012$noms2 <- gsub("ê","e",familles_2012$noms2)
familles_2012$noms2 <- gsub("à","a",familles_2012$noms2)
familles_2012$noms2 <- gsub("ï","i",familles_2012$noms2)
familles_2012$noms2 <- gsub(" ","_",familles_2012$noms2)
familles_2012$noms2 <- gsub("\\(","",familles_2012$noms2)
familles_2012$noms2 <- gsub(")","",familles_2012$noms2)

familles_2012 <- familles_2012[order(familles_2012$noms2),]
familles_2012 

#----map 2012 - lissées----


png(paste("./map/carte_lissee_Moy_FamillesPesticide_2012.png",sep=""),  width= larg , height= haut ,res=resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
    smoothLayer(x = dept_pest_2012, var = familles_2012$noms2[i],
              span = 75000, beta = 2, #breaks = bks,
              mask = france, border = NA,
              col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
              legend.pos = "topleft", legend.values.rnd = 3,
              legend.title.txt = "Qté. en µg/l"
              )
  layoutLayer(title = familles_2012$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()


#----map 2012 - par départements ----


png(paste("./map/carte_dept_Moy_FamillesPesticide_2012.png",sep=""),  width= larg , height= haut ,res=resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
  bks <- getBreaks(v=mesures_par_dept_2012_2[,familles_2012$noms2[i]], method = "quantile", nclass = 8)
  choroLayer(x = dept_pest_2012, var = familles_2012$noms2[i],
              breaks = bks,
              col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
              legend.pos = "bottomleft", legend.values.rnd = 4,
              legend.title.txt = "Qté. en µg/l",
              border = NA
  )
  
  plot(france, border="grey15", add = TRUE , lwd = 0.5)

  layoutLayer(title = familles_2012$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()




#----praparation des données cartes 2007---

#ajout des données des stations dans les mesures
ma_qp_fm_ttres_pesteso_2007 <- left_join(x = ma_qp_fm_ttres_pesteso_2007,y =  stations, by = "CD_STATION")
ma_qp_fm_ttres_pesteso_2007 <- left_join(x = ma_qp_fm_ttres_pesteso_2007,y = pesticides,by = "LB_PARAMETRE")

grouped_mesures_2007 <- group_by(ma_qp_fm_ttres_pesteso_2007, NUM_DEP, CODE_FAMILLE)
mesures_par_dept_2007 <- na.omit(summarise(grouped_mesures_2007, moy_dept=mean(MA_MOY)))

#supressions des caractères speciaux dans les titres de colonnes
mesures_par_dept_2007$CODE_FAMILLE <- gsub("é","e",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("è","e",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("ê","e",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("à","a",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("ï","i",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub(" ","_",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub("\\(","",mesures_par_dept_2007$CODE_FAMILLE)
mesures_par_dept_2007$CODE_FAMILLE <- gsub(")","",mesures_par_dept_2007$CODE_FAMILLE)

mesures_par_dept_2007_2 <- cast(data = mesures_par_dept_2007, NUM_DEP ~ CODE_FAMILLE )

#ajout des mesures par pesticide dans la carte des departements.
dept_pest_2007 <- left_join(x=dept2 , y=mesures_par_dept_2007_2, by=c("INSEE_DEP"="NUM_DEP") )

#vecteur de titres des cartes

familles_2007 <- data.frame(noms=unique(ma_qp_fm_ttres_pesteso_2007$CODE_FAMILLE))
familles_2007$noms2 <- familles_2007$noms

familles_2007$noms2 <- gsub("é","e",familles_2007$noms2)
familles_2007$noms2 <- gsub("è","e",familles_2007$noms2)
familles_2007$noms2 <- gsub("ê","e",familles_2007$noms2)
familles_2007$noms2 <- gsub("à","a",familles_2007$noms2)
familles_2007$noms2 <- gsub("ï","i",familles_2007$noms2)
familles_2007$noms2 <- gsub(" ","_",familles_2007$noms2)
familles_2007$noms2 <- gsub("\\(","",familles_2007$noms2)
familles_2007$noms2 <- gsub(")","",familles_2007$noms2)

familles_2007 <- familles_2007[order(familles_2007$noms2),]
familles_2007

#----map 2017 lissée ----


png(paste("./map/carte_lissee_Moy_FamillesPesticide_2007.png",sep=""),  width= larg , height= haut ,res=resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
  smoothLayer(x = dept_pest_2007, var = familles_2007$noms2[i],
              span = 75000, beta = 2, #breaks = bks,
              mask = france, border = NA,
              col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
              legend.pos = "topleft", legend.values.rnd = 3,
              legend.title.txt = "Qté. en µg/l"
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = familles_2007$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()


#----map 2007 - par départements ----

png(paste("./map/carte_dept_Moy_FamillesPesticide_2007.png",sep=""),  width= larg , height= haut ,res= resolution)

opar <- par(mfrow = c(4,4), mar = c(0,0,1.2,0))

i<-1
for(i in 1:16){
  bks <- getBreaks(v=mesures_par_dept_2007_2[,familles_2007$noms2[i]], method = "quantile", nclass = 8)
  choroLayer(x = dept_pest_2007, var = familles_2007$noms2[i],
             breaks = bks,
             col = c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4)),
             legend.pos = "bottomleft", legend.values.rnd = 3,
             legend.title.txt = "Qté. en µg/l" , border = NA
  )
  plot(france, border="grey15", add = TRUE , lwd = 0.5)
  layoutLayer(title = familles_2007$noms[i], 
              author = "", sources = "", scale=NULL , col="grey40")
  
}

dev.off()



#----preparation des données normes----

mesures$Ecart_norme <- mesures$MA_MOY - mesures$NORME_DCE

#ajout des données des stations dans les mesures
mesures <- left_join(x = mesures,y =  stations, by= "CD_STATION")
mesures <- left_join(x=mesures,y=pesticides,by="LB_PARAMETRE")

grouped_mesures <- group_by(mesures, NUM_DEP, année)
grouped_mesures$respect_norme <- ifelse(grouped_mesures$Ecart_norme > 0,"norme depassée","norme respectée")


mesures_par_dept <- na.omit(summarise(grouped_mesures, resp_norme = n()  ))
sum(mesures_par_dept$resp_norme)


#supressions des caractères speciaux dans les titres de colonnes
mesures_par_dept$CODE_FAMILLE <- gsub("é","e",mesures_par_dept$CODE_FAMILLE)
mesures_par_dept$CODE_FAMILLE <- gsub("è","e",mesures_par_dept$CODE_FAMILLE)
mesures_par_dept$CODE_FAMILLE <- gsub("ê","e",mesures_par_dept$CODE_FAMILLE)
mesures_par_dept$CODE_FAMILLE <- gsub("à","a",mesures_par_dept$CODE_FAMILLE)
mesures_par_dept$CODE_FAMILLE <- gsub("ï","i",mesures_par_dept$CODE_FAMILLE)
mesures_par_dept$CODE_FAMILLE <- gsub(" ","_",mesures_par_dept$CODE_FAMILLE)
mesures_par_dept$CODE_FAMILLE <- gsub("\\(","",mesures_par_dept$CODE_FAMILLE)
mesures_par_dept$CODE_FAMILLE <- gsub(")","",mesures_par_dept$CODE_FAMILLE)

mesures_par_dept_2 <- cast(data = mesures_par_dept, NUM_DEP ~ CODE_FAMILLE )

#ajout des mesures par pesticide dans la carte des departements.
dept_pest <- left_join(x=dept2 , y=mesures_par_dept_2, by=c("INSEE_DEP"="NUM_DEP") )

#vecteur de titres des cartes

familles <- data.frame(noms=unique(mesures$CODE_FAMILLE))
familles$noms2 <- familles$noms

familles$noms2 <- gsub("é","e",familles$noms2)
familles$noms2 <- gsub("è","e",familles$noms2)
familles$noms2 <- gsub("ê","e",familles$noms2)
familles$noms2 <- gsub("à","a",familles$noms2)
familles$noms2 <- gsub("ï","i",familles$noms2)
familles$noms2 <- gsub(" ","_",familles$noms2)
familles$noms2 <- gsub("\\(","",familles$noms2)
familles$noms2 <- gsub(")","",familles$noms2)

familles <- familles[order(familles$noms2),]
familles 






#----carte avec les normes-----







#dimmentionnement 

dimcarte <- st_bbox(dept3)
dimcarte
largeurcarte <- round((dimcarte[1,2] - dimcarte[1,1] ), 0)
hauteurcarte <- round((dimcarte[2,2] - dimcarte[2,1] ), 0)
largeurimage <- 1920 #1920  < 3840 < 7680
hauteurimage <- round((( largeurimage * hauteurcarte) / largeurcarte ),0)

#taille des caractere : labels
Taille_labels <- 0.35
TailleNombres <- 0.6

#resolution des cartes
Resolution <- 750 # *1500



#nom de la carte et du fichier export?
Nom_carte <- paste("Nb licenciement par dept Martinique_",Version)
Nom_carte2 <- "Martinique"
#fichier image

dev.off() 
png(paste("./map/test.png",sep="")  ,res=750)
plot(dept3)
choroLayer(dept3 , var = "Freq")
layoutLayer(title = "Nombre de mesures supérieurs à la norme DCE \npar département pour l'année 2017",
            frame = TRUE, tabtitle = TRUE)
dev.off() 

