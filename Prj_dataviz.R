#----Cleaning memory----
rm(list = ls())

#----Loading libraries----

library(tidyverse)
library(sf)
library(rgdal)
library(cartography)


#----Setting Working Directory----

#PORTABLE RAPH
setwd("C:/Users/serou/Documents/cours/MEDAS/DATAVIZ/projet")


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

#cartes
aquiferes <- readOGR(dsn = "./data/Polygone_MasseDEauSouterraine_VEDL2013_FXX-shp"
                     , layer="PolygMasseDEauSouterraine", stringsAsFactors=FALSE)

pathAE <- "./data/ADMIN-EXPRESS-COG_1-0__SHP__FRA_2017-06-19/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2017-06-19/ADE-COG_1-0_SHP_LAMB93_FR"

dept <- readOGR(dsn = pathAE, layer="DEPARTEMENT", stringsAsFactors=FALSE)

dept2 <- st_read(dsn = pathAE, layer="DEPARTEMENT", stringsAsFactors=FALSE)

# autres dataset

regions <- read.csv2(file="./data/Reg2016.csv")
departements <- read.csv2(file="./data/depts2016.csv")

#----Sandbox-mesures----

ma_qp_fm_ttres_pesteso_2007$année <- "2007"
ma_qp_fm_ttres_pesteso_2008$année <- "2008"
ma_qp_fm_ttres_pesteso_2009$année <- "2009"
ma_qp_fm_ttres_pesteso_2010$année <- "2010"
ma_qp_fm_ttres_pesteso_2011$année <- "2011"
ma_qp_fm_ttres_pesteso_2012$année <- "2012"

mesures <- rbind(ma_qp_fm_ttres_pesteso_2007,
                 ma_qp_fm_ttres_pesteso_2008,
                 ma_qp_fm_ttres_pesteso_2009,
                 ma_qp_fm_ttres_pesteso_2010,
                 ma_qp_fm_ttres_pesteso_2011,
                 ma_qp_fm_ttres_pesteso_2012)

mesures$Ecart_norme <- mesures$MA_MOY - mesures$NORME_DCE


moyennes <- rbind(moy_tot_quantif_2007,
                  moy_tot_quantif_2008,
                  moy_tot_quantif_2009,
                  moy_tot_quantif_2010,
                  moy_tot_quantif_2011,
                  moy_tot_quantif_2012)

moyennes <- left_join(moyennes, stations, by= "CD_STATION")

departements <- left_join(departements,regions,by="REGION")

moyennes <- left_join(moyennes, departements, by=c("NUM_DEP"="DEP"))

#noms des région stocké dans NCC.y
moyennes <- moyennes %>% dplyr::rename(NOM_REG = NCC.y)




str(mesures[mesures$Ecart_norme > 0 , "MA_MOY"])

stations$CD_STATION <- as.character(stations$CD_STATION) 
ma_qp_fm_ttres_pesteso_2007$CD_STATION <- as.character(ma_qp_fm_ttres_pesteso_2007$CD_STATION)
test <- left_join(x = ma_qp_fm_ttres_pesteso_2007,y =  stations, by= "CD_STATION")

nb_me_sup_norm <- as.data.frame(table(test[test$Ecart_norme > 0 , "NUM_DEP"]))


dept3 <- left_join(x=dept2 , y=nb_me_sup_norm, by=c("INSEE_DEP"="Var1") )

dev.off()



choroLayer(dept3 , var = "Freq")
layoutLayer(title = "Nombre de mesures supérieurs à la norme DCE \npar département pour l'année 2017",
            frame = TRUE, tabtitle = TRUE)



#----carte-----


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



#----sandbox-aquiferes----
plot(aquiferes)

aquiferes_data <- aquiferes@data

head(aquiferes@data)

str(aquiferes@data[aquiferes@data$Niveau==2,])

plot(aquiferes[aquiferes@data$Niveau==2,])


