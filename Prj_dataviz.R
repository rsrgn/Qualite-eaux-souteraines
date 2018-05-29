#----Cleaning memory----
rm(list = ls())

#----Loading libraries----

library(tidyverse)

#Cartographie
library(sf)
library(SpatialPosition)
library(cartography)

library(gridExtra)

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

#----praparation des donnes carte----

str(mesures[mesures$Ecart_norme > 0 , "MA_MOY"])

stations$CD_STATION <- as.character(stations$CD_STATION) 
ma_qp_fm_ttres_pesteso_2012$CD_STATION <- as.character(ma_qp_fm_ttres_pesteso_2012$CD_STATION)

#ajout des données des stations dans les mesures
ma_qp_fm_ttres_pesteso_2012 <- left_join(x = ma_qp_fm_ttres_pesteso_2012,y =  stations, by= "CD_STATION")
grouped_mesures <- group_by(ma_qp_fm_ttres_pesteso_2012, NUM_DEP, LB_PARAMETRE)
mesures_par_dept <- na.omit(summarise(grouped_mesures, moy_dept=mean(MA_MOY)))



mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
smoothLayer(x = mtq, var = 'P13_POP',
            span = 4000, beta = 2, breaks = c(0,5000,seq(10000,110000,10000)),
            mask = mtq, border = NA,
            col = carto.pal(pal1 = 'wine.pal', n1 = 12),
            legend.title.txt = "Population\nPotential",
            legend.pos = "topright", legend.values.rnd = -2)
propSymbolsLayer(x = mtq, var = "P13_POP", legend.pos = c(690000, 1599950),
                 legend.title.txt = "Population 2013",
                 col = NA, border = "#ffffff50")
layoutLayer(title = "Actual and Potential Popultation in Martinique",
            author = "INSEE, 2016", sources = "")



# Plot dentsity of population
mtq$dens <- mtq$P13_POP / (st_area(mtq) / (1000 * 1000))
mygrid$densitykm <- mygrid$P13_POP / (mygrid$gridarea / (1000 * 1000))


bks <- getBreaks(v = dept_AMPA$moy_dept, method = "q6")
cols <- carto.pal(pal1 = "taupe.pal", n1 = length(bks)-1)
opar <- par(mfrow = c(1,2), mar = c(0,0,0,0))

choroLayer(x = dept_AMPA, var = "moy_dept", breaks = bks,
           border = "burlywood3", col = cols,
           legend.pos = "topright", legend.values.rnd = 4,
           legend.title.txt = "Population density")

mygrid <- getGridLayer(x = dept_AMPA, cellsize = 50000 * 50000,
                       type = "regular", var = "moy_dept")

choroLayer(x = mygrid, var = "moy_dept", breaks = bks,
           border = "burlywood3", col = cols,
           #legend.pos = "n", 
           legend.values.rnd = 4,
           legend.title.txt = "Population density")



smoothLayer(x = dept_AMPA, var = 'moy_dept',
            span = 75000, beta = 2, #breaks = bks,
            mask = france, border = NA,
            col = c(rev(carto.pal("green.pal", 3)), carto.pal("orange.pal", 5)),
            legend.pos = "topright", legend.values.rnd = 4)

points(x=ma_qp_fm_ttres_pesteso_2012$X_FICT_L93,
       y=ma_qp_fm_ttres_pesteso_2012$Y_FICT_L93,add=T)


mesures_par_dept_endrine <- mesures_par_dept %>% filter(LB_PARAMETRE == "Endrine")
mesures_par_dept_AMPA <- mesures_par_dept %>% filter(LB_PARAMETRE == "AMPA")

dept3 <- left_join(x=dept2 , y=mesures_par_dept_endrine, by=c("INSEE_DEP"="NUM_DEP") )
dept_AMPA <- left_join(x=dept2 , y=mesures_par_dept_AMPA, by=c("INSEE_DEP"="NUM_DEP") )


dev.off()

choroLayer(dept_AMPA , var = "moy_dept")
layoutLayer(title = "AMPA / dept",
            frame = TRUE, tabtitle = TRUE)

?choroLayer

# list of breaks
v <- c(0.05, 0.1, 0.2,0.3,0.35, 0.4, 0.45,0.5,1)
# set a color palette
cols <- c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4))
cols2 <- c(rev(carto.pal("green.pal", 7)), carto.pal("orange.pal", 7))



mygrid <- getGridLayer(x = dept_AMPA, var = "moy_dept", cellsize = 100000 * 100000, type = "regular")
bks <- getBreaks(v=mygrid$moy_dept, method = "q6")
length(bks)
cols <- carto.pal(pal1 ="red.pal", n1=length(bks)-1)
choroLayer(mygrid, 
           var = "moy_dept",
           col = cols, breaks = bks,
           legend.values.rnd = 3)



summary(mygrid$moy_dept)
plot(dept3$geometry, border = NA, col = NA, bg = "#A6CAE0")

plot(mygrid$geometry)

dept4 <-  as(dept3,"Spatial")

france <- st_union(dept_AMPA)
france <- as(france,"Spatial")
plot(france)

# compute & display the potential map

par(mfrow=c(1,2))


choroLayer(mygrid, 
           var = "moy_dept",
           col = cols2,
           legend.values.rnd = 3)


cols <- c(rev(carto.pal("green.pal", 4)), carto.pal("orange.pal", 4))

smoothLayer(x = dept_AMPA, 
            var = "moy_dept",
            #breaks=bks,
            span = 0.65e+05, beta = 2, mask = france, 
            resolution = 5000, col = cols, 
            legend.title.txt = "AMPA", 
            border = "grey80", lwd = 0.5,
            legend.values.rnd = 3)

bks <- getBreaks(v=dept_AMPA$moy_dept, method = "q6")
length(bks)
cols <- c(rev(carto.pal("green.pal", length(bks)/2 )), carto.pal("orange.pal", length(bks)/2))
choroLayer(dept_AMPA , 
           var = "moy_dept",
           col = cols, breaks = bks,
           legend.title.txt = "AMPA",
           legend.values.rnd = 3)

dept_AMPA$area <- st_area(dept_AMPA)
dept_AMPA$moy_dept_by_m2 <- dept_AMPA$moy_dept / dept_AMPA$area
choroLayer(dept_AMPA , var = "moy_dept_by_m2",col = cols,legend.title.txt = "AMPA",legend.values.rnd = 15)

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


