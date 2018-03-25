#GOYHEX Vincent, vgoyhex@gmail.com
# Cartes : la part des migrants par canton.

#Empty memory
#-----------------------------------------------------------
rm(list=ls())

library(ggplot2)
library(sp)
library(maptools)
library(rgeos)
library(tmap)
library(dplyr)
library(dplyrr)
library(tidyr)
library(reshape2)
#library(plyr) #Faire très attention avec plyr, même quand on a besoin, si on l'appelle au début, la fonction summarize()de plyr remplacera celle de dplyr et génère des problèmes.
detach("package:plyr", unload=TRUE) #Si une erreur est renvoyée, ce n'est pas un problème.

# pour établir la connexion avec le serveur
con <- src_postgres("postgresuser", "localhost", "5432", "postgresuser", "postgres")
# pour le fichier détail RP 2011 cantons-ou-villes
rp2011cvi <- tbl(con, from = "fd_indcvi_2011")



#-----------------------------------------------------------
#PARTIE 1: REQUETAGE DE LA BDD & Mise en forme.
test_requete <- rp2011cvi %>%
  select(ipondi,cantville,iris)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  collect%>%
  group_by(cantville,iris)%>%
  summarize(total_cant = sum(ipondi))%>%# On récupère le nombre d'individus qui rentre dans chaque catégorie.
  mutate(ID = ifelse(iris %in% "ZZZZZZZZZ", cantville, iris))
#Nouveau collect, cette fois pour recupérer le nombre de migrants fr
total_cantville <- rp2011cvi %>%
  select(ipondi,cantville,iris)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(dnai != '04') %>% #Je fais sauter les gens qui sont nés en PACA.
  filter(dnai != '05') %>%
  filter(dnai != '06') %>%
  filter(dnai != '13') %>%
  filter(dnai != '83') %>%
  filter(dnai != '84') %>%
  filter(dnai != '99') %>% #99 pour les migrants internationaux, qu'on ne veut pas non plus pour l'instant.
  collect%>%
  group_by(cantville,iris)%>%
  summarize(total_migrants_fr = sum(ipondi))%>%# On récupère le nombre d'individus qui rentre dans chaque catégorie.
  mutate(ID = ifelse(iris %in% "ZZZZZZZZZ", cantville, iris))
#J'ajoute ma nouvelle information à ma table initiale.
test_requete <- merge(test_requete, total_cantville, by.x = "ID", by.y = "ID", all.x=TRUE)

#Nouveau collect, cette fois pour recupérer le nombre de migrants internationaux
total_cantville <- rp2011cvi %>%
  select(ipondi,cantville,iris)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(dnai == '99') %>% #99 pour les migrants internationaux, les seuls que l'on veut garder cette fois ci.
  collect%>%
  group_by(cantville,iris)%>%
  summarize(total_migrants_internationaux = sum(ipondi))%>%
  mutate(ID = ifelse(iris %in% "ZZZZZZZZZ", cantville, iris))

#J'ajoute ma nouvelle information à ma table initiale.
test_requete <- merge(test_requete, total_cantville, by.x = "ID", by.y = "ID", all.x=TRUE)

test_requete$percent_migrants_fr<-test_requete$total_migrants_fr/test_requete$total_cant*100
test_requete$percent_migrants_internationaux<-test_requete$total_migrants_internationaux/test_requete$total_cant*100
#Des colonnes "doublons" sont apparues, il convient de nettoyer tout ça.
test_requete<- subset(test_requete, select=-c(cantville.x,cantville.y,iris.x,iris.y))
test_requete[is.na(test_requete)] <- 0
test_requete$discretisation_migrants_fr<-cut(test_requete$percent_migrants_fr, c(0,20,30,40,50,100),labels=c("Moins de 20%","Entre 20% et 30%","Entre 30% et 40%","Entre 40% et 50%","Plus de 50%"))
test_requete$discretisation_migrants_internationaux<-cut(test_requete$percent_migrants_internationaux, c(-1,10,20,30,100),labels=c("Moins de 10%","Entre 10% et 20%","Entre 20% et 30%","Plus de 30%")) #Le premier break est à -1, pour pouvoir englober les deux valeurs NA que j'ai remplacé par des 0.

#-----------------------------------------------------------
#PARTIE 3: CARTOGRAPHIE AVEC TMAPS.

# un peu de préparation pour avoir les éléments pour une carto propre
load("CVetIRIS_PACA.Rdata")
load("departements.Rdata")
load("communesOSM.Rdata")
load("communes_ident.Rdata")

communes_ident$grandesAU <- NA
communes_ident[communes_ident$Tranche.AU10 %in% c("AU 500 000 à 9 999 999 hab.", "AU 200 000 à 499 999 hab.", "AU 100 000 à 199 999 hab.", "AU 50 000 à 99 999 hab."), "grandesAU"] <- communes_ident[communes_ident$Tranche.AU10 %in% c("AU 500 000 à 9 999 999 hab.", "AU 200 000 à 499 999 hab.", "AU 100 000 à 199 999 hab.", "AU 50 000 à 99 999 hab."), "CodeAU10"]


communes <- spTransform(communes, CVetIRIS@proj4string)
departements <- spTransform(departements, CVetIRIS@proj4string)
departements <- departements[departements@data$CODE_REG %in% "93",]

communesPACA <- communes[substr(communes@data$insee, 1, 2) %in% c("04", "05", "06", "13", "83", "84"), ]
communesPACA$size <- communes_ident[match(communesPACA$insee, communes_ident$CodeInsee), "Pop.mun07"]
communesPACA <- spTransform(communesPACA, CRSobj = proj4string(communes))
villes <- communesPACA

villes[villes$size < 140000, "nom"] <- NA
villes[villes@data$insee %in% "13001", "nom"] <- NA
communesPACA[communesPACA@data$insee %in% "13001", "nom"] <- "Aix"

shpAU <- unionSpatialPolygons(communesPACA, communes_ident[match(communesPACA@data$insee, communes_ident$CodeInsee), "grandesAU"])
shpAU <- SpatialPolygonsDataFrame(shpAU, data = data.frame(ID = names(shpAU), nom = as.character(communes_ident[match(names(shpAU), communes_ident$CodeAU10), "Libellé.AU10"]), row.names = names(shpAU), stringsAsFactors = FALSE))
shpAU@data[shpAU@data$ID %in% "112", "nom"] <- "Menton\nMonaco"
shpAU@data[shpAU@data$ID %in% "003", "nom"] <- "Aix-en-Provence\nMarseille"

# chargement de la fonction ad hoc
source("cartoPACA.R", local = TRUE)

migrants_fr = cartoPACA(test_requete, shp = CVetIRIS, key.shp="ID", key.data="ID","percent_migrants_fr", style = "fixed", breaks = c(0,20,30,40,50,100), title = "Part des migrants interregionaux (en %) \n au sein des communes de la région PACA")
migrants_internationaux = cartoPACA(test_requete, shp = CVetIRIS, key.shp="ID", key.data="ID","percent_migrants_internationaux", style = "fixed", breaks = c(0,10,20,30,100), title = "Part des migrants internationaux (en %) \n au sein des communes de la région PACA")

