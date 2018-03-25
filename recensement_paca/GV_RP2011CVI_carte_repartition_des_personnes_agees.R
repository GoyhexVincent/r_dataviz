#vgoyhex@gmail.com
#Cartes : répartition des 65-80 ans et des plus de 80 ans

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
library(MonetDB.R)

# pour établir la connexion avec le serveur
con <- src_postgres("postgresuser", "localhost", "5432", "postgresuser", "postgres")
# pour le fichier détail RP 2011 cantons-ou-villes
rp2011cvi <- tbl(con, from = "fd_indcvi_2011")


#-----------------------------------------------------------
#PARTIE 1: REQUETAGE DE LA BDD & Mise en forme.
#Une première fois pour les 65-80ans
test_requete <- rp2011cvi %>%
  select(ipondi,cantville,iris,agerev)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(agerev >'064')%>%
  filter(agerev < '081')%>%
  collect%>%
  group_by(cantville,iris)%>%
  summarize(total_6580 = sum(ipondi))%>%# On récupère le nombre d'individus qui rentre dans chaque catégorie.
  mutate(ID = ifelse(iris %in% "ZZZZZZZZZ", cantville, iris))
#Une deuxième fois pour les plus de 80ans.
test_requete2 <- rp2011cvi %>%
  select(ipondi,cantville,iris,agerev)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(agerev > '081')%>%
  collect%>%
  group_by(cantville,iris)%>%
  summarize(total_80 = sum(ipondi))%>%# On récupère le nombre d'individus qui rentre dans chaque catégorie.
  mutate(ID = ifelse(iris %in% "ZZZZZZZZZ", cantville, iris))
test_requete <- merge(test_requete, test_requete2, by.x = "ID", by.y = "ID", all.x=TRUE)
#Une troisième fois parceque j'ai un doute sur ce que vous voulez comme donnée:
#Une première fois pour les 65-80ans
test_requete2 <- rp2011cvi %>%
  select(ipondi,cantville,iris,agerev)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  collect%>%
  group_by(cantville,iris)%>%
  summarize(total = sum(ipondi))%>%# On récupère le nombre d'individus qui rentre dans chaque catégorie.
  mutate(ID = ifelse(iris %in% "ZZZZZZZZZ", cantville, iris))

test_requete <- merge(test_requete, test_requete2, by.x = "ID", by.y = "ID", all.x=TRUE)
test_requete<- subset(test_requete, select=-c(cantville.y,iris.y,iris.x,cantville.x))
test_requete[is.na(test_requete)] <- 0

#J'ai ici un petit doute sur ce que vous voulez: La part des 65-80 et 80+ par rapport au total de chaque canton?
test_requete$part_canton_6580<-test_requete$total_6580/sum(test_requete$total)*100
test_requete$part_canton_80<-test_requete$total_80/sum(test_requete$total)*100


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

#Pour la part de personnes agées par rapport à la pop totale du canton (en %):
part_canton_6580 = cartoPACA(test_requete, shp = CVetIRIS, key.shp="ID", key.data="ID","part_canton_6580", style = "fixed", breaks = c(0,0.02,0.04,0.06,0.8,0.1), title = "Part des personnes agées entre 65 et 80 ans \n au sein des communes de la région PACA")
part_canton_80 = cartoPACA(test_requete, shp = CVetIRIS, key.shp="ID", key.data="ID","part_canton_80", style = "fixed", breaks = c(0,0.005,0.01,0.015,0.02,0.025), title = "Part des personnes de plus de 80 ans \n au sein des communes de la région PACA")


# Pour des statistiques aussi faibles, est ce que les % sont adaptés ou ne vaudrait-il pas mieux passer sur les chiffres réels?