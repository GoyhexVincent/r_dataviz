#Goyhex Vincent, vgoyhex@gmail.com
#- RP1968-2008 : Carte (France) : où sont les jeunes actifs diplômés résidant en PACA au recensement précédent [par département]
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
rp19682008 <- tbl(con, from = "rp19682008")

# ETAPE 1: REQUETAGE DE LA BDD / Mise en forme de l'information.
glimpse(rp19682008) #Verification de la structure de la table.

test_requete <- rp19682008 %>%
  select(an_recens,dep_ran_10,dep_res_10,reg_ran_10,pond)%>%
  filter(dipl !='*')%>% #On ne veut que les jeunes actifs "diplomés" (donc à partir du CEP?)
  filter(dipl !='0')%>%
  filter(reg_ran_10 =='93')%>% #On ne conserve que la région PACA au recensement précedent.
  collect %>%
  group_by(an_recens,dep_res_10)%>%
  summarize(count_id = sum(pond))# On récupère le nombre d'individus qui rentre dans chaque catégorie.


# ETAPE 2: CARTOGRAPHIE DES RESULTATS.

test_requete$count_id<-round(as.numeric(test_requete$count_id),1)
test<-dcast(test_requete,dep_res_10 ~an_recens,value.var="count_id") #On eclate la colonne an_recens en une colonne par année, pour pouvoir ensuite faire une carto propre.

colnames(test)<-c("dep_res_10","an1968","an1975","an1982","an1990","an1999","an2008") #Il est nécéssaire de changer les noms de colonnes, sinon tmap va comprendre qu'il doit lire la colonne numéro 1968 et se planter.
# Cartes avec tmap

load("departements.Rdata") #Problème: uniquement les departements de PACA.
load("CVetIRIS_PACA.Rdata")
departements.tmap <- spTransform(departements, CVetIRIS@proj4string)
departements.tmap@data <- merge(test_requete, departements.tmap@data, by.x = "dep_res_10", by.y = "CODE_DEPT")

map<- tm_shape(departements.tmap) +
  tm_fill(c("an1968","an1975","an1982","an1990","an1999","an2008"),
          style="quantile",
          palette="Blues",
          #breaks= c(0,1000,5000,10000,20000,50000,100000,200000),
          title=c(""),
          title.size=3)+
  tm_borders() +
  tm_layout(frame = TRUE,
            outer.margins = 0,
            legend.position = c(0.05, 0),
            legend.frame = FALSE,
            legend.text.size = 0.9,
            legend.width = 0.7,
            legend.height = 2,
            legend.hist.height = 0.7,
            title=c("jeunes actifs en provenance de la région PACA \n 1968", "jeunes actifs en provenance \n de la région PACA: 1975","jeunes actifs en provenance de la région PACA \n 1982","jeunes actifs en provenance de la région PACA \n 1990","jeunes actifs en provenance de la région PACA \n 1999","jeunes actifs en provenance de la région PACA \n 2008"),
            title.snap.to.legend=TRUE,
            title.size=7,
            title.position = c("left","top"))
                                    

