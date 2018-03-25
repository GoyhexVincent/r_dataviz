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
  select(an_recens,dep_ran_10,reg_res_10,reg_ran_10,pond)%>%
  filter(reg_res_10 == '93') %>%          #On ne conserve que la région PACA
  filter(dipl !='*')%>% #On ne veut que les jeunes actifs "diplomés" (donc à partir du CEP?)
  filter(dipl !='0')%>%
  collect %>%
  mutate(pond=replace(pond, reg_ran_10=='93', NA)) %>% #J'enlève dès maintenant les resultats internes à la region PACA, sinon la discretisation future tombe à l'eau.
  
  group_by(an_recens,reg_res_10,dep_ran_10)%>%
  summarize(count_id = sum(pond))# On récupère le nombre d'individus qui rentre dans chaque catégorie.


# ETAPE 2: CARTOGRAPHIE DES RESULTATS.

test_requete$count_id<-round(as.numeric(test_requete$count_id),1)
test<-dcast(test_requete,dep_ran_10 ~an_recens) #On eclate la colonne an_recens en une colonne par année, pour pouvoir ensuite faire une carto propre.
colnames(test)<-c("dep_ran_10","an1968","an1975","an1982","an1990","an1999","an2008") #Il est nécéssaire de changer les noms de colonnes, sinon tmap va comprendre qu'il doit lire la colonne numéro 1968 et se planter.
# Cartes avec tmap

load("departements.Rdata") #Problème: uniquement les departements de PACA.
load("CVetIRIS_PACA.Rdata")
departements.ggplot <- spTransform(departements, CVetIRIS@proj4string)
departements.ggplot@data <- merge(test_requete, departements.ggplot@data, by.x = "dep_ran_10", by.y = "CODE_DEPT")

#Avec GGPLOT2
cbbPalette <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

departements.ggplot<-departements.ggplot[order(departements.ggplot@data$ID_GEOFLA),]
departements.df<-fortify(departements, region="ID_GEOFLA") #Sans fortify(), pas de carte choroplète.
departements.ggplot@data$ID_GEOFLA<- as.character(departements.ggplot@data$ID_GEOFLA)
departements.df <- left_join(departements.df,departements.ggplot@data, by=c( "id" = 'ID_GEOFLA')) #Mais fortify ne conserve pas les variables intéressantes, je les remet donc dedans avec un left join.
#Je reprend la discretisation de tmaps, qui était plutot reussie:
departements.df$discretisation<-cut(departements.df$count_id, c(0,1676,4082,7947,15908,25447),labels=c("Moins de 1700 jeunes actifs","Entre 1700 et 4100 jeunes actifs","Entre 4100 et 8000 jeunes actifs","Entre 8000 et 15900 jeunes actifs","Entre 15900 et 25500 jeunes actifs"))

ggplot(data=departements.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=discretisation))+         # draw polygons 
  geom_path(color="black", lwd = 0.1)+  # draw boundaries
  coord_equal()+
  ggtitle("Localisation au précédent recensement des jeunes actifs diplomés en région PACA")+
  #scale_colour_brewer(palette = "Blues")+
  #scale_fill_gradient(low = "white", high = "darkblue")+
  scale_fill_manual(values=brewer.pal(5, "YlOrRd"),na.value="Darkgrey",name="Localisation précédente des jeunes actifs")+
  facet_wrap(~an_recens)

