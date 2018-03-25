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
  select(ipondi,cantville)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  collect%>%
  group_by(cantville)%>%
  summarize(total_cant = sum(ipondi))# On récupère le nombre d'individus qui rentre dans chaque catégorie.

#Nouveau collect, cette fois pour recupérer le nombre de migrants fr
total_cantville <- rp2011cvi %>%
  select(ipondi,cantville)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(dnai != '04') %>% #Je fais sauter les gens qui sont nés en PACA.
  filter(dnai != '05') %>%
  filter(dnai != '06') %>%
  filter(dnai != '13') %>%
  filter(dnai != '83') %>%
  filter(dnai != '84') %>%
  filter(dnai != '99') %>% #99 pour les migrants internationaux, qu'on ne veut pas non plus pour l'instant.
  collect%>%
  group_by(cantville)%>%
  summarize(total_migrants_fr = sum(ipondi))# On récupère le nombre d'individus qui rentre dans chaque catégorie.

#J'ajoute ma nouvelle information à ma table initiale.
test_requete <- merge(test_requete, total_cantville, by.x = "cantville", by.y = "cantville")

#Nouveau collect, cette fois pour recupérer le nombre de migrants internationaux
total_cantville <- rp2011cvi %>%
  select(ipondi,cantville)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(dnai == '99') %>% #99 pour les migrants internationaux, les seuls que l'on veut garder cette fois ci.
  collect%>%
  group_by(cantville)%>%
  summarize(total_migrants_internationaux = sum(ipondi))

#J'ajoute ma nouvelle information à ma table initiale.
test_requete <- merge(test_requete, total_cantville, by.x = "cantville", by.y = "cantville")

test_requete$percent_migrants_fr<-test_requete$total_migrants_fr/test_requete$total_cant*100
test_requete$percent_migrants_internationaux<-test_requete$total_migrants_internationaux/test_requete$total_cant*100
#-----------------------------------------------------------
#PARTIE 3: Cartographie.
load("CVetIRIS_PACA.Rdata")
cv_iris<-CVetIRIS
cv_iris@data <- merge(test_requete, cv_iris@data, by.x = "cantville", by.y = "ID")
cv_iris<-cv_iris[order(cv_iris@data$cantville),]
cv_iris.df<-fortify(cv_iris, region="cantville") #Sans fortify(), pas de carte choroplète.
cv_iris@data$cantville<- as.character(cv_iris@data$cantville)
cv_iris.df <- left_join(cv_iris.df,cv_iris@data, by=c( "id" = 'cantville')) #Mais fortify ne conserve pas les variables intéressantes, je les remet donc dedans avec un left join.

cv_iris.df$discretisation_migrants_fr<-cut(cv_iris.df$percent_migrants_fr, c(0,20,30,40,50,100),labels=c("Moins de 20%","Entre 20% et 30%","Entre 30% et 40%","Entre 40% et 50%","Plus de 50%"))
cv_iris.df$discretisation_migrants_internationaux<-cut(cv_iris.df$percent_migrants_internationaux, c(0,10,20,30,100),labels=c("Moins de 10%","Entre 10% et 20%","Entre 20% et 30%","Plus de 30%"))

#Les deux plots dont j'ai besoin:
#plot1
migrants_fr.plot<- ggplot(data=cv_iris.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=discretisation_migrants_fr))+         # draw polygons 
  geom_path(color="black", lwd = 0.1)+  # draw boundaries
  coord_equal()+
  ggtitle("Part de la population née hors PACA.")+
  #scale_colour_brewer(palette = "Blues")+
  #scale_fill_gradient(low = "white", high = "darkblue")
  scale_fill_manual(values=brewer.pal(5, "BuPu"),na.value="Darkgrey",name="Pourcentage de migrants interrégionaux au sein des cantons")

#plot2
migrants_internationaux.plot<- ggplot(data=cv_iris.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=discretisation_migrants_internationaux))+         # draw polygons 
  geom_path(color="black", lwd = 0.1)+  # draw boundaries
  coord_equal()+
  ggtitle("Part de la population née hors de France.")+
  #scale_colour_brewer(palette = "Blues")+
  #scale_fill_gradient(low = "white", high = "darkblue")
  scale_fill_manual(values=brewer.pal(5, "BuPu"),na.value="Darkgrey",name="Pourcentage de migrants internationaux au sein des cantons")

library(gridExtra)
#grid.arrange(h, f, ncol=2, top="Evolution du niveau de diplome au sein de la région PACA",)
#-----------------------------------------------------------

grid.arrange(arrangeGrob(migrants_fr.plot,
                         migrants_internationaux.plot,
                         nrow=2),
                         nrow=2,heights=c(10, 1),top=c("Part des migrants en région PACA",fontzise=20))


#PARTIE 4: CARTOGRAPHIE AVEC TMAPS.

# chargement de la fonction ad hoc
source("cartoPACA.R", local = TRUE)