#Author: Goyhex vincent
#vgoyhex@gmail.com

#Remise en forme en R.
#Utilisons opendatasoft pour générer un Webcrawler qui ira pomper toute la donnée OPENDATA d'un territoire donné:

install.packages("maptools")
install.packages("ggplot2")
install.packages("rgdal")
#######################
# ETAPE 0: NETTOYAGE: #
#######################

# On nettoie l'environnement de travail et on importe les packages dont on a besoin:
rm(list= ls())
setwd('/home/vgoyhex/local_server/r_dataviz/Opendatasoft')
getwd()
list.files()
library(dplyr)
library(rgdal)
library (ggplot2)
library(plyr)
library(sf)
library (cartography)


#####################################
# ETAPE 1: RECUPERATION DES DONNEES #
#####################################

#CSV Recensant les portails opendata FR: principalement des communautés de communes et communautés d'agglomération:
# Pour plus d'infos sur les EPCI en France:
# https://fr.wikipedia.org/wiki/%C3%89tablissement_public_de_coop%C3%A9ration_intercommunale
data <- read.csv2("open-data-territoires-epci.csv", header=T, sep=";")
head(data)
colnames(data)


#On va commencer par uniquement s'intéresser aux résultats présents au pays basque:
dept_64 <-(filter(data, dept==64))
dept_64$epci # CA du Pays Basque -> On va se baser sur celui là.
pays_basque <- subset(dept_64, epci=="CA du Pays Basque")
#On génère l'URL dont on a besoin:

URL <- paste(pays_basque$domain_url, "/explore/download/", sep="")
download.file(URL, destfile = "dataset.csv", method="auto")
dataset_pays_basque <- read.csv2("dataset.csv", header=T, sep=";")
dataset_pays_basque[1] # La liste des jeux de données pour le CA du Pays Basque.

#On va maintenant boucler sur les valeurs de "datasetid" pour:
# 1 - récuperer les noms de fichiers qu'on veut récuperer
# 2 - Générer l'URL du fichier à télécharger
# 3 - Télécharger le fichier
# 4 - Le charger dans R
for (value in dataset_pays_basque$datasetid) {
print(value)
URL <-paste(pays_basque$domain_url, "/explore/dataset/",(value),"/download/?format=csv", sep="")
download.file(URL, destfile = paste(value,".csv", sep=""), method="auto")
}
list.files() #On reliste les fichiers présents dans le working directory.

#Enfin, il nous faut récuperer le fond de carte de toutes les communes de france (overkilll) pour pouvoir
#faire des visualisations intéressantes de la donnée.
#https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/
#Puisque la ressource était assez lourde, j'ai pris la liberté de la télécharger hors du process, je vais l'importer direct depuis mon dépot.


################################
# ETAPE 2: ANALYSE DES DONNEES #
################################


couverture_telecom <- read.csv2("couverture-2g-3g-4g-en-france-par-operateur-juillet-2015.csv", header=T, sep=";")
head(couverture_telecom)
g4 <-filter(couverture_telecom, operateur=="Tout Opérateur", var2 =="population 4G", code_departement=="64", couverture!="0.0")
g4 <- g4[,c("operateur","var2","couverture", "nom_commune", "code_insee", "population_commune")]


liste <- unique(g4[,c("code_insee")])
typeof(liste)

#https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/#_
#On récupère la derniere version à jour des communes française, depuis Openstreetmap.
shape <- readOGR(dsn = ".", layer = "communes-20180101")
shape.df <- as(shape, "data.frame")

proj4string(shape) # WGS84.
commune_pays_basque <- subset(shape, shape$insee %in% liste)
#On va merger l'info couverture 4g avec les polygones des communes du pays basque
commune_pays_basque <- merge(commune_pays_basque, g4, by.x="insee", by.y="code_insee") 

commune_pays_basque.for <- fortify(commune_pays_basque, region="nom_commune")
# On renomme les champs pour pouvoir faire une jointure ensuite.
colnames(commune_pays_basque.for) <- c('long','lat','order','hole','piece','nom_commune','group') 
commune_pays_basque.for = plyr::join(x = commune_pays_basque.for,y = commune_pays_basque@data) # Jointure par ID
commune_pays_basque.for$couverture <- as.numeric(as.character(commune_pays_basque.for$couverture))

########################################
# ETAPE 4: VISUALISATION DE LA DONNEE  #
########################################

#Merci à TimoGrossenbacher pour son thème plutôt efficace:
#https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


map <- ggplot()+
  geom_polygon(data = commune_pays_basque.for, 
            aes(x = long, y = lat, fill = couverture, group = group),
            color = 'gray', size = .2)+
  scale_colour_gradient(low = "white", high = "black")+
  geom_path(data = commune_pays_basque.for,
            aes(x = long, y = lat, group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  # add the previously defined basic theme
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "Couverture 4g au Pays basque (Fr)", 
       subtitle = "2014", # à verifier
       caption = "Source: Opendatasoft (à corriger)")

print(map)
map_projected <- map +
  coord_map()








print(map_projected)

# Est ce que ça vaut le coup d'aller plus bas et de regarder directement une commune en particulier?
hasparren <- filter(couverture_telecom, nom_commune=="HASPARREN")


#################################################
# ETAPE 3: SUPPRESSION DES FICHIERS TELECHARGES #
#################################################
# directory_files <-list.files()
# value = "README"
# for (file in directory_files) {
#   if (grepl (value, file) = TRUE){
#     file.remove(file)
#   }
# }

