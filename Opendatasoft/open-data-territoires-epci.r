#Author: Goyhex vincent
#vgoyhex@gmail.com

#Remise en forme en R.
#Utilisons opendatasoft pour générer un Webcrawler qui ira pomper toute la donnée OPENDATA d'un territoire donné:

#######################
# ETAPE 0: NETTOYAGE: #
#######################

# On nettoie l'environnement de travail et on importe les packages dont on a besoin:
rm(list= ls())
setwd('/home/vgoyhex/local_server/r_dataviz/Opendata')
getwd()
list.files()
library(dplyr)


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

################################
# ETAPE 2: ANALYSE DES DONNEES #
################################