# On nettoie l'environnement de travail et on importe les packages dont on a besoin:
rm(list= ls())
setwd('/home/vgoyhex/local_server/r_dataviz/nuclear_reactors')
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

URL <-paste("https://www.data.gouv.fr/fr/datasets/r/6e15f43d-35ae-407a-b322-cc3bc93a4959")
download.file(URL, destfile = "nuclear_reactors_dataset.csv", method="auto")
list.files() #On reliste les fichiers présents dans le working directory.

#http://forums.cirad.fr/logiciel-r/viewtopic.php?t=3947
#fileencoding = "cp1252" pour quand le fichier a été créé sous windows.
nuclear_reactors <- read.csv("nuclear_reactors_dataset.csv", header=T, sep=",", fileEncoding="CP1252")
