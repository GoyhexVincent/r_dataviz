#https://www.kaggle.com/datasets/wafaaelhusseini/major-cities-weather-data
#install.packages("arrow")
library("arrow")
library('corrr')
library("ggcorrplot")
library("FactoMineR")
library("factoextra")
options(digits=5) #combien de chiffres après la virgule on tolère, paramétrage système.

setwd("/home/oryx/Documents/r_dataviz/world_weather/")
rm(list=ls())

data<-read_parquet("history.parquet")
colnames(data)
unique(data$country)
head(data$date)
unique(substr(data$date,1,4))

#vérifier la présence de valeurs nulles
colSums(is.na(data))
data <- na.omit(data)
head(data)
#enlever les colonnes qualitatives pour pouvoir performer une ACP
numerical_data <- data[,6:20]
numerical_data <- numerical_data[ , !(names(numerical_data) == "wind_dir_dom_deg")]

data.pca <- princomp(numerical_data)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE) #identifier la contribution de chaque dimension à la comprehension du jeu de donnée
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1:2)










