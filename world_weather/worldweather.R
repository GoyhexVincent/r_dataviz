#https://www.kaggle.com/datasets/wafaaelhusseini/major-cities-weather-data
#install.packages("arrow")
library("arrow")
library('corrr')
library("ggcorrplot")
library("FactoMineR")


setwd("/home/oryx/Documents/r_dataviz/world_weather/")
rm(list=ls())

data<-read_parquet("history.parquet")
colnames(data)
unique(data$country)
head(data$date)
unique(substr(data$date,1,4))

#vérifier la présence de valeurs nulles
colSums(is.na(data))
head(data)
#enlever les colonnes qualitatives pour pouvoir performer une ACP
numerical_data <- data[,6:20]
numerical_data <- subset( numerical_data, select = -numerical_data$"wind_dir_dom_deg" )
head(numerical_data)
data.pca <- princomp(numerical_data)
