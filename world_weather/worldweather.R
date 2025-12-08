#https://www.kaggle.com/datasets/wafaaelhusseini/major-cities-weather-data
#install.packages("arrow")
library("arrow")

setwd("/home/oryx/Documents/r_dataviz/world_weather/")
rm(list=ls())

data<-read_parquet("history.parquet")
colnames(data)
unique(data$country)
head(data$date)
unique(substr(data$date,1,4))

