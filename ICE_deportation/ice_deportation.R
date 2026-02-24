#ICE Deportation data project
#Analyser les données proposeées par le "deportation data project" https://deportationdata.org/
rm(list=ls())
library ("RCurl")
library ("feather")
library("dplyr")
library("plyr")


setwd("/home/oryx/Documents/r_dataviz/ICE_deportation")
arrest_latest <- "https://github.com/deportationdata/ice/raw/refs/heads/main/data/arrests-latest.feather"
detention_facilities_pop <- "https://github.com/deportationdata/ice/raw/refs/heads/main/data/facilities-daily-population-latest.feather"
download.file(detention_facilities_pop, destfile="facilities-daily-population-latest.feather", method="wget")

data<- read_feather("facilities-daily-population-latest.feather")
head(data)

table(data$state)
table(data$state, data$n_detained)


result <- data %>%
  group_by(state) %>%
  summarise(unique_facilities = n_distinct(detention_facility_code),
            total_records = n())

#Number of unique facilities per state
facility_per_state<- aggregate(detention_facility_code ~ state, data = data, FUN = function(x) length(unique(x)))
#Below all line where a facility is mentioned, per state (makes no sense in this case but it allows to better understand the structure of the line above
#aggregate(detention_facility_code ~ state, data = data, FUN = length)
unique(data$state) #50 states + permanently inhabited territories (american samoa, guam, northern mariana islands, puerto rico...)
resultat <- subset(facility_per_state, state == "PR") ##Il existe bien 6 lieux de détention à Puerto Rico, sous contrôle des USA...

##Mesure t-on une hausse du "intake" de prisonniers ICE par état entre 2023 et 2025?


