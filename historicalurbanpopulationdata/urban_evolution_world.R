# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage
packages <- c("ggplot2", "ggthemes", "rworldmap", "classInt", "gridExtra", "grid", "cowplot")
ipak(packages)

#######################
# First things first: #
#######################

# remove old variables
rm(list= ls())
#Setting the right Working directory.
getwd() # Current working directory is:"/home/vgoyhex"
setwd("/home/vgoyhex/local_server/r_dataviz/historicalurbanpopulationdata/") #Setting the Current working directory to our GIT repo.


#Following Spatial.ly 5000 years of city growth tutorial: http://spatial.ly/2017/03/mapping-5000-years-of-city-growth/
cities<- load("AllObjects.RData") 






