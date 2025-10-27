library (ggplot2)
library (dplyr)
library(plyr)
library (scales)
library(gridExtra)
library(gtable)
library(grid)
library(RColorBrewer)





#setup eRColorBrewer#setup env de travail
rm(list=ls())
#setwd("D:\\Users\\vgoyhex\\Documents\\data_vgoyhex\\2026_CIO") #lien rectorat
setwd("~/Documents/SAIO/SAIO2026/2026_CIO")
#chargement des données
data2025 <- read.csv2("candidats_reos_2025.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2025$annee <- "2025"
data2024 <- read.csv2("candidats_reos_2024.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2024$annee <- "2024"
data2023<- read.csv2("candidats_reos_2023.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2023$annee <- "2023"
data2022<- read.csv2("candidats_reos_2022.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2022$annee <- "2022"
data <- rbind(data2025, data2024, data2023, data2022)
data[is.na(data)] <- 0
head(data)
colnames(data)
unique(data$annee)

data_plot <- ddply(data, .(annee, `Profil synthétique candidat`), numcolwise(sum))
data_plot$varPA=(data_plot$`Nb de cddt ayant au moins une proposition Parcoursup`/data_plot$`Nb de candidats ayant fait au moins un vœu confirmé PP`)

#Graphique:
plot_reo <- subset(data_plot,`Profil synthétique candidat`!='Non scolarisés')
plot_ns <- subset(data_plot,`Profil synthétique candidat`=="Non scolarisés")

#Plot 1: line plot evolution du taux de succès des réos/non scolarisés
plot <- ggplot()+
  geom_line(data=plot_reo, aes(x=annee, y=varPA,color="Réorientation", group=`Profil synthétique candidat`,color=`Profil synthétique candidat`),size=1.7) +
  geom_line(data=plot_ns, aes(x=annee, y=varPA,color="Non scolarisés", group=`Profil synthétique candidat`,color=`Profil synthétique candidat`),size=1.7) +
  ylab("Pourcentage de candidats obtenant au moins une PA")+
  scale_y_continuous(labels = percent_format())+
  labs(colour="")

plot + scale_color_manual(values=c("#d83034", "#0b81a2")) 


#Plot 2: line plot evolution du taux de succès des réos/non scolarisés
cbbPalette <- c("#d83034","#0b81a2","#ededed","#98c127","#ffb255")

h <- ggplot(data=data, aes(x=`Profil synthétique candidat`,
                               y=`Nb de candidats ayant fait au moins un vœu confirmé PP`,
                               group=annee,
                               position = "stacked",
                               fill=factor(`Niveau d'études`)))+
  geom_bar(colour="white",
           stat="identity",
           position=position_dodge())+
  scale_fill_manual(values=cbbPalette, name="Niveau d'études")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(breaks =c(0,1000,2000,3000,4000,5000,6000)) +
  xlab("Niveau d'étude atteint par le candidat") +
  ggtitle("Profil des candidats en réorientation au sein de l'académie de Paris")+
  labs(colour="")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=3, byrow=TRUE))



h<-h +  theme(axis.text.x = element_text(angle = 0, hjust = 1))
h

#plot 3: piechart répartition des profils année 2025
data2025_plot <- ddply(data2025, .(`Niveau d'études`), numcolwise(sum))
data2025_plot$pourcent <-data2025_plot$`Nb de candidats ayant fait au moins un vœu confirmé PP`/sum(data2025_plot$`Nb de candidats ayant fait au moins un vœu confirmé PP`)*100


ggplot(data=data2025_plot, aes(x = , y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
