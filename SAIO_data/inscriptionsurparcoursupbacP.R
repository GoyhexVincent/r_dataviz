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
data2025 <- read.csv2("bacsproevol2025.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2025$annee <- "2025"
data2024 <- read.csv2("bacsproevol2024.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2024$annee <- "2024"
data2023<- read.csv2("bacsproevol2023.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2023$annee <- "2023"
data2022<- read.csv2("bacsproevol2022.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2022$annee <- "2022"
data <- rbind(data2025, data2024, data2023, data2022)
data[is.na(data)] <- 0
head(data)
colnames(data)
unique(data$annee)


#ETAPE 2:
#recenser les lycées publics par CIO pour recréer le secteur
CIO_ouest <- c("Lycée autogéré","Lycée Buffon","Lycée Camille See","Lycée Claude Bernard","Lycée Fresnel","Lycée Janson De Sailly","Lycée Jean De La Fontaine","Lycée Jean-Baptiste Say","Lycée Léonard de Vinci","Lycée Louis Armand","Lycée Moliere","Lycée professionnel Beaugrenelle","Lycée professionnel Claude Anthime Corbon","Lycée professionnel Gustave Eiffel","Lycée professionnel Octave Feuillet","Lycée professionnel Rene Cassin","Lycée Roger Verlomme","Lycée Victor Duruy")
CIO_nord <-c("Lycée Auguste Renoir","Lycée Carnot","Lycée Chaptal","Lycée Condorcet","Lycée Des métiers Jean DROUANT","Lycée Edgar Quinet","Lycée François Rabelais","Lycée Honoré De Balzac","Lycée Jacques Decour","Lycée Jules Ferry","Lycée Lamartine","Lycée professionnel Camille Jenatzy","Lycée professionnel Edmond Rostand","Lycée professionnel Hôtelier Belliard","Lycée professionnel Maria Deraismes","Lycée RACINE")
CIO_est1 <-c("ESAA Boulle","Lycée Arago","Lycée Charlemagne","Lycée Colbert","Lycée Elisa Lemonnier","Lycée Jules SIEGFRIED","Lycée Paul Valery","Lycée professionnel Chenneviere Malezieux","Lycée professionnel Gustave Ferrie","Lycée professionnel Marie Laurencin","Lycée professionnel Metiers De L'Ameublement","Lycée professionnel Pierre Lescot","Lycée professionnel Régional Abbe Gregoire","Lycée professionnel Theophile Gautier","Lycée Simone Weil","Lycée Sophie Germain","Lycée Turgot","Lycée Victor Hugo")
CIO_est2 <-c("Lycée Hélène Boucher","Lycée Martin-Nadaud","Lycée Maurice Ravel","Lycée professionnel Etienne Dolet","Lycée professionnel Marcel Deprez","Lycée professionnel Turquetil","Lycée Voltaire","Lycée des Métiers Dorian","Lycée Paul Poiret")
CIO_est3 <-c("Lycée Batiment-Saint-Lambert","Lycée D'Alembert-Cuir","Lycée Diderot","Lycée Henri Bergson-Jacquard","Lycée professionnel Hector Guimard")
CIO_sud <-c("EREA CROCE SPINELLI","Lycée Claude Monet","Lycée des Métiers de l'Hôtellerie Guillaume TIREL","Lycée Emile Dubois","Lycée Fénelon","Lycée Francois Villon","Lycée Gabriel Faure","Lycée Henri IV","Lycée Jacques Monod","Lycée Jean Lurcat","Lycée Lavoisier","Lycée Louis Le Grand","Lycée Lucas De Nehou","Lycée Maximilien Vox","Lycée Montaigne","Lycée Paul Bert","Lycée Pierre-Gilles de Gennes - ENCPB","Lycée professionnel Corvisart-Arts Graphiques","Lycée professionnel Erik Satie","Lycée professionnel Galilee","Lycée professionnel Gaston Bachelard","Lycée professionnel Nicolas Louis Vauquelin","Lycée Raspail","Lycée Rodin")

data$CIO = data$Etablissement
data<-data %>%   mutate(CIO = ifelse(data$CIO %in% CIO_ouest, "CIO Ouest", CIO))
data<-data %>%   mutate(CIO = ifelse(data$CIO %in% CIO_nord, "CIO Nord", CIO))
data<-data %>%   mutate(CIO = ifelse(data$CIO %in% CIO_est1, "CIO Est 1", CIO))
data<-data %>%   mutate(CIO = ifelse(data$CIO %in% CIO_est2, "CIO Est 2", CIO))
data<-data %>%   mutate(CIO = ifelse(data$CIO %in% CIO_est3, "CIO Est 3", CIO))
data<-data %>%   mutate(CIO = ifelse(data$CIO %in% CIO_sud, "CIO Sud", CIO))


data_CIO <- data[grep("CIO", data$CIO), ]
data_plot <- ddply(data_CIO, .(annee), numcolwise(sum))
data_plot$varinscription=(data_plot$`Nb de candidats ayant fait au moins un vœu confirmé PP`/data_plot$`Nb d'individus`)

dodge <- position_dodge(width = 0.9)

#ggplot(data_plot, aes(x=annee, y=voeux)) + 
# geom_bar(stat = "identity", width=0.5)

cbbPalette <- c("#d83034","#0b81a2","#ffb255","#98c127")
cbbPalette2 <- c(brewer.pal(n = 6, name = 'Dark2'))


ggplot()+
  geom_line(data=data_plot, aes(x=annee, y=varinscription,color="Bacheliers professionnels de terminale", group=1),size=1.7) +
  ylab("Pourcentage de bacheliers pro s'inscrivant sur parcoursup")+
  scale_y_continuous(labels = percent_format())+
  theme(legend.position="bottom", legend.name="test",legend.direction="horizontal")+
  labs(colour="")



