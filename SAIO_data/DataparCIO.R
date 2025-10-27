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
data2025 <- read.csv2("candidats_terminale_public2025.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2025$annee <- "2025"
data2024 <- read.csv2("candidats_terminale_public2024.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2024$annee <- "2024"
data2023<- read.csv2("candidats_terminale_public2023.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2023$annee <- "2023"
data2022<- read.csv2("candidats_terminale_public2022.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2022$annee <- "2022"
data <- rbind(data2025, data2024, data2023, data2022)
data[is.na(data)] <- 0
head(data)
colnames(data)
unique(data$`Cordées de la réussite`)
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

data<- subset(data,`Type de la classe` !="Brevet")
data<- subset(data,`Type de la classe` !="Autres Diplômes et brevets de niveau 4")
data <- subset( data, select = -`Au moins 1 avis favorable en STS (Oui/Non)` )

data_CIO <- data[grep("CIO", data$CIO), ]
data_CIO<-subset(data, Etablissement %in% CIO_sud)
data_CIO<-subset(data_CIO, annee=="2025")
data_CIO <- ddply(data_CIO, .(`Type de la classe`, Etablissement), numcolwise(sum))


data_CIO$varPA=(data_CIO$`Nb de cddt ayant au moins une proposition Parcoursup`/data_CIO$`Nb de candidats ayant fait au moins un vœu confirmé PP`)


dodge <- position_dodge(width = 0.9)

#ggplot(data_plot, aes(x=annee, y=voeux)) + 
# geom_bar(stat = "identity", width=0.5)

cbbPalette <- c("#d83034","#0b81a2","#ffb255","#98c127")

h <- ggplot(data=data_CIO, aes(x=`Etablissement`,
                             y=`varPA`,
                             group=`Type de la classe`,
                             fill=factor(`Type de la classe`)))+
  geom_bar(colour="white",
           stat="identity",
           position=position_dodge())+
  scale_fill_manual(values=cbbPalette, name="Type de la classe")+
  scale_y_continuous(labels = percent_format())+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Etablissement") +
  ggtitle("Taux de réussite des candidats de terminale, CIO Sud")+
  labs(colour="")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")+
  geom_text(aes(label=round(varPA,digits=2)*100), position=position_dodge(width=0.9), vjust=-0.25)



h<-h +  theme(axis.text.x = element_text(angle = 90, hjust = 1))


h



