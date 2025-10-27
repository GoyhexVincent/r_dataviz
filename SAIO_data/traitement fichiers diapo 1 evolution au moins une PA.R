library (ggplot2)
library (dplyr)
library(plyr)
library (scales)
library(gridExtra)
library(gtable)
library(grid)



#setup env de travail
rm(list=ls())
#setwd("D:\\Users\\vgoyhex\\Documents\\data_vgoyhex\\2026_CIO")
setwd("~/Documents/SAIO/SAIO2026/2026_CIO")

#chargement des données
data2025 <- read.csv2("evolution_candidatures_admissions2025.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2025$annee <- "2025"
data2024 <- read.csv2("evolution_candidatures_admissions2024.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2024$annee <- "2024"
data2023<- read.csv2("evolution_candidatures_admissions2023.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2023$annee <- "2023"
data2022<- read.csv2("evolution_candidatures_admissions2022.csv", header=T, sep=";",check.names=FALSE, encoding="UTF-8")
data2022$annee <- "2022"
data <- rbind(data2025, data2024, data2023, data2022)
data[is.na(data)] <- 0
head(data)
colnames(data)
unique(data$`Profil synthétique candidat`)
unique(data$annee)

#ETAPE 2:
#recenser les lycées publics par CIO pour recréer le secteur
CIO_ouest <- c("Lycée autogéré","Lycée Buffon","Lycée Camille See","Lycée Claude Bernard","Lycée Fresnel","Lycée Janson De Sailly","Lycée Jean De La Fontaine","Lycée Jean-Baptiste Say","Lycée Léonard de Vinci","Lycée Louis Armand","Lycée Moliere","Lycée professionnel Beaugrenelle","Lycée professionnel Claude Anthime Corbon","Lycée professionnel Gustave Eiffel","Lycée professionnel Octave Feuillet","Lycée professionnel Rene Cassin","Lycée Roger Verlomme","Lycée Victor Duruy")
CIO_nord <-c("Lycée Auguste Renoir","Lycée Carnot","Lycée Chaptal","Lycée Condorcet","Lycée Des métiers Jean DROUANT","Lycée Edgar Quinet","Lycée François Rabelais","Lycée Honoré De Balzac","Lycée Jacques Decour","Lycée Jules Ferry","Lycée Lamartine","Lycée professionnel Camille Jenatzy","Lycée professionnel Edmond Rostand","Lycée professionnel Hôtelier Belliard","Lycée professionnel Maria Deraismes","Lycée RACINE")
CIO_est1 <-c("ESAA Boulle","Lycée Arago","Lycée Charlemagne","Lycée Colbert","Lycée Elisa Lemonnier","Lycée Jules SIEGFRIED","Lycée Paul Valery","Lycée professionnel Chenneviere Malezieux","Lycée professionnel Gustave Ferrie","Lycée professionnel Marie Laurencin","Lycée professionnel Metiers De L'Ameublement","Lycée professionnel Pierre Lescot","Lycée professionnel Régional Abbe Gregoire","Lycée professionnel Theophile Gautier","Lycée Simone Weil","Lycée Sophie Germain","Lycée Turgot","Lycée Victor Hugo")
CIO_est2 <-c("Lycée Hélène Boucher","Lycée Martin-Nadaud","Lycée Maurice Ravel","Lycée professionnel Etienne Dolet","Lycée professionnel Marcel Deprez","Lycée professionnel Turquetil","Lycée Voltaire","Lycée des Métiers Dorian","Lycée Paul Poiret")
CIO_est3 <-c("Lycée Batiment-Saint-Lambert","Lycée D'Alembert-Cuir","Lycée Diderot","Lycée Henri Bergson-Jacquard","Lycée professionnel Hector Guimard")
CIO_sud <-c("EREA CROCE SPINELLI","Lycée Claude Monet","Lycée des Métiers de l'Hôtellerie Guillaume TIREL","Lycée Emile Dubois","Lycée Fénelon","Lycée Francois Villon","Lycée Gabriel Faure","Lycée Henri IV","Lycée Jacques Monod","Lycée Jean Lurcat","Lycée Lavoisier","Lycée Louis Le Grand","Lycée Lucas De Nehou","Lycée Maximilien Vox","Lycée Montaigne","Lycée Paul Bert","Lycée Pierre-Gilles de Gennes - ENCPB","Lycée professionnel Corvisart-Arts Graphiques","Lycée professionnel Erik Satie","Lycée professionnel Galilee","Lycée professionnel Gaston Bachelard","Lycée professionnel Nicolas Louis Vauquelin","Lycée Raspail","Lycée Rodin")
#data_sub<-subset(data, Etablissement %in% CIO_ouest)
data_sub<-data
#data_sub$`Profil synthétique candidat` <- ifelse(data_sub$`Profil synthétique candidat` == " Lycéens de Terminale","Lycéens de terminale","Réorientation")

data_plot <- ddply(data_sub, .(annee,`Profil synthétique candidat`,`Académie de rattachement`,Sexe), numcolwise(sum))
data_plot[is.na(data_plot)] <- 0

data_h <- subset(data_plot, Sexe=='M')
data_f <- subset (data_plot, Sexe=='F')



dodge <- position_dodge(width = 0.9)

#ggplot(data_plot, aes(x=annee, y=voeux)) + 
# geom_bar(stat = "identity", width=0.5)

cbbPalette <- c("#0d0d0d","#d83034","#0b81a2","#ffb255","#98c127")

h <- ggplot(data=data_h, aes(x=annee,
                             y=`Nb de cddt ayant au moins une proposition Parcoursup`,
                             group=Sexe,
                             fill=`Profil synthétique candidat`,
                             position = "stacked"
))+
  geom_bar(colour="white",
           stat="identity",
           #position=position_dodge(),
           vjust = -0.25,)+
  scale_fill_manual(values=cbbPalette, name="Profil du candidat")+
  scale_y_continuous(limits = c(0, 42000))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Année") +
  ggtitle("Hommes")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

h<-h +
  facet_grid(. ~ `Académie de rattachement`)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

f <- ggplot(data=data_f, aes(x=annee,
                             y=`Nb de cddt ayant au moins une proposition Parcoursup`,
                             group=Sexe,
                             fill=`Profil synthétique candidat`,
                             position = "stacked"
))+
  geom_bar(colour="white",
           stat="identity",
           #position=position_dodge(),
           vjust = -0.25,)+
  scale_fill_manual(values=cbbPalette, name="Profil du candidat")+
  scale_y_continuous(limits = c(0, 42000))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Année") +
  ggtitle("Femmes")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

f<-f +
  facet_grid(. ~ `Académie de rattachement`)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(h)

#-----------------------------------------------------------

grid.arrange(arrangeGrob(h + theme(legend.position="none"),
                         f + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1),
             top=c("Candidats ayant reçu au moins une proposition d'admission sur Parcoursup en IDF",fontzise=33),
             bottom= textGrob("source de données, Business Object, années 2022-2025",x=1, hjust=1, vjust=0,gp=gpar(fontsize=7)))
