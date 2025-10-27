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
data_CIO <- data[grep("CIO", data$CIO), ]
data_plot <- ddply(data_CIO, .(annee,`CIO`,`Type de la classe`), numcolwise(sum))
data_plot$varPA=(data_plot$`Nb de cddt ayant au moins une proposition Parcoursup`/data_plot$`Nb de candidats ayant fait au moins un vœu confirmé PP`)


data_g <- subset(data_plot, `Type de la classe`=='Générale')
data_t <- subset (data_plot, `Type de la classe`=='Technologique')
data_p <- subset (data_plot, `Type de la classe`=='Professionnelle')


data_expe <- subset(data_CIO,`Au moins 1 avis favorable en STS (Oui/Non)`=="Oui")
data_expe <- ddply(data_expe, .(annee,`Type de la classe`), numcolwise(sum))
data_expe3 <- subset(data_CIO,`Au moins 1 avis favorable en STS (Oui/Non)`=="Non")
data_expe3 <- ddply(data_expe3, .(annee,`Type de la classe`), numcolwise(sum))


data_expe <- subset (data_expe, `Type de la classe`=='Professionnelle')
data_expe3 <- subset (data_expe3, `Type de la classe`=='Professionnelle')
data_expe$varPA=(data_expe$`Nb de cddt ayant au moins une proposition Parcoursup`/data_expe$`Nb de candidats ayant fait au moins un vœu confirmé PP`)
data_expe3$varPA=(data_expe3$`Nb de cddt ayant au moins une proposition Parcoursup`/data_expe3$`Nb de candidats ayant fait au moins un vœu confirmé PP`)


data_plot <- ddply(data_CIO, .(annee,`CIO`,`Type de la classe`), numcolwise(sum))
data_plot$varPA=(data_plot$`Nb de cddt ayant au moins une proposition Parcoursup`/data_plot$`Nb de candidats ayant fait au moins un vœu confirmé PP`)




#data_CIOnord <- subset(data_plot2, CIO=='CIO Nord')


dodge <- position_dodge(width = 0.9)

#ggplot(data_plot, aes(x=annee, y=voeux)) + 
# geom_bar(stat = "identity", width=0.5)

cbbPalette <- c("#d83034","#0b81a2","#ffb255","#98c127")
cbbPalette2 <- c(brewer.pal(n = 6, name = 'Dark2'))


plot <- ggplot(data=data_plot, aes(x=annee,
                             y=`Nb de candidats ayant fait au moins un vœu confirmé PP`,
                             fill=`Type de la classe`,
                             position = "stacked"
))+
  geom_bar(colour="white",
           stat="identity",
           #position=position_dodge(),
           vjust = -0.25,)+
  scale_fill_manual(values=cbbPalette, name="Profil du candidat")+
  #scale_y_continuous(limits = c(0, 2000))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Année") +
  ggtitle("Hommes")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

plot<-plot +
  facet_grid(. ~ `CIO`)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(plot)

#-----------------------------------------------------------

grid.arrange(arrangeGrob(plot + theme(legend.position="none"),
                         
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1),
             top=c("Répartition des candidats de Terminale par CIO",fontzise=33),
             bottom= textGrob("Lycées publics, académie de Paris, source de données, Business Object, années 2022-2025",x=1, hjust=1, vjust=0,gp=gpar(fontsize=7)))






g<- ggplot(data=data_g, aes(x=annee, y=varPA, group=CIO, color=CIO)) +
  geom_line(size=1.2,) +
  scale_fill_manual(values=cbbPalette2, name="CIO")+
  ggtitle("Obtention d'une PA sur parcoursup") +
  ylab("Candidats obtenant au moins une PA")+
  scale_y_continuous(labels = percent_format())+
  scale_fill_continuous(palette=cbbPalette2,guide = guide_legend()) +
  theme(legend.position="bottom", legend.direction="horizontal")

t<- ggplot(data=data_t, aes(x=annee, y=varPA, group=CIO, color=CIO)) +
  geom_line(size=1.2,) +
  ggtitle("Obtention d'une PA sur parcoursup") +
  ylab("Candidats obtenant au moins une PA")+
  scale_y_continuous(labels = percent_format())

p<- ggplot(data=data_p, aes(x=annee, y=varPA, group=CIO, color=CIO)) +
  geom_line(size=1.2,) +
  ggtitle("Obtention d'une PA sur parcoursup") +
  ylab("Candidats obtenant au moins une PA")+
  scale_y_continuous(labels = percent_format())

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend2<-g_legend(g)

grid.arrange(arrangeGrob(g + theme(legend.position="none"),
                        p + theme(legend.position="none"),
                        t + theme(legend.position="none"),
                         nrow=1),
             mylegend2, nrow=2,heights=c(5, 1),
             top=c("Part des candidats ayant reçu au moins une proposition d'admission",fontzise=33),
             bottom= textGrob("source de données, Business Object, années 2022-2025",x=1, hjust=1, vjust=0,gp=gpar(fontsize=7)))

#######################################################################################


expe <- ggplot()+
  geom_line(data=data_expe, aes( x=annee, y=varPA, group=`Type de la classe`,colour="Bacheliers Pro avec Avis STS Positif"),size=1.7) +
  geom_line(data=data_expe3, aes( x=annee, y=varPA, group=`Type de la classe`,colour="Bacheliers Pro sans Avis STS Positif"),size=1.7) +
  ylab("Candidats obtenant au moins une PA")+
  scale_y_continuous(labels = percent_format())+
  theme(legend.position="bottom", legend.direction="horizontal")

mylegend3<-g_legend(expe)


grid.arrange(arrangeGrob(expe + theme(legend.position="none"),
                         nrow=1),
             mylegend3, nrow=2,heights=c(5, 1),
             top=c("Réussite des candidats ayant obtenu un avis favorable à l'experimentation STS",fontzise=33),
             bottom= textGrob("Académie de Paris, source de données, Business Object, années 2022-2025",x=1, hjust=1, vjust=0,gp=gpar(fontsize=7)))


