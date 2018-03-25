#RP2011Reg : Graphique : répartition des ouvriers et employés par secteur d'activité, par sexe et par âge
#(pour l'âge utiliser des classes d'âge large, type 16-25, 25-45, 45-65
#vgoyhex@gmail.com


#Empty memory
#-----------------------------------------------------------
rm(list=ls())

library(ggplot2)
library(sp)
library(maptools)
library(rgeos)
library(tmap)
library(dplyr)
library(dplyrr)
library(tidyr)
library(reshape2)
#library(plyr) #Faire très attention avec plyr, même quand on a besoin, si on l'appelle au début, la fonction summarize()de plyr remplacera celle de dplyr et génère des problèmes.
detach("package:plyr", unload=TRUE) #Si une erreur est renvoyée, ce n'est pas un problème.
library(MonetDB.R)

# pour établir la connexion avec le serveur
con <- src_postgres("postgresuser", "localhost", "5432", "postgresuser", "postgres")
rp2011reg <- tbl(con, from = "fd_indreg_2011")

# ETAPE 1: REQUETAGE DE LA BDD / Mise en forme de l'information.
target <- c("5","6" )
test_requete <- rp2011reg %>%
  select(sexe,aged,cs1,ipondi,na38)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(aged >'015')%>%
  filter(cs1 %in% target)%>% #C'est plus rapide pour filtrer selon deux critères potentiels.
  collect %>%
  mutate(age_cat = cut (as.numeric(aged), breaks = c(0,25,45,65,120),include.lowest = TRUE, right = FALSE, labels = c("16-25ans","25-44ans","45-65ans","65ans et plus")))%>%
  mutate(sexe=replace(sexe, sexe=="1", "H")) %>%
  mutate(sexe=replace(sexe, sexe=="2", "F")) %>%
  mutate(cs1=replace(cs1, cs1=="5", " Employés")) %>%
  mutate(cs1=replace(cs1, cs1=="6", " Ouvriers")) %>%
  group_by(sexe, cs1,age_cat,na38)%>%
  summarize(count_id = sum(ipondi))# On récupère le nombre d'individus qui rentre dans chaque catégorie.
#-----------------------------------------------------
#ETAPE 2: Un problème à régler: 38 secteurs d'activité vont se montrer ingérables. Je propose ici de redécouper les secteurs d'activité en 3 classes: primaire/secondaire/tertiaire.
primaire<-c("AZ","BZ")
secondaire<-c("CA","CB","CC","CD","CE","CF","CH","CI","CJ","CK","CL","CM","DZ","EZ","FZ","GZ","HZ","IZ")
tertiaire<-c("JA","JB","JC","KZ","LZ","MA","MB","MC","NZ","OZ","PZ","QA","QB","RZ","SZ","TZ","UZ")
secteur_primaire<-subset(test_requete, na38 %in% primaire)
secteur_secondaire<-subset(test_requete, na38 %in% secondaire)
secteur_tertiaire<-subset(test_requete, na38 %in% tertiaire)
test_requete$secteur_eco<- ifelse (test_requete$na38 %in% primaire, "secteur primaire",
                              ifelse (test_requete$na38 %in% secondaire, "secteur secondaire","secteur tertiaire"))
#la catégorie ZZ a été enlevée, parceque je ne savais pas quoi en faire pour l'instant.

# ETAPE 3: GGPLOT2.
#A partir d'ici, deux solutions: Je peux proposer le graphique habituel: age, sexe, secteur economique d'activité.
# On perd quand meme pas mal d'informations en passant sur du primaire, secondaire, tertiaire. Donc eventuellement, il peut être intéressant de refaire trois graphiques détaillant ces infos.
cbbPalette <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette_hf<-c("#CC0000","#0099FF")
library(scales)
plot_primaire<- ggplot(data=secteur_primaire, aes(x=age_cat,
                            y=count_id,
                            fill=sexe,
                            group=sexe
)) +
  geom_bar(
           stat="identity",
           position=position_dodge()
#           vjust = -0.25,
)+
  scale_fill_manual(values=palette_hf, name="test")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  ggtitle("Secteur économique primaire")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

plot_primaire<-plot_primaire +
  facet_grid(. ~ na38)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# PLOT SECTEUR SECONDAIRE
plot_secondaire<- ggplot(data=secteur_secondaire, aes(x=age_cat,
                                                  y=count_id,
                                                  fill=sexe,
                                                  group=sexe
)) +
  geom_bar(
    stat="identity",
    position=position_dodge()#,
#    vjust = -0.25,
)+
  scale_fill_manual(values=palette_hf, name="test")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  ggtitle("Secteur économique secondaire")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

plot_secondaire<-plot_secondaire +
  facet_grid(. ~ na38)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# PLOT SECTEUR TERTIAIRE
plot_tertiaire<- ggplot(data=secteur_tertiaire, aes(x=age_cat,
                                                      y=count_id,
                                                      fill=sexe,
                                                      group=sexe
)) +
  geom_bar(
    stat="identity",
    position=position_dodge()#,
#    vjust = -0.25,
)+
  scale_fill_manual(values=palette_hf, name="test")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  ggtitle("Secteur économique tertiaire")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

plot_tertiaire<-plot_tertiaire +
  facet_grid(. ~ na38)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Dans l'idéal pour ces trois plots, il aurait fallu intégrer du texte qui détaille les abréviations.
#J'ai essayé rapidement sous ggplot, ce n'est pas très esthétique, je pense que le mieux serait de le rajouter à coté ou en annexe.

#PLOT PRINCIPAL:
#Quelques modifications aux données avant de proceder:
test_requete<- test_requete[order(test_requete$sexe,test_requete$age_cat,test_requete$secteur_eco),]
test_h<-subset(test_requete,sexe=='H')
test_f<-subset(test_requete,sexe=='F')
#Pour les femmes:
data <- tapply(test_f$count_id,test_f$age_cat, FUN=sum)
test_proportions <- melt((data), id=c())
colnames(test_proportions) <- c("age_cat","count_total")
test_f <- merge(x = test_f, y = test_proportions, by = c( "age_cat"), all = TRUE)
#Pour les hommes:
data <- tapply(test_h$count_id,test_h$age_cat, FUN=sum)
test_proportions <- melt((data), id=c())
colnames(test_proportions) <- c("age_cat","count_total")
test_h <- merge(x = test_h, y = test_proportions, by = c( "age_cat"), all = TRUE)

test_h$count_id_percent<-test_h$count_id/as.numeric(test_h$count_total)
test_f$count_id_percent<-test_f$count_id/as.numeric(test_f$count_total)

plot_principal_F<- ggplot(data=test_f, aes(x=age_cat,
                                                     y=count_id_percent,
                                                     fill=secteur_eco,
)) +
  geom_bar(
    stat="identity"#,
#    vjust = -0.25,
)+
  scale_y_continuous(labels = percent_format())+
  scale_fill_manual(values=cbbPalette, name="Secteur économique:")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  ggtitle("Femmes")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

plot_principal_H<- ggplot(data=test_h, aes(x=age_cat,
                                           y=count_id_percent,
                                           fill=secteur_eco,
)) +
  geom_bar(
    stat="identity"#,
#    vjust = -0.25,
)+
  scale_y_continuous(labels = percent_format())+
  scale_fill_manual(values=cbbPalette, name="Secteur économique:")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  ggtitle("Hommes")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

library(gridExtra)
library(gtable)
#grid.arrange(h, f, ncol=2, top="Evolution du niveau de diplome au sein de la région PACA",)
#-----------------------------------------------------------
#Fonction pour récuperer et stocker la légende, permettra ensuite de la réutiliser dans la grid.
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(plot_principal_F)

plot_principal<- grid.arrange(arrangeGrob(
                         plot_principal_H + theme(legend.position="none"),
                         plot_principal_F + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1),
             top=c("répartition des ouvriers et employés par secteur d'activité",fontzise=33))

