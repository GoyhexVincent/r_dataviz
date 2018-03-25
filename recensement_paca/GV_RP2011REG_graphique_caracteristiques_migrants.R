#GOYHEX Vincent, vgoyhex@gmail.com
#Part des immobiles, des migrants intra-regionaux et des migrants extraregionaux par age, par sexe et par CSP.
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
# pour établir la connexion avec le serveur
con <- src_postgres("postgresuser", "localhost", "5432", "postgresuser", "postgres")
rp2011reg <- tbl(con, from = "fd_indreg_2011")

# ETAPE 1: REQUETAGE DE LA BDD / Mise en forme de l'information.

test_requete <- rp2011reg %>%
  select(sexe,aged,cs1,iran,ipondi)%>%
  filter(region == '93') %>%          #On ne conserve que la région PACA
  filter(iran !='Z') %>%              #On supprime les ind nés durant l'année de collecte.
  collect %>%
  arrange(iran)%>%
  mutate(iran_cat = cut (as.numeric(iran), breaks = c(0,2,5,10),include.lowest = TRUE, right=FALSE, labels = c("Immobile","Migrant intra-régional","Migrant extra-regional")))%>%
  mutate(age_cat = cut (as.numeric(aged), breaks = c(0,25,45,65,120),include.lowest = TRUE, right = FALSE, labels = c("Moins de 25 ans","25-44ans","45-64ans","65ans et plus")))%>%
  mutate(sexe=replace(sexe, sexe==1, "H")) %>%
  mutate(sexe=replace(sexe, sexe==2, "F")) %>%
  mutate(cs1=replace(cs1, cs1==1, "Agr.")) %>%
  mutate(cs1=replace(cs1, cs1==2, " ACCE*")) %>%
  mutate(cs1=replace(cs1, cs1==3, " CPIS*")) %>%
  mutate(cs1=replace(cs1, cs1==4, " Prof.Inter")) %>%
  mutate(cs1=replace(cs1, cs1==5, " Employés")) %>%
  mutate(cs1=replace(cs1, cs1==6, " Ouvriers")) %>%
  mutate(cs1=replace(cs1, cs1==7, " Retraités ")) %>%
  mutate(cs1=replace(cs1, cs1==8, " Inactifs")) %>%
  group_by(sexe, cs1, iran_cat,age_cat)%>% # quadruple group_by
  summarize(count_id = sum(ipondi))# On récupère le nombre d'individus qui rentre dans chaque catégorie.

#-----------------------------------------------------
#Variable "percent_year_age" pour harmoniser la "forme" du graphique en ramenant chaque tranche d'age en pourcent.
#Division de la table en deux sous tables, une pour les hommes, une pour les femmes.
test_h <- subset(test_requete, sexe =='H')
test_f <- subset(test_requete, sexe =='F')

data <- tapply(test_h$count_id, list(test_h$cs1,test_h$age_cat), FUN=sum)
test_proportions <- melt((data), id=c())
test_proportions <- na.omit(test_proportions) #Retraités + moins de 25ans générait un NA.
colnames(test_proportions) <- c("cs1","age_cat","count_total")
test_h <- merge(x = test_h, y = test_proportions, by = c( "age_cat","cs1"), all = TRUE)
test_h$percent_year_age <- test_h$count_id/as.numeric(test_h$count_total)
#--
data <- tapply(test_f$count_id, list(test_f$cs1,test_f$age_cat), FUN=sum)
test_proportions <- melt((data), id=c())
test_proportions <- na.omit(test_proportions) #Retraités + moins de 25ans générait un NA.
colnames(test_proportions) <- c("cs1","age_cat","count_total")
test_f <- merge(x = test_f, y = test_proportions, by = c( "age_cat","cs1"), all = TRUE)
test_f$percent_year_age <- test_f$count_id/as.numeric(test_f$count_total)

#Un problème en aval nécéssite ici de placer un order, pour s'assurer que l'information soit représentée de manière cohérente dans le bon ordre.
test_f<- test_f[order(test_f$cs1,test_f$age_cat,test_f$iran_cat),]
test_h<- test_h[order(test_h$cs1,test_h$age_cat,test_h$iran_cat),]
# ETAPE 2: GGPLOT2.

cbbPalette <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(scales)
h<- ggplot(data=test_h, aes(x=age_cat,
                            y=percent_year_age,
                            group=sexe,
                            fill=iran_cat,
                            position = "stacked"
)) +
  geom_bar(colour="white",
           stat="identity",
           #position=position_dodge(),
           vjust = -0.25,)+
  scale_fill_manual(values=cbbPalette, name="Mobilité")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Hommes")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

h<-h +
  facet_grid(. ~ cs1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

f<- ggplot(data=test_f, aes(x=age_cat,
                            y=percent_year_age,
                            group=sexe,
                            fill=iran_cat,
)) +
  geom_bar(colour="white",
           stat="identity",
           #position=position_dodge(),
           vjust = -0.25,)+
  scale_fill_manual(values=cbbPalette, name="Mobilité")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Femmes")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")

f<-f +
  facet_grid(. ~ cs1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Fonction pour récuperer et stocker la légende, permettra ensuite de la réutiliser dans la grid.
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(h)


#-----------------------------------------------------------
#ETAPE 3: GRIDEXTRA


library(gridExtra)
library(gtable)
#grid.arrange(h, f, ncol=2, top="Evolution du niveau de diplome au sein de la région PACA",)
#-----------------------------------------------------------

grid.arrange(arrangeGrob(h + theme(legend.position="none"),
                         f + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1),
             top=c("Mobilités au sein de la région PACA",fontzise=33),
             bottom= textGrob("ACCE*: Artisans, Commercants, Chefs d'entreprises  -  Agr*: Agriculteurs  -  CPIS*: Cadres et Professions intellectuelles supérieures  -  Prof.Inter*: Professions Intermédiaires     ",x=1, hjust=1, vjust=0,gp=gpar(fontsize=9)))



