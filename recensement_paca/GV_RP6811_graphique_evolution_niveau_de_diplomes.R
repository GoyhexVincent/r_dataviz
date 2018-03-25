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
rp6811 <- tbl(con, from = "rp682011ok")

#-----------------------------------------------------------
# ETAPE 1: REQUETAGE DE LA BDD / Mise en forme de l'information.
test_requete <- rp6811 %>%
  select(an_recens,age_rev,sexe,dipl,pond)%>%
  filter(reg_res_13 == '93') %>% #On ne conserve que la région PACA
  filter(dipl !='*') %>% # On supprime les personnes trop jeunes pour prétendre à un diplome (- de 14 ans)
  collect %>%
  mutate(age_cat = cut (as.numeric(age_rev), breaks = c(0,25,45,65,100),include.lowest = TRUE, right = FALSE, labels = c("Moins de 25 ans","25-44ans","45-64ans","65ans et plus")))%>%
  mutate(dipl_cat = cut(as.numeric(dipl),breaks = c(0,1,4,5,8), include.lowest = TRUE, right = FALSE, labels =c("Aucun diplome","Inférieur BAC","BAC","Diplome universitaire")))%>%
  na.omit() %>% # Suppression des valeurs NA, correspondant aux gens ayant moins de 25ans (ou plus de 100 ans)
  mutate(sexe=replace(sexe, sexe==1, "H")) %>%
  mutate(sexe=replace(sexe, sexe==2, "F")) %>%
  group_by(sexe, an_recens, dipl_cat,age_cat)%>% # quadruple group_by
  summarize(count_id = sum(pond))# On récupère le nombre d'individus qui rentre dans chaque catégorie.

#-----------------------------------------------------
#Variable "percent_year_age": % du total de chaque recensement.
test_h <- subset(test_requete, sexe =='H')
test_f <- subset(test_requete, sexe =='F')

data <- tapply(test_h$count_id, list(test_h$an_recens,test_h$age_cat), FUN=sum)
test_proportions <- melt((data), id=c())
colnames(test_proportions) <- c("an_recens","age_cat","count_total")
test_h <- merge(x = test_h, y = test_proportions, by = c( "age_cat","an_recens"), all = TRUE)
test_h$percent_year_age <- test_h$count_id/as.numeric(test_h$count_total)
#--
data <- tapply(test_f$count_id, list(test_f$an_recens,test_f$age_cat), FUN=sum)
test_proportions <- melt((data), id=c())
colnames(test_proportions) <- c("an_recens","age_cat","count_total")
test_f <- merge(x = test_f, y = test_proportions, by = c( "age_cat","an_recens"), all = TRUE)
test_f$percent_year_age <- test_f$count_id/as.numeric(test_f$count_total)
#-----------------------------------------------------
  



#ETAPE 2: GGPLOT2.  
#-----------------------------------------------------------  
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(scales)
h<- ggplot(data=test_h, aes(x=age_cat,
                                y=percent_year_age,
                                group=sexe,
                                fill=dipl_cat,
                              position = "stacked"
)) +
  geom_text(aes(label=sexe), position=position_dodge(width=1), vjust=-0.20, size = 2.5)+
  geom_bar(colour="white",
           stat="identity",
           #position=position_dodge(),
           vjust = -0.25,)+
  scale_fill_manual(values=cbbPalette, name="Niveau de diplome")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Hommes")+
  guides(fill = guide_legend(reverse = TRUE))+
  geom_histogram(binwidth = 0.2)+
  theme(legend.position = "bottom")

h<-h +
  facet_grid(. ~ an_recens)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

f<- ggplot(data=test_f, aes(x=age_cat,
                            y=percent_year_age,
                            group=sexe,
                            fill=dipl_cat,
)) +
  geom_text(aes(label=sexe), position=position_dodge(width=1), vjust=-0.20, size = 2.5)+
  geom_bar(colour="white",
           stat="identity",
           #position=position_dodge(),
           vjust = -0.25,)+
  scale_fill_manual(values=cbbPalette, name="Niveau de diplome")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Femmes")+
  guides(fill = guide_legend(reverse = TRUE))+
  geom_histogram(binwidth = 0.2)+
  theme(legend.position = "bottom")

f<-f +
  facet_grid(. ~ an_recens)+
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
#grid.arrange(h, f, ncol=2, top="Evolution du niveau de diplome au sein de la région PACA",)
#-----------------------------------------------------------

grid.arrange(arrangeGrob(h + theme(legend.position="none"),
                               f + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1),top=c("Evolution du niveau de diplome au sein de la région PACA",fontzise=20))




