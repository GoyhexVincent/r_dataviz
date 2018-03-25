#GOYHEX Vincent, vgoyhex@gmail.com
#- RP1968-2008 :pyramide des âges par sexe des entrants, des sortants et des immobiles [par rapport au recensement précédent]
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
detach("package:plyr", unload=TRUE)


# pour établir la connexion avec le serveur
con <- src_postgres("postgresuser", "localhost", "5432", "postgresuser", "postgres")
rp19682008 <- tbl(con, from = "rp19682008")

# ETAPE 1: REQUETAGE DE LA BDD / Mise en forme de l'information.
#3 tables à créer: une pour les entrants, une pour les sortants, une pour les immobiles.

glimpse <- rp19682008 %>%glimpse() #Verification de la structure de la table.

#Immobiles
requete_immobiles <- rp19682008 %>%
  select(an_recens,reg_res_10,pond,sexe,age_rev)%>%
  filter(reg_res_10 == '93') %>%          #On ne conserve que la région PACA
  filter(reg_ran_10 =='93')%>% #On ne veut que les immobiles.
  collect %>%
  mutate(age_cat = cut (as.numeric(age_rev), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),include.lowest = TRUE, right=FALSE, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99")))%>%
  mutate(sexe=replace(sexe, sexe==1, "H")) %>%
  mutate(sexe=replace(sexe, sexe==2, "F")) %>%
  group_by(an_recens,sexe,age_cat)%>%
  summarize(count_id = sum(pond))# On récupère le nombre d'individus qui rentre dans chaque catégorie.
#Entrants.
requete_entrants <- rp19682008 %>%
  select(an_recens,reg_res_10,pond,sexe,age_rev)%>%
  filter(reg_res_10 == '93') %>%          #On ne conserve que la région PACA
  filter(reg_ran_10 !='93')%>% #On ne veut que les immobiles.
  collect %>%
  mutate(age_cat = cut (as.numeric(age_rev), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),include.lowest = TRUE, right=FALSE, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99")))%>%
  mutate(sexe=replace(sexe, sexe==1, "H")) %>%
  mutate(sexe=replace(sexe, sexe==2, "F")) %>%
  group_by(an_recens,sexe,age_cat)%>%
  summarize(count_id = sum(pond))# On récupère le nombre d'individus qui rentre dans chaque catégorie.
#Sortants
requete_sortants <- rp19682008 %>%
  select(an_recens,reg_res_10,pond,sexe,age_rev)%>%
  filter(reg_res_10 != '93') %>%          #On ne conserve que la région PACA
  filter(reg_ran_10 =='93')%>% #On ne veut que les immobiles.
  collect %>%
  mutate(age_cat = cut (as.numeric(age_rev), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),include.lowest = TRUE, right=FALSE, labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99")))%>%
  mutate(sexe=replace(sexe, sexe==1, "H")) %>%
  mutate(sexe=replace(sexe, sexe==2, "F")) %>%
  group_by(an_recens,sexe,age_cat)%>%
  summarize(count_id = sum(pond))# On récupère le nombre d'individus qui rentre dans chaque catégorie.

#Il ne reste plus qu'à aggréger ces trois tables en une seule, après avoir rajouté une colonne qui permettra de distinguer qui entrant, sortant ou immobile.
requete_entrants$type<-"entrants"
requete_sortants$type<-"sortants"
requete_immobiles$type<-"immobiles"
test_requete<- rbind(requete_immobiles,requete_sortants,requete_entrants)
#Je rédonne un order_by cohérent à la table finale, pour m'éviter des mauvaises surprises lors de la réalisation de la pyramide.
test_requete<-test_requete[order(test_requete$an_recens,test_requete$sexe,test_requete$age_cat,test_requete$type),]

test_h<-subset(test_requete,sexe=="H")
test_f<-subset(test_requete,sexe=="F")


# ETAPE 2: Creation du graphique.
cbbPalette <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Pour la pyramide d'age, les etapes sont un peu speciales: je crée d'abord un histogramme "hommes",
# j'ajoute ensuite les femmes en valeurs negatives, via un histogramme "mirroir".
#J'inverse ensuite les axes de l'histogramme (pour obtenir cette forme caractéristique des pyramides d'ages)
# Je demande enfin un facet_wrap pour désintegrer les données et les redistribuer selon an_recens.
ggplot(test_requete, aes(x=age_cat,
                         y=-count_id,
                         fill=type)) + 
  geom_bar(data = subset(test_requete, sexe == "F"),
           aes(y=count_id, fill = type), stat = "identity")+
  scale_fill_manual(values=cbbPalette, name="Type de mobilité")+
  scale_y_continuous(breaks = pretty_breaks(n = 10),labels=comma)+
  geom_hline(xintercept = 0,colour = "grey90", size=2)+
  theme_bw()


last_plot() + geom_bar(data = subset(test_requete, sexe == "H"), 
                       aes(y=-count_id, fill = type), stat = 'identity')+
  scale_fill_manual(values=cbbPalette, name="Type de mobilité")+
  scale_y_continuous(breaks = pretty_breaks(n = 10), labels=comma())
  theme_bw()


last_plot()+coord_flip()+facet_wrap(~an_recens) + theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("Hommes  -  Femmes")+
  ggtitle("Evolution des mobilités en région PACA par rapport au recensement précedent")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle=60, hjust=1))



