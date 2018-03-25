# - RP2011Reg : Graphique : répartition des ouvriers et employés par âge (idem), sexe et commune urbaine/rurale
#GOYHEX Vincent, vgoyhex@gmail.com

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
  select(sexe,aged,cs1,ipondi,ur)%>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(aged >'015')%>%
  filter(cs1 %in% target)%>% #C'est plus rapide pour filtrer selon deux critères potentiels.
  collect %>%
  mutate(age_cat = cut (as.numeric(aged), breaks = c(0,25,45,65,120),include.lowest = TRUE, right = FALSE, labels = c("16-25ans","25-44ans","45-65ans","65ans et plus")))%>%
  mutate(sexe=replace(sexe, sexe=="1", "H")) %>%
  mutate(sexe=replace(sexe, sexe=="2", "F")) %>%
  mutate(cs1=replace(cs1, cs1=="5", " Employés")) %>%
  mutate(cs1=replace(cs1, cs1=="6", " Ouvriers")) %>%
  mutate(ur=replace(ur,ur=="1","Commune urbaine"))%>%
  mutate(ur=replace(ur,ur=="0","Commune rurale"))%>%
  group_by(sexe, cs1,age_cat,ur)%>%
  summarize(count_id = sum(ipondi))# On récupère le nombre d'individus qui rentre dans chaque catégorie.
#-----------------------------------------------------
cbbPalette <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette_hf<-c("#CC0000","#0099FF")

#PLOT PRINCIPAL:
#Quelques modifications aux données avant de proceder:
test_requete<- test_requete[order(test_requete$sexe,test_requete$age_cat,test_requete$ur),]
test_h<-subset(test_requete,sexe=='H')
test_f<-subset(test_requete,sexe=='F')
#Pour les femmes:
data <- tapply(test_f$count_id,list(test_f$age_cat,test_f$cs1), FUN=sum)
test_proportions <- melt((data), id=c())
colnames(test_proportions) <- c("age_cat","cs1","count_total")
test_f <- merge(x = test_f, y = test_proportions, by = c( "age_cat","cs1"), all = TRUE)
#Pour les hommes:
data <- tapply(test_h$count_id,list(test_h$age_cat,test_h$cs1), FUN=sum)
test_proportions <- melt((data), id=c())
colnames(test_proportions) <- c("age_cat","cs1","count_total")
test_h <- merge(x = test_h, y = test_proportions, by = c( "age_cat","cs1"), all = TRUE)

test_h$count_id_percent<-test_h$count_id/as.numeric(test_h$count_total)
test_f$count_id_percent<-test_f$count_id/as.numeric(test_f$count_total)
test_requete<- rbind(test_h,test_f)


plot_rural<- ggplot(data=subset(test_requete, ur=="Commune rurale"), aes(x=age_cat,
                                           y=count_id_percent,
                                           group=sexe,
                                           fill=sexe,
)) +
  geom_bar(
    stat="identity",
    color="white",
    position="dodge"
#    vjust = -0.25,
)+
  scale_y_continuous(breaks = pretty_breaks(n = 8),labels = percent_format())+
  scale_fill_manual(values= palette_hf, name="Type:")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Age") +
  ylab("population") +
  ggtitle("Répartition des ouvriers et employés au sein des communes rurales")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")+
  facet_wrap(~cs1)

