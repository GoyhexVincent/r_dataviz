#RP1968-2008 :Evolution du secteur d'activité des ouvriers et employés.
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
detach("package:plyr", unload=TRUE)


# pour établir la connexion avec le serveur
con <- src_postgres("postgresuser", "localhost", "5432", "postgresuser", "postgres")
rp19682008 <- tbl(con, from = "rp19682008")


# ETAPE 1: REQUETAGE DE LA BDD / Mise en forme de l'information.
glimpse <- rp19682008 %>%glimpse() #Verification de la structure de la table.


target <- c("5","6" )
test_requete <- rp19682008 %>%
  select(csp,nes3,an_recens,pond)%>%
  filter(reg_res_10 == '93') %>% #On ne conserve que la région PACA
  filter(csp %in% target)%>% #C'est plus rapide pour filtrer selon deux critères potentiels.
  filter(nes3 != "9") %>%
  collect %>%
  mutate(csp=replace(csp, csp=="5", " Employés")) %>%
  mutate(csp=replace(csp, csp=="6", " Ouvriers")) %>%
  mutate(nes3=replace(nes3, nes3=="1", "Agriculture")) %>%
  mutate(nes3=replace(nes3, nes3=="2", "Industrie-BTP")) %>%
  mutate(nes3=replace(nes3, nes3=="3", "Tertiaire")) %>%
  group_by(csp,nes3,an_recens)%>%
  summarize(count_id = sum(pond))# On récupère le nombre d'individus qui rentre dans chaque catégorie.

# ETAPE 3: GGPLOT2.
#Comme toujours, un peu de traitement en plus pour passer les données en %, c'est plus joli à visualiser.
data <- tapply(test_requete$count_id, list(test_requete$an_recens,test_requete$csp), FUN=sum)
test_proportions <- melt((data), id=c())
colnames(test_proportions) <- c("an_recens","csp","count_total")
test_requete <- merge(x = test_requete, y = test_proportions, by = c( "an_recens","csp"), all = TRUE)
test_requete$percent <- test_requete$count_id/test_requete$count_total
test_requete<- test_requete[order(test_requete$an_recens,test_requete$nes3,test_requete$csp),]


cbbPalette <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette_hf<-c("#CC0000","#0099FF")
library(scales)
plot1 <- ggplot(data=test_requete, aes(x=csp,
                                                  y=percent,
                                                  fill=nes3,
)) +
  geom_bar(
    color = "white",
    stat="identity"#,
#    vjust = -0.25,
)+
  scale_fill_manual(values=cbbPalette, name="test")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab("Type d'emploi") +
  ylab("En % des employés et ouvriers actifs") +
#  ggtitle("Evolution des secteurs d'activité des employés et ouvriers en région PACA")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~an_recens)
