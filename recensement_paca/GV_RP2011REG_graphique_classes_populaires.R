# RP2011Reg : Graphique : part des actifs occupés [cest-à-dire ni au chômage ni à la retraite]
#dans les classes populaires ("classes populaires" étant défini comme ouvriers
#et employés actifs et retraités, inactifs dont
#l'un des membres du ménage est ouvrier ou employé actif ou retraité) 

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
# target <- c("5","6" )
# actifs_occupés <- rp2011reg %>%
#   filter(region == '93') %>% #On ne conserve que la région PACA
#   filter(as.integer(aged) > 15) %>%
#   filter(cs1 %in% target) %>%
#   select(cs1,ipondi) %>%
#   collect %>%
#   mutate(cs1=replace(cs1, cs1=="5", " Employés")) %>%
#   mutate(cs1=replace(cs1, cs1=="6", " Ouvriers")) %>%
#   group_by(cs1) %>%
#   summarize(count_id = sum(ipondi))# On récupère le nombre d'individus qui rentre dans chaque catégorie.
# #-----------------------------------------------------
classes_populaires <- rp2011reg %>%
  filter(region == '93') %>% #On ne conserve que la région PACA
  filter(as.integer(aged) > 15) %>%
  filter(!is.na(nummr)) %>%
  group_by(nummr) %>%
  mutate(classespop = cs2  %in% c("51", "54", "55", "56", "61", "66", "69", "76")) %>%
  select(nummr, cs2, classespop, ipondi) %>%
  collect %>%
  mutate(menagepop = any(classespop)) %>%
  ungroup() %>%
  filter(menagepop) %>%
  mutate(classespop = classespop | (cs2 %in% c("81", "82"))) %>%
  filter(classespop) %>%
  group_by(cs2) %>%
  summarize(count_id = sum(ipondi))# On récupère le nombre d'individus qui rentre dans chaque catégorie.

relabel <- function(df, modalites, variable, VAR_CODE = "VAR_CODE", VAR_LIB = "VAR_LIB", MOD_LIB = "MOD_LIB", MOD_CODE = "MOD_CODE") {
  df[, unique(modalites[tolower(modalites[[VAR_CODE]]) %in% tolower(variable), VAR_LIB])] <-  modalites[tolower(modalites[[VAR_CODE]]) %in% tolower(variable), MOD_LIB][match(df[[variable]], modalites[tolower(modalites[[VAR_CODE]]) %in% tolower(variable), MOD_CODE])]
  return(df)
}

modalites_reg <- read.csv("./MOD_INDREG_2011.txt", sep = ";", stringsAsFactors = FALSE)


classes_populaires <- classes_populaires %>%
  relabel(modalites_reg, "cs2")




#GGPLOT2:
Palette <- c("#fef0d9","#fdd49e","#fdbb84","#fc8d59","#ef6548","#d7301f","#990000", "#bdc9e1", "#74a9cf","#0570b0")

classes_populaires %>%
  mutate(`Catégorie socioprofessionnelle en 24 postes` = factor(`Catégorie socioprofessionnelle en 24 postes`, levels = unique(`Catégorie socioprofessionnelle en 24 postes`))) %>%
  ggplot(aes(x = factor(1), y=count_id, fill=`Catégorie socioprofessionnelle en 24 postes`)) +
  geom_bar(
    stat="identity",
    position = "fill"
#    color="white",
#    vjust = -0.25
  )+
  scale_y_continuous(labels = percent_format(), breaks = NULL) +
  scale_fill_manual(values=Palette, name = "") +
  coord_polar("y") +
  scale_x_discrete(breaks=NULL) +
  ggthemes::theme_pander() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")+
  xlab("") +
  ylab("") +
  ggtitle("Actifs et inactifs dans les classes populaires")+
  guides(fill = guide_legend(reverse = TRUE, nrow = 5))
