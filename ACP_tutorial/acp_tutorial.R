#https://www.sthda.com/french/wiki/analyse-en-composante-principale-avec-r


library('corrr')
library("FactoMineR")
library ("factoextra")
setwd("/home/oryx/Documents/r_dataviz/ACP_tutorial")
data<-read.table(file=file.choose(), sep="\t", header=TRUE, row.names=1, check.names=FALSE)

pca<-PCA(data)
pca$eig #obtenir les dimensions et la contribution des axes factoriels
pca$var$coord  # Affiche les coordonn?es des variables
pca$var$cor #Affiche les corr?lations variables - axes factoriels
pca$var$cos2 #qualit? de projection des variables
pca$var$contrib # contribution des variables aux axes factoriels
pca$ind$coord # coordonn?es des individus dans le plan factoriel
pca$ind$cos2 # Qualit? de repr?sentation des individus
pca$ind$contrib # Contribution des individus aux axes factoriels

write.infile(pca, file="pca_result.txt", sep="\t")
