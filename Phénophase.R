# Titre: Analyse phénophase
# Auteur: Tondra Typhaine
# Date: 08/04/2024


## Importation du jeu de donnees ##
pheno<-read.csv("Synthese_Pheno.csv", header = T, sep = ";")
pheno
dim(pheno)
summary(pheno)

## Calcul du nombre d'especes ##
?paste()
# Assemble la colonne genre avec colonne espece
Sp<-paste(pheno$Genus,pheno$Species)
Sp
# Enumération de chaque espece du jeu de donnees
Nbsp=unique(Sp)
Nbsp
# Comptage du nombre d'especes
Nb=length(Nbsp)
Nb
# Tableau de frequence
#?table
#table(Nbsp)



## Espece en floraison (Fl = flowering) ##

## Exemple de Symphonia globulifera
#Phenologie pour S.globulifera
globu=subset(pheno[,13:77],pheno$Species=="globulifera"&pheno$Genus=="Symphonia")
globu

# Phenologie pour Vouacapou americana
am=subset(pheno[,13:77],pheno$Species=="americana")
am

