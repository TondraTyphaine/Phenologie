# Titre: Analyse phénophase
# Auteur: Tondra Typhaine
# Date: 08/04/2024


## Importation du jeu de données ##
pheno<-read.csv("Synthese_Pheno.csv", header = T, sep = ";")
pheno
dim(pheno)

## Calcul du nombre d'espèces ##
?paste()
# Assemble la colonne genre avec colonne espèce
Sp<-paste(pheno$Genus,pheno$Species)
Sp
# Enumération de chaque espèce du jeu de données
Nbsp=unique(Sp)
Nbsp
# Comptage du nombre d'espèces
Nb=length(Nbsp)
Nb
# Tableau de fréquence
#?table
#table(Nbsp)

## Espèce en floraison (Fl = flowering) ##


