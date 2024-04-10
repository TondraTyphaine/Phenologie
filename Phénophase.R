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



## Especes en floraison (Fl = flowering) ##

## Symphonia globulifera
#Phenologie pour S.globulifera
globu=subset(pheno[,13:77],pheno$Species=="globulifera"&pheno$Genus=="Symphonia")
globu
# Nombre d'individus
nrow(globu)
# Nombre de jours
length(globu)


## Symphonia sp.1
# Phenologie pour S.sp1
sp1=subset(pheno[,13:77],pheno$Species=="sp.1"&pheno$Genus=="Symphonia")
sp1
# Nombre d'individus
nrow(sp1)
# Nombre de jours
length(sp1)

## Vouacapou americana
# Phenologie pour 
am=subset(pheno[,13:77],pheno$Species=="americana")
am
# Nombre d'individus
nrow(am)
# Nombre de jours
length(am)




## Tidyverse ##

# Installation du package tidyverse
install.packages("tidyverse")
library("tidyverse")

# Lecture du jeu de données
pheno<-read.csv("Synthese_Pheno.csv", header = T, sep = ";")
print(pheno)

# Filtrer les données pour l'espèce S.globulifera et les dates d'observation
pheno %>% 
  filter(Genus == "Symphonia" & Species == "globulifera") %>%
  select(Family:Species, X23.10.2020 : X23.01.2024) %>% 
  print()-> globu

# Nombre de floraison par observation
X23.10.2020 = as.factor(globu$X23.10.2020)
table(X23.10.2020)

dim(globu)
date<- globu[,4:49]

