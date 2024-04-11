# Titre: Analyse phénophase
# Auteur: Tondra Typhaine
# Date: 08/04/2024

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




### Tidyverse ###

# Installation du package tidyverse

install.packages("tidyverse")
library("tidyverse")


## Lecture du jeu de données

read.csv("Synthese_Pheno.csv", header = T, sep = ";")%>%
  view("Synthese_Pheno.csv")->
  pheno

dim(pheno)


## Calcul du nombre d'especes

sp<-paste(pheno$Genus,pheno$Species)

sp %>% 
  unique() %>% 
  length %>% 
  print() ->
  Nbsp


## Nombre d'individu par espece

table(sp) %>% 
  print() ->
  idsp

## Filtrer les données pour l'espèce S.globulifera et les dates d'observation

pheno %>%
  
  # Creation d'une nouvelle colonne intitulee "espece".
  mutate(espece = sp) %>%
  
  # Filtration pour n'avoir que les lignes correspondant a S.globulifera
  filter(espece == "Symphonia globulifera") %>% 
  
  # Selection des colonnes d'interets
  select(Family:Species, X23.10.2020 : X23.01.2024) %>%
  
  print() ->
  globu


## Nombre de floraison

dim(globu)
globu[1:20,4:68]

# Conversion en tableau long pour pouvoir realiser un ggplot

globu %>% 
  pivot_longer(
    cols = espece,
    names_to = "variable",
    values_to = "variable"
  ) %>% 
  print() ->
  globu_fl








# Nombre de floraison par date d'observation
X23.10.2020 = as.factor(globu$X23.10.2020)
table(X23.10.2020)

dim(globu)
date<- globu[,4:49]
date
