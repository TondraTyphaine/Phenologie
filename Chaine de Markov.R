
## Packages necessaires
install.packages("pacman")
pacman::p_load ("tidyverse","plotly","strucchange","timeSeries","lubridate","bfast", "data.table",
                "ggfortify", "zoo", "readxl", "cluster", "stringr", "bookdown",
                "ggpubr", "kableExtra", "tibbletime", "pracma", "imputeTS",
                "TraMineR", "clValid", "FactoMineR", "factoextra", "dunn.test", "ggrepel")

## Source custom functions
source("Source_custom_functions/Func_dataPrepExplo.R")
source("Source_custom_functions/Func_analyse.R")
source("Source_custom_functions/myTheme.R")

## Lecture du jeu de données
read_csv2("data/Synthese_Pheno.csv") ->
  pheno


# On ajuste en supprimant les colonnes qu'on veut garder (dans ton jeux de données adapté tu as deux colonnes factices encore)
pheno[,-c(1,4)] -> 
  pheno


# Formatage des donnees
PrepPhase(pheno) -> pheno2 #Preparation des données brutes


# Formatage des colonnes
pheno2 = pheno2 %>% mutate(CrownID = as.factor(CrownID), # Pour être sure que ce soit considérer comme un facteur
                           PPVeg = str_replace_all(PPVeg,"(NA|Na|Na;|NA;)", NA_character_), # au cas-où il y a des NA mal écrits
                           PPVeg = as.factor(PPVeg), # Pour être sure que ce soit considérer comme un facteur
                           Update = as.Date(Update,format = "%d/%m/%Y")) # Pour être sure de la bonne date au bon format 





### CHAINE DE MARKOV ###

## Essai 1 → FAIL ##

#Package necessaire
installed.packages("msm")
library(msm)

# data
pheno2 %>% 
  filter(Genus_Spec== "Symphonia_globulifera", Usable== 1) %>% 
  select(Genus_Spec,date, Usable, PPraw) %>% 
  na.omit %>% 
  print() ->
  markovdata_glb

# Associer un numero a chaque etat different
#niveaux <- unique(markovdata_glb$PPraw) # nombre d'etat differents
#levels <- seq_along(niveaux) #incrementer de 1 le numero attribue pour chaque nouvel etat
#PPraw2 <- factor(markovdata_glb$PPraw, levels = niveaux, labels = levels ) 

# Nouvel colonne associant un numero a l'etat
#markovdata_glb %>% 
#mutate(PPraw2= PPraw2) ->
#markovdata_glb

# Matrice de transition initiale
matrice_init_glb = rbind(c(0,1,1,1), c(1,0,1,1),c(1,1,0,1),c(1,1,1,0))

# Etats
statetable.msm(PPraw, data = markovdata_glb)

# Chaine de markov 
mc_glb = msm(PPraw~date, qmatrix = matrice_init_glb, data = markovdata_glb)
# La somme des probabilites des lignes de la matrice de transition ne sont pas egales a 1

# Matrice de transition optimisee
pmatrix.msm(mc_glb)



## Essai 2 ## 

# Installation du package necessaire
install.packages("markovchain")
library(markovchain)


phenotest1 <- c("L","F","L","L","Fl","Fl","L","L","L","L")
phenotest2 <- c("L","L","L","L","L","Fl","L","L","L","L")
phenotest = list(phenotest1,phenotest2)
markovchainFit(phenotest)

# data pour Symphonia globulifera
pheno2 %>%
  filter(Genus_Spec== "Symphonia_globulifera", Usable== 1) %>% 
  select(PPraw,date) %>% 
  filter(!is.na(PPraw)) %>% 
  print() ->
  markovdata_glb

dim(markovdata_glb)

#Etats

markovdata_glb %>% 
  select(PPraw) %>% 
  unique() %>%
  print() ->
  states_glb

dim(states_glb)  

#Matrice de transition
markovdata_glb %>% 
  select(PPraw) ->
  data_matrice_glb

data_matrice_glb<- as.list(data_matrice_glb)

matrice_glb= markovchainFit(data= data_matrice_glb)

dim(matrice_glb)

# Modele de markov
mc_glb = new("markovchain", 
             states = states_glb, 
             transitionMatrix = matrice_glb, data = markovdata_glb)
