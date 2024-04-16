# Titre: Analyse phenophase
# Auteur: Tondra Typhaine
# Date de creation: 08/04/2024
# Dernieres modifications : 16/04/2024



### Tidyverse (fait moi-meme grace au script du projet plan B) ###

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
  mutate(espece = paste(Genus, Species)) %>% 
  
  print()->
  Pheno
  
Pheno %>% 
  
  # Filtration pour n'avoir que les lignes correspondant a S.globulifera
  filter(espece == "Symphonia globulifera") %>% 
  
  # Selection des colonnes d'interets
  select(Family:Species, X23.10.2020 : X23.01.2024) %>%
  
  print() ->
  globu
       

## Nombre de floraison

# Conversion en tableau long pour pouvoir realiser un ggplot
globu %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "date",
    values_to = "phenohases"
  ) %>% 
  print() ->
  globu_fl


## Histogramme des phenophases par date d'observation

# Dates des observations
date <- globu_fl$date

# Les phenophases observees
phenophases <- globu_fl$phenohases


globu_fl %>% 
  ggplot(aes(x = date, fill = phenophases)) +
  geom_bar() +
  
  # Pour affichicher toutes les valeurs de l'axe Y de 0 à 20 par pas de 1 (représente le nombre d'individus observés)
  scale_y_continuous(breaks = seq(0, 20, by = 1)) + 
  
  #permet d'afficher les labels de l'axe x en diagonal
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = " Observations des phenophases", y =" Individus")


##################################################################################
##################################################################################



### Worflow floraison (issu du script de Mme.Derroire et M.Lagrange) ###

## Packages necessaires
install.packages("pacman")
pacman::p_load ("plotly","strucchange","timeSeries","lubridate","bfast", "data.table",
                "ggfortify", "zoo", "readxl", "cluster", "stringr", "bookdown",
                "ggpubr", "knitr", "kableExtra", "tibbletime", "pracma", "imputeTS",
                "TraMineR", "clValid", "FactoMineR", "factoextra", "dunn.test", "ggrepel")

install.packages("knitr")
library(knitr)

## Source custom functions
source("Source custom functions/Func_dataPrepExplo.R")
source("Source custom functions/Func_analyse.R")
source("Source custom functions/myTheme.R")


## Pivoter le tableau Pheno en tableau long
Pheno %>% 
  
  # Pivotement
  select(Num_crown,Usable, Family:Species, X23.10.2020 : X23.01.2024) %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "date",
    values_to = "phenophases"
  ) %>%
  
  # Ajout d'une colonne "espece"
  mutate(espece = paste(Genus,Species)) %>% 
  print()-> 
  Pheno_fl

## Nombre de floraison selon l'espèce choisi 

# Pour S.globulifera
Pheno_fl %>%
  filter(espece == "Symphonia globulifera") %>% 
  distinct(date,phenophases) %>%
  group_by(phenophases) %>% 
  summarise(n = n()) %>% 
  filter(grepl(";Fl|L;Fl|L/D;Fl|L/D?;Fl|L;Fl?|D;Fl|F;Fl", phenophases)) %>% 
  ungroup() %>% pull(n) %>% 
  sum() %>% 
  print()->
  n_event_flo_globu

# Pour V.americana
Pheno_fl %>%
  filter(espece == "Vouacapoua americana") %>% 
  distinct(date,phenophases) %>%
  group_by(phenophases) %>% 
  summarise(n = n()) %>% 
  filter(grepl(";Fl|L;Fl|L/D;Fl|L/D?;Fl|L;Fl?|D;Fl|F;Fl|;Fl?", phenophases)) %>% 
  ungroup() %>% pull(n) %>% 
  sum() %>% 
  print()->
  n_event_flo_americana


## Dates pour lesquelles il y a au moins 3 evenements de floraison pour l'ensemble des individus d'une meme espece.
condition_flo_globu = n_event_flo_globu > 3
condition_flo_americana = n_event_flo_americana > 3


## Pattern general

# Heatmap pour S.globulifera
PrepPhase(pheno) -> pheno2 #Preparation des données brutes
Leaf_Pattern(Data = pheno2 %>% filter(Usable == 1),
             Obs_Veg = "PPVeg", 
             Spec = Symphonia_globulifera, 
             fertility = TRUE)[[2]]


