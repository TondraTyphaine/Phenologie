# Titre: Analyse phenophase
# Auteur: Tondra Typhaine
# Date de creation: 08/04/2024
# Dernieres modifications : 16/04/2024


## Packages necessaires
install.packages("pacman")
pacman::p_load ("tidyverse","plotly","strucchange","timeSeries","lubridate","bfast", "data.table",
                "ggfortify", "zoo", "readxl", "cluster", "stringr", "bookdown",
                "ggpubr", "knitr", "kableExtra", "tibbletime", "pracma", "imputeTS",
                "TraMineR", "clValid", "FactoMineR", "factoextra", "dunn.test", "ggrepel")

## Source custom functions
source("Source_custom_functions/Func_dataPrepExplo.R")
source("Source_custom_functions/Func_analyse.R")
source("Source_custom_functions/myTheme.R")

## Lecture du jeu de données
read_csv2("Synthese_Pheno.csv") ->
  pheno

# On ajuste en supprimant les colonnes qu'on veut garder (dans ton jeux de données adapté tu as deux colonnes factices encore)
pheno[,-c(1,4)] -> 
  pheno


## Calcul du nombre d'especes
paste(pheno$Genus,pheno$Species) ->
  sp

sp %>% 
  unique() %>% 
  length %>% 
  print() ->
  Nbsp


## Nombre d'individu par espece
table(sp) %>% 
  print() ->
  idsp

# Creation d'une nouvelle colonne "espece"
pheno %>%
  mutate(espece = paste(Genus, Species)) %>% 
  print() ->
  pheno_sp

## Pivoter le tableau Pheno en tableau long
pheno_sp %>% 
  
  # Pivotement
  select(Num_crown,Usable,espece, Family:Species, `23/10/2020` : `23/01/2024`) %>% 
  pivot_longer(
    cols =c(`23/10/2020` : `23/01/2024`),
    names_to = "date",
    values_to = "phenophases"
  ) %>%
  print()-> 
  pheno_fl

## Nombre de floraison selon l'espèce choisi 

# Pour S.globulifera
pheno_fl %>%
  filter(espece == "Symphonia globulifera") %>% 
  distinct(date,phenophases) %>%
  group_by(phenophases) %>% 
  summarise(n = n()) %>% 
  filter(grepl(";Fl|L;Fl|L/D;Fl|L/D?;Fl|L;Fl?|D;Fl|F;Fl|L/D/F;Fl", phenophases)) %>% 
  ungroup() %>% pull(n) %>% 
  sum() %>% 
  print()->
  n_event_flo_globu

# Pour V.americana
pheno_fl %>%
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


# Formatage des donnees
PrepPhase(pheno) -> pheno2 #Preparation des données brutes

# Formatage des colonnes
pheno2 = pheno2 %>% mutate(CrownID = as.factor(CrownID), # Pour être sure que ce soit considérer comme un facteur
                           PPVeg = str_replace_all(PPVeg,"(NA|Na|Na;|NA;)", NA_character_), # au cas-où il y a des NA mal écrits
                           PPVeg = as.factor(PPVeg), # Pour être sure que ce soit considérer comme un facteur
                           Update = as.Date(Update,format = "%d/%m/%Y")) # Pour être sure de la bonne date au bon format 


## Pattern general

# Heatmap pour S.globulifera
Leaf_Pattern(
  Data = filter(pheno2, Usable == 1) ,
  Obs_Veg = "PPVeg",
  Spec = "Symphonia_globulifera", 
  fertility = TRUE
)[[2]]

# Heatmap pour S.sp1
Leaf_Pattern(
  Data = filter(pheno2, Usable == 1) ,
  Obs_Veg = "PPVeg",
  Spec = "Symphonia_sp.1", 
  fertility = TRUE
)[[2]]

# Heatmap pour V.americana
Leaf_Pattern(
  Data = filter(pheno2, Usable == 1) ,
  Obs_Veg = "PPVeg",
  Spec = "Vouacapoua_americana", 
  fertility = TRUE
)[[2]]


## Temps de sejour (difference entre le debut et la fin d'un evenement de floraison)

# Temps de sejour de la floraion pour S.globulifera
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Symphonia_globulifera",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

# Temps de sejour de la floraison pour S.sp1
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Symphonia_sp.1",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

# Temps de sejour de la floraison pour V.americana
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Vouacapoua_americana",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]


## Temps de retour (temps entre la fin d'un evenement de floraison
# et le debut du prochain evenement de floraison)

# Temps de retour pour S.globulifera
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Symphonia_globulifera",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

# Temps de retour pour S.sp1
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Symphonia_sp.1",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

# Temps de retour pour Vouacapoua_americana
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Vouacapoua_americana",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]


## Temps des cycles (temps de sejour + temps de retour)

# Temps de cycle pour S.globulifera
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Symphonia_globulifera",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

# Temps de cycle pour S.sp1
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Symphonia_sp.1",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

# Temps de cycle pour V.americana
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Vouacapoua_americana",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]


## Nombre d'individu en floraison par date

# Pour S.globulifera
GraphPropF_globu <- LeafedOTim(Data=pheno2,
                         Spec= "Symphonia_globulifera",
                         Pattern=c("Fl"),
                         Obs_Veg = "PPFlo")
GraphPropF_globu[[2]]

#Pour S.sp1
GraphPropF_sp1 <- LeafedOTim(Data=pheno2,
                               Spec= "Symphonia_sp.1",
                               Pattern=c("Fl"),
                               Obs_Veg = "PPFlo")
GraphPropF_sp1[[2]]

# Pour V.americana
GraphPropF_americana<- LeafedOTim(Data=pheno2,
                               Spec= "Vouacapoua_americana",
                               Pattern=c("Fl"),
                               Obs_Veg = "PPFlo")
GraphPropF_americana[[2]]


## Proportion de phenophase par mois

#Pour S.globulifera
Leaf_Circular(Data = pheno2, 
              Spec = "Symphonia_globulifera",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

#Pour S.sp1
Leaf_Circular(Data = pheno2, 
              Spec = "Symphonia_sp.1",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

# Pour V.americana
Leaf_Circular(Data = pheno2, 
              Spec = "Vouacapoua_americana",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]


# Proportion de floraison par mois et par annees

#Pour S.globulifera
Leaf_Circular(Data = pheno2, 
              Spec = "Symphonia_globulifera",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = TRUE)[[2]]

#Pour S.sp1
Leaf_Circular(Data = pheno2, 
              Spec = "Symphonia_sp.1",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = TRUE)[[2]]

# Pour V.americana
Leaf_Circular(Data = pheno2, 
              Spec = "Vouacapoua_americana",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = TRUE)[[2]]


## Temporalité de la proportion d'individu en floraison

# Pour S.globulifera #

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal = LeafedOTim(Data=pheno2 %>% 
                         filter(Usable==1),
                         Spec= "Symphonia_globulifera",
                         Pattern=c("Fl"),
                         Obs_Veg = "PPFlo")[[1]]

# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal = data_signal %>% 
  select(prop) %>% 
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile = moving_average(data_signal %>% 
                                select(prop) %>% 
                                pull(),
                                filter = fpoids(n=2,p=2,q=2)$y) 





### Code hors script PhenObs ###       

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




