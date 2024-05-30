# Titre: Analyse phenophase
# Auteur: Tondra Typhaine
# Date de creation: 08/04/2024
# Dernieres modifications : 03/05/2024


# ANALYSES QUALITATIVES DE LA PHENOLOGIE FLORALE #

## Packages necessaires
install.packages("pacman")
pacman::p_load ("tidyverse","plotly","strucchange","timeSeries","lubridate","bfast", "data.table",
                "ggfortify", "zoo", "readxl", "cluster", "stringr", "bookdown",
                "ggpubr", "kableExtra", "tibbletime", "pracma", "imputeTS",
                "TraMineR", "clValid", "FactoMineR", "factoextra", "dunn.test", "ggrepel")


install.packages("knitr")
install.packages("tidyverse")
library(knitr)
library(tidyverse)


## Source custom functions
source("Source_custom_functions/Func_dataPrepExplo.R")
source("Source_custom_functions/Func_analyse.R")
source("Source_custom_functions/myTheme.R")


## Lecture du jeu de données
read_csv2("data/Synthese_Pheno_20230724.csv") ->
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


## Les 20 especes les plus abondantes
pheno2 %>% 
  filter(!is.na(PPVeg)) %>% 
  select(Genus_Spec,CrownID) %>% 
  distinct() %>% 
  group_by(Genus_Spec) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  slice_head(n=20)


## Creation d'une nouvelle colonne "espece"
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
?pivot_longer

## Nombre de floraison selon l'espèce choisi 

# Pour S.globulifera
pheno_fl %>%
  filter(espece == "Symphonia globulifera") %>% 
  distinct(date,phenophases) %>%
  group_by(phenophases) %>% 
  summarise(n = n()) %>% 
  filter(grepl(";Fl|L;Fl|L/D;Fl|L/D?;Fl|L;Fl?|D;Fl|F;Fl|L/D/F;Fl", phenophases)) %>% 
  ungroup() %>% 
  pull(n) %>% 
  sum() %>% 
  print()->
  n_event_flo_globu

# Pour S.sp1
pheno_fl %>%
  filter(espece == "Symphonia sp.1") %>% 
  distinct(date,phenophases) %>%
  group_by(phenophases) %>% 
  summarise(n = n()) %>% 
  filter(grepl(";Fl|L;Fl|L/D;Fl|L/D?;Fl|L;Fl?|D;Fl|F;Fl|L/D/F;Fl", phenophases)) %>% 
  ungroup() %>% pull(n) %>% 
  sum() %>% 
  print()->
  n_event_flo_sp1

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
condition_flo_sp1 = n_event_flo_sp1 > 3
condition_flo_americana = n_event_flo_americana > 3


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

# Heatmap pour Couma guianensis
Leaf_Pattern(
  Data = filter(pheno2, Usable == 1) ,
  Obs_Veg = "PPVeg",
  Spec = "Couma_guianensis", 
  fertility = TRUE
)[[2]]

# Heatmap pour Moronobea coccinea
Leaf_Pattern(
  Data = filter(pheno2, Usable == 1) ,
  Obs_Veg = "PPVeg",
  Spec = "Moronobea_coccinea", 
  fertility = TRUE
)[[2]]

# Heatmap pour Platonia insignis
Leaf_Pattern(
  Data = filter(pheno2, Usable == 1) ,
  Obs_Veg = "PPVeg",
  Spec = "Platonia_insignis", 
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

# Temps de sejour de la floraison pour C.guianensis
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Couma_guianensis",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

# Temps de sejour de la floraison pour M.coccinea
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Moronobea_coccinea",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

# Temps de sejour de la floraison pour P.insignis
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Platonia_insignis",
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

# Temps de retour pour C.guianensis
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Couma_guianensis",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

# Temps de retour pour M.coccinea
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Moronobea_coccinea",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

# Temps de retour pour P.insignis
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Platonia_insignis",
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

# Temps de cycle pour C.guianensis
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Couma_guianensis",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

# Temps de cycle pour M.coccinea
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Moronobea_coccinea",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

# Temps de cycle pour P.insignis
PhenoPhase_Time(
  Data = pheno2,
  Pattern = "Fl",
  Spec = "Platonia_insignis",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]



## Nombre d'individu en floraison par date

# Pour S.globulifera
GraphPropF_globu <- LeafedOTim(Data=pheno2,
                         Spec = "Symphonia_globulifera",
                         Pattern = c("Fl"),
                         Obs_Veg = "PPFlo")
GraphPropF_globu[[2]]

#Pour S.sp1
GraphPropF_sp1 <- LeafedOTim(Data=pheno2,
                               Spec = "Symphonia_sp.1",
                               Pattern = c("Fl"),
                               Obs_Veg = "PPFlo")
GraphPropF_sp1[[2]]

# Pour V.americana
GraphPropF_americana<- LeafedOTim(Data=pheno2,
                               Spec = "Vouacapoua_americana",
                               Pattern = c("Fl"),
                               Obs_Veg = "PPFlo")
GraphPropF_americana[[2]]

# Pour C.guianensis
GraphPropF_guianensis <- LeafedOTim(Data=pheno2,
                                  Spec= "Couma_guianensis",
                                  Pattern=c("Fl"),
                                  Obs_Veg = "PPFlo")
GraphPropF_guianensis[[2]]

# Pour M.coccinea
GraphPropF_coccinea <- LeafedOTim(Data=pheno2,
                                    Spec= "Moronobea_coccinea",
                                    Pattern=c("Fl"),
                                    Obs_Veg = "PPFlo")
GraphPropF_coccinea[[2]]

# Pour P.insignis
GraphPropF_insignis <- LeafedOTim(Data=pheno2,
                                  Spec= "Platonia_insignis",
                                  Pattern=c("Fl"),
                                  Obs_Veg = "PPFlo")
GraphPropF_insignis[[2]]


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
              perYears = F)[[2]]

# Pour V.americana
Leaf_Circular(Data = pheno2, 
              Spec = "Vouacapoua_americana",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

# Pour C.guianensis
Leaf_Circular(Data = pheno2, 
              Spec = "Couma_guianensis",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

# Pour M.coccinea
Leaf_Circular(Data = pheno2, 
              Spec = "Moronobea_coccinea",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

# Pour P.insignis
Leaf_Circular(Data = pheno2, 
              Spec = "Platonia_insignis",
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

# Pour C.guianensis
Leaf_Circular(Data = pheno2, 
              Spec = "Couma_guianensis",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = T)[[2]]

# Pour M.coccinea
Leaf_Circular(Data = pheno2, 
              Spec = "Moronobea_coccinea",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = T)[[2]]

# Pour P.insignis
Leaf_Circular(Data = pheno2, 
              Spec = "Platonia_insignis",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = T)[[2]]


## Temporalité de la proportion d'individu en floraison

# Pour S.globulifera #

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_globu = LeafedOTim(Data=pheno2 %>% 
                         filter(Usable==1),
                         Spec= "Symphonia_globulifera",
                         Pattern=c("Fl"),
                         Obs_Veg = "PPFlo")[[1]]

# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_globu = data_signal_globu %>% 
  select(prop) %>% 
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_globu = moving_average(data_signal_globu %>% 
                                select(prop) %>% 
                                pull(),
                                filter = fpoids(n=2,p=2,q=2)$y) 

#   On indentifie les différents pics positif et négatif de la floraison
dates_globu = data_signal_globu %>% 
  select(date) %>% 
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet d'ordonner dans l'ordre croissant les elements d'un vecteur
# findpeaks () premet de trouver les pics de maximum de floraison
dates_max_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks 
amplitude_peaks_globu = findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_globu = signal_globu[dates_max_globu]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2
Plot = ggplot(data_signal_globu, 
              aes(x = date)) +
  geom_line(aes(y = prop, 
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_globu, 
                color = "processed signal"))+
  geom_point(aes(y = prop, 
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_globu[dates_max_globu],
             col = "tomato3" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_globu[dates_begin_globu],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_globu[dates_end_globu],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Symphonia globulifera", 
       x = "Time", y = "Value") + 
  annotate("text",x = dates_globu[dates_max_globu], 
           y= 100,label = paste(round(amplitude_real_globu,1),"%"),
           col = "grey40")

Plot

# Ajout des pics de floraison precedenment calcules sur la figure du pattern general de S.globulifera
Leaf_Pattern(Data = pheno2 %>% filter(Usable ==1), 
             Obs_Veg ="PPVeg", 
             Spec = "Symphonia_globulifera", 
             fertility = TRUE)[[2]] +    
  geom_vline(xintercept = dates_globu[dates_max_globu], 
             col = "white" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_globu[dates_begin_globu], 
             col = "black" , 
             linetype = "dashed") +
  geom_vline(xintercept = dates_globu[dates_end_globu], 
             col = "black" , 
             linetype = "dashed")


# Recapitulatif des informations du signal floral pour S.globulifera
summary_table_globu = tibble(Genus_Spec = data_signal_globu$Genus_Spec %>% unique(),
                             max = dates_globu[dates_max_globu],
                             range = abs(difftime(dates_globu[dates_begin_globu],
                                                  dates_globu[dates_end_globu])),
                             start = dates_globu[dates_begin_globu],
                             end = dates_globu[dates_end_globu]
)

# Representation formatee pour Markdown (une table)
kable(summary_table_globu)


# Pour S.sp1 #

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_sp1 = LeafedOTim(Data = pheno2 %>% 
                               filter(Usable==1),
                               Spec= "Symphonia_sp.1",
                               Pattern=c("Fl"),
                               Obs_Veg = "PPFlo")[[1]]

# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_sp1 = data_signal_sp1 %>% 
  select(prop) %>% 
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_sp1 = moving_average(data_signal_sp1 %>% 
                                        select(prop) %>% 
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y) 

#   On indentifie les différents pics positif et négatif de la floraison
dates_sp1 = data_signal_sp1 %>% 
  select(date) %>% 
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet de 
# findpeaks () premet de trouver
dates_max_sp1 = sort(findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_sp1 = sort(findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_sp1 = sort(findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks 
amplitude_peaks_sp1 = findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_sp1 = signal_sp1[dates_max_sp1]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2
Plot = ggplot(data_signal_sp1, 
              aes(x = date)) +
  geom_line(aes(y = prop, 
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_sp1, 
                color = "processed signal"))+
  geom_point(aes(y = prop, 
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_sp1[dates_max_sp1],
             col = "tomato3" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_sp1[dates_end_sp1],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Symphonia sp1", 
       x = "Time", y = "Value") + 
  annotate("text",x = dates_sp1[dates_max_sp1], 
           y= 100,label = paste(round(amplitude_real_sp1,1),"%"),
           col = "grey40")

Plot

# Ajout des pics de floraison precedenment calcules sur la figure du pattern general de S.globulifera
Leaf_Pattern(Data = pheno2 %>% filter(Usable ==1), 
             Obs_Veg ="PPVeg", 
             Spec = "Symphonia_sp.1", 
             fertility = TRUE)[[2]] +    
  geom_vline(xintercept = dates_sp1[dates_max_sp1], 
             col = "white" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1], 
             col = "black" , 
             linetype = "dashed") +
  geom_vline(xintercept = dates_sp1[dates_end_sp1], 
             col = "black" , 
             linetype = "dashed")


# Recapitulatif des informations du signal floral pour S.globulifera
summary_table_sp1 = tibble(Genus_Spec = data_signal_sp1$Genus_Spec %>% unique(),
                             max = dates_sp1[dates_max_sp1],
                             range = abs(difftime(dates_sp1[dates_begin_sp1],
                                                  dates_sp1[dates_end_sp1])),
                             start = dates_sp1[dates_begin_sp1],
                             end = dates_sp1[dates_end_sp1]
)

# Representation formatee pour Markdown (une table)
kable(summary_table_sp1)


# Pour V.americana #

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_am = LeafedOTim(Data = pheno2 %>% 
                               filter(Usable==1),
                             Spec= "Vouacapoua_americana",
                             Pattern=c("Fl"),
                             Obs_Veg = "PPFlo")[[1]]

# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_am = data_signal_am %>% 
  select(prop) %>% 
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_am = moving_average(data_signal_am %>% 
                                      select(prop) %>% 
                                      pull(),
                                    filter = fpoids(n=2,p=2,q=2)$y) 

#   On indentifie les différents pics positif et négatif de la floraison
dates_am = data_signal_am %>% 
  select(date) %>% 
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet de trier les elements d'un vecteur
# findpeaks () premet de trouver les pics de floraison
dates_max_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups =0)[,2])

# When the pics begin
dates_begin_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 0)[,3])

# When the pics end
dates_end_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 0)[,4])

# Percent of ind by peaks 
amplitude_peaks_am = findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 0)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_am = signal_am[dates_max_am]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2
Plot = ggplot(data_signal_am, 
              aes(x = date)) +
  geom_line(aes(y = prop, 
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_am, 
                color = "processed signal"))+
  geom_point(aes(y = prop, 
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_am[dates_max_am],
             col = "tomato3" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_am[dates_begin_am],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_am[dates_end_am],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Vouacapoua americana", 
       x = "Time", y = "Value") + 
  annotate("text",x = dates_am[dates_max_am], 
           y = 35,label = paste(round(amplitude_real_am,1),"%"),
           col = "grey40")

Plot

# Ajout des pics de floraison precedenment calcules sur la figure du pattern general de V.americana
Leaf_Pattern(Data = pheno2 %>% filter(Usable ==1), 
             Obs_Veg ="PPVeg", 
             Spec = "Vouacapoua_americana", 
             fertility = TRUE)[[2]] +    
  geom_vline(xintercept = dates_am[dates_max_am], 
             col = "white" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_am[dates_begin_am], 
             col = "black" , 
             linetype = "dashed") +
  geom_vline(xintercept = dates_am[dates_end_am], 
             col = "black" , 
             linetype = "dashed")


# Recapitulatif des informations du signal floral pour S.globulifera
summary_table_am = tibble(Genus_Spec = data_signal_am$Genus_Spec %>% unique(),
                           max = dates_am[dates_max_am],
                           range = abs(difftime(dates_am[dates_begin_am],
                                                dates_am[dates_end_am])),
                           start = dates_am[dates_begin_am],
                           end = dates_am[dates_end_am]
)

# Representation formatee pour Markdown (une table)
kable(summary_table_am)



# Pour C.guianensis #

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_gui = LeafedOTim(Data = pheno2 %>% 
                              filter(Usable==1),
                            Spec= "Couma_guianensis",
                            Pattern=c("Fl"),
                            Obs_Veg = "PPFlo")[[1]]

# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_gui = data_signal_gui %>% 
  select(prop) %>% 
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_gui = moving_average(data_signal_gui %>% 
                                     select(prop) %>% 
                                     pull(),
                                   filter = fpoids(n=2,p=2,q=2)$y) 

#   On indentifie les différents pics positif et négatif de la floraison
dates_gui = data_signal_gui %>% 
  select(date) %>% 
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet de trier les elements d'un vecteur
# findpeaks () premet de trouver les pics de floraison
dates_max_gui = sort(findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_gui = sort(findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_gui = sort(findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks 
amplitude_peaks_gui = findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_gui = signal_gui[dates_max_gui]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2
Plot = ggplot(data_signal_gui, 
              aes(x = date)) +
  geom_line(aes(y = prop, 
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_gui, 
                color = "processed signal"))+
  geom_point(aes(y = prop, 
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_gui[dates_max_gui],
             col = "tomato3" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_gui[dates_begin_gui],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_gui[dates_end_gui],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Couma guianensis", 
       x = "Time", y = "Value") + 
  annotate("text",x = dates_gui[dates_max_gui], 
           y= 100,label = paste(round(amplitude_real_gui,1),"%"),
           col = "grey40")

Plot

# Ajout des pics de floraison precedenment calcules sur la figure du pattern general de V.americana
Leaf_Pattern(Data = pheno2 %>% filter(Usable ==1), 
             Obs_Veg ="PPVeg", 
             Spec = "Couma_guianensis", 
             fertility = TRUE)[[2]] +    
  geom_vline(xintercept = dates_gui[dates_max_gui], 
             col = "white" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_gui[dates_begin_gui], 
             col = "black" , 
             linetype = "dashed") +
  geom_vline(xintercept = dates_gui[dates_end_gui], 
             col = "black" , 
             linetype = "dashed")


# Recapitulatif des informations du signal floral pour S.globulifera
summary_table_gui = tibble(Genus_Spec = data_signal_gui$Genus_Spec %>% unique(),
                          max = dates_gui[dates_max_gui],
                          range = abs(difftime(dates_gui[dates_begin_gui],
                                               dates_gui[dates_end_gui])),
                          start = dates_gui[dates_begin_gui],
                          end = dates_gui[dates_end_gui]
)

# Representation formatee pour Markdown (une table)
kable(summary_table_gui)


# Pour M.coccinea #

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_cocci = LeafedOTim(Data = pheno2 %>% 
                               filter(Usable==1),
                             Spec= "Moronobea_coccinea",
                             Pattern=c("Fl"),
                             Obs_Veg = "PPFlo")[[1]]

# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_cocci = data_signal_cocci %>% 
  select(prop) %>% 
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_cocci = moving_average(data_signal_cocci %>% 
                                      select(prop) %>% 
                                      pull(),
                                    filter = fpoids(n=2,p=2,q=2)$y) 

#   On indentifie les différents pics positif et négatif de la floraison
dates_cocci = data_signal_cocci %>% 
  select(date) %>% 
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet de trier les elements d'un vecteur
# findpeaks () premet de trouver les pics de floraison
dates_max_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10, nups = 0)[,2])
dates_max_cocci <- dates_max_cocci[1]

# When the pics begin
dates_begin_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10, nups = 0)[,3])[1]

# When the pics end
dates_end_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10, nups = 0)[,4])[3]

# Percent of ind by peaks 
amplitude_peaks_cocci = findpeaks(moyenne_mobile_cocci,minpeakheight = 10, nups = 0)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_cocci = signal_cocci[dates_max_cocci]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2
Plot = ggplot(data_signal_cocci, 
              aes(x = date)) +
  geom_line(aes(y = prop, 
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_cocci, 
                color = "processed signal"))+
  geom_point(aes(y = prop, 
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_cocci[dates_max_cocci],
             col = "tomato3" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_cocci[dates_end_cocci],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Moronobea coccinea", 
       x = "Time", y = "Value") + 
  annotate("text",x = dates_cocci[dates_max_cocci], 
           y = 25,label = paste(round(amplitude_real_cocci,1),"%"),
           col = "grey40")

Plot

# Ajout des pics de floraison precedenment calcules sur la figure du pattern general de V.americana
Leaf_Pattern(Data = pheno2 %>% filter(Usable ==1), 
             Obs_Veg ="PPVeg", 
             Spec = "Moronobea_coccinea", 
             fertility = TRUE)[[2]] +    
  geom_vline(xintercept = dates_cocci[dates_max_cocci], 
             col = "white" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci], 
             col = "black" , 
             linetype = "dashed") +
  geom_vline(xintercept = dates_cocci[dates_end_cocci], 
             col = "black" , 
             linetype = "dashed")


# Recapitulatif des informations du signal floral pour S.globulifera
summary_table_cocci = tibble(Genus_Spec = data_signal_cocci$Genus_Spec %>% unique(),
                           max = dates_cocci[dates_max_cocci],
                           range = abs(difftime(dates_cocci[dates_begin_cocci],
                                                dates_cocci[dates_end_cocci])),
                           start = dates_cocci[dates_begin_cocci],
                           end = dates_cocci[dates_end_cocci]
)

# Representation formatee pour Markdown (une table)
kable(summary_table_cocci)



# Pour P.insignis #

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_insi = LeafedOTim(Data = pheno2 %>% 
                                 filter(Usable==1),
                               Spec= "Platonia_insignis",
                               Pattern=c("Fl"),
                               Obs_Veg = "PPFlo")[[1]]

# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_insi = data_signal_insi %>% 
  select(prop) %>% 
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_insi = moving_average(data_signal_insi %>% 
                                        select(prop) %>% 
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y) 

#   On indentifie les différents pics positif et négatif de la floraison
dates_insi = data_signal_insi %>% 
  select(date) %>% 
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet de trier les elements d'un vecteur
# findpeaks () premet de trouver les pics de floraison
dates_max_insi = sort(findpeaks(moyenne_mobile_insi,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_insi = sort(findpeaks(moyenne_mobile_insi,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_insi = sort(findpeaks(moyenne_mobile_insi,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks 
amplitude_peaks_insi = findpeaks(moyenne_mobile_insi,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_insi = signal_insi[dates_max_insi]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2
Plot = ggplot(data_signal_insi, 
              aes(x = date)) +
  geom_line(aes(y = prop, 
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_insi, 
                color = "processed signal"))+
  geom_point(aes(y = prop, 
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_insi[dates_max_insi],
             col = "tomato3" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_insi[dates_begin_insi],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_insi[dates_end_insi],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Platonia insignis", 
       x = "Time", y = "Value") + 
  annotate("text",x = dates_insi[dates_max_insi], 
           y= 100,label = paste(round(amplitude_real_insi,1),"%"),
           col = "grey40")

Plot

# Ajout des pics de floraison precedenment calcules sur la figure du pattern general de V.americana
Leaf_Pattern(Data = pheno2 %>% filter(Usable ==1), 
             Obs_Veg ="PPVeg", 
             Spec = "Platonia_insignis", 
             fertility = TRUE)[[2]] +    
  geom_vline(xintercept = dates_insi[dates_max_insi], 
             col = "white" , 
             linetype = "dashed") + 
  geom_vline(xintercept = dates_insi[dates_begin_insi], 
             col = "black" , 
             linetype = "dashed") +
  geom_vline(xintercept = dates_insi[dates_end_insi], 
             col = "black" , 
             linetype = "dashed")


# Recapitulatif des informations du signal floral pour S.globulifera
summary_table_insi = tibble(Genus_Spec = data_signal_insi$Genus_Spec %>% unique(),
                             max = dates_insi[dates_max_insi],
                             range = abs(difftime(dates_insi[dates_begin_insi],
                                                  dates_insi[dates_end_insi])),
                             start = dates_insi[dates_begin_insi],
                             end = dates_insi[dates_end_insi]
)

# Representation formatee pour Markdown (une table)
kable(summary_table_insi)

