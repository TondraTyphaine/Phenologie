# Titre : Analyses conjointe des variables climatiques et phénologiques
# Auteur : Tondra Typhaine
# Date de creation : 22/05/2024

# Packages
pacman::p_load ("tidyverse","plotly","strucchange","timeSeries","lubridate","bfast", "data.table",
                "ggfortify", "zoo", "readxl", "cluster", "stringr", "bookdown",
                "ggpubr", "kableExtra", "tibbletime", "pracma", "imputeTS",
                "TraMineR", "clValid", "FactoMineR", "factoextra", "dunn.test", "ggrepel")

pacman::p_load("tidyverse","lubridate","RColorBrewer","pracma", "vioplot","lmtest")


## Source custom functions
source("Source_custom_functions/Func_dataPrepExplo.R")
source("Source_custom_functions/Func_analyse.R")
source("Source_custom_functions/myTheme.R")


### FLORAISON ###

## Data brutes
read_csv2("data/Synthese_Pheno_20230724.csv") ->
  pheno

# On ajuste en supprimant les colonnes qu'on veut garder (dans ton jeux de données adapté tu as deux colonnes factices encore)
pheno[,-c(1,4)] -> 
  pheno

# Formatage des donnees
PrepPhase(pheno) -> pheno2 #Preparation des données brutes

# Data pour Symphonia globulifera
pheno2 %>% 
  filter(Genus_Spec == "Symphonia_globulifera") %>% 
  select(date, PPFlo, CrownID) %>% 
  mutate(PPFlo = if_else(is.na(PPFlo), "not_Fl", PPFlo)) %>% 
  print() ->
  Sympho_glb

Sympho_glb %>% 
  mutate(Nb_Flo = ifelse(PPFlo == "Fl", 1, 0)) %>% 
  group_by(date) %>% 
  summarise(Nb_Flo = sum(Nb_Flo)) %>% 
  select(date, Nb_Flo) %>% 
  print() ->
  Floraison_glb

sum(is.na(Floraison_glb))

# Nombre de floraison par date pour chaque annee
Flo_glb_2020 <- Floraison_glb[year(Floraison_glb$date) == 2020,]
Flo_glb_2021 <- Floraison_glb[year(Floraison_glb$date) == 2021,]
Flo_glb_2022 <- Floraison_glb[year(Floraison_glb$date) == 2022,]
Flo_glb_2023 <- Floraison_glb[year(Floraison_glb$date) == 2023,]
Flo_glb_2024 <- Floraison_glb[year(Floraison_glb$date) == 2024,]

# Data pour Symphonia sp1
pheno2 %>% 
  filter(Genus_Spec == "Symphonia_sp.1") %>% 
  select(date, PPFlo, CrownID) %>% 
  mutate(PPFlo = if_else(is.na(PPFlo), "not_Fl", PPFlo)) %>% 
  print() ->
  Sympho_sp1

Sympho_sp1 %>% 
  mutate(Nb_Flo = ifelse(PPFlo == "Fl", 1, 0)) %>% 
  group_by(date) %>% 
  summarise(Nb_Flo = sum(Nb_Flo)) %>% 
  select(date, Nb_Flo) %>% 
  print() ->
  Floraison_sp1

sum(is.na(Floraison_sp1))

# Nombre de floraison par date pour chaque annee
Flo_sp1_2020 <- Floraison_sp1[year(Floraison_sp1$date) == 2020,]
Flo_sp1_2021 <- Floraison_sp1[year(Floraison_sp1$date) == 2021,]
Flo_sp1_2022 <- Floraison_sp1[year(Floraison_sp1$date) == 2022,]
Flo_sp1_2023 <- Floraison_sp1[year(Floraison_sp1$date) == 2023,]
Flo_sp1_2024 <- Floraison_sp1[year(Floraison_sp1$date) == 2024,]

# Data pour Vouacapoua americana
pheno2 %>% 
  filter(Genus_Spec == "Vouacapoua_americana") %>% 
  select(date, PPFlo, CrownID) %>% 
  mutate(PPFlo = if_else(is.na(PPFlo), "not_Fl", PPFlo)) %>% 
  print() ->
  Sympho_ame

Sympho_ame %>% 
  mutate(Nb_Flo = ifelse(PPFlo == "Fl", 1, 0)) %>% 
  group_by(date) %>% 
  summarise(Nb_Flo = sum(Nb_Flo)) %>% 
  select(date, Nb_Flo) %>% 
  print() ->
  Floraison_ame

sum(is.na(Floraison_ame))

# Nombre de floraison par date pour chaque annee
Flo_ame_2020 <- Floraison_ame[year(Floraison_ame$date) == 2020,]
Flo_ame_2021 <- Floraison_ame[year(Floraison_ame$date) == 2021,]
Flo_ame_2022 <- Floraison_ame[year(Floraison_ame$date) == 2022,]
Flo_ame_2023 <- Floraison_ame[year(Floraison_ame$date) == 2023,]
Flo_ame_2024 <- Floraison_ame[year(Floraison_ame$date) == 2024,]

#### CLIMAT (donnees de M.Bonal) ####

## Data 
dataB<- read_csv2("data/GX-METEO-2020 - 2024E - AK.csv")

## Reduction deu jeu de donnees
dataB %>% 
  filter(!is.na(`Temp(55)`)) %>% 
  filter(!is.na(Rain)) %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(vpd55)) %>%
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(T_10cm)) %>% 
  select(Year,Month, Day,`J/N`, 
         `Temp(55)`, `Hr(55)`,
         vpd55,
         Rain, 
         ETP,
         VWC_10cm,
         T_10cm) %>% 
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            vpd55= mean(vpd55),
            Rain= sum(Rain),
            ETP = sum(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm)) ->
  dataB


## Data brutes pour chaque annee

# Pour 2020
dataB %>% 
  filter(Year == 2020) %>% 
  mutate(date = ymd("2020-01-01") + days(Day - 1)) %>% 
  print() ->
  dataB2020

# Pour 2021
dataB %>% 
  filter(Year == 2021) %>% 
  mutate(date = ymd("2021-01-01") + days(Day - 1)) %>%
  print() ->
  dataB2021

# Pour 2022
dataB %>% 
  filter(Year == 2022) %>% 
  mutate(date = ymd("2022-01-01") + days(Day - 1)) %>%
  print() ->
  dataB2022

# Pour 2023
dataB %>% 
  filter(Year == 2023) %>% 
  mutate(date = ymd("2023-01-01") + days(Day - 1)) %>%
  print() ->
  dataB2023

# Pour 2024
dataB %>% 
  filter(!is.na(Rain)) %>% 
  filter(Year == 2024) %>% 
  mutate(date = ymd("2024-01-01") + days(Day - 1)) %>%
  print() ->
  dataB2024

## Empilement 
bind_rows(dataB2020) %>% 
  bind_rows(dataB2021) %>% 
  bind_rows(dataB2022) %>% 
  bind_rows(dataB2023) %>% 
  bind_rows(dataB2024) %>% 
  print ->
  dataB_resume

dataB_resume %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),`Hr(55)` = mean(`Hr(55)`), vpd55 = mean(vpd55), 
            Rain = sum(Rain),ETP = sum(ETP), VWC_10cm = mean(VWC_10cm)) %>% 
  print() ->
  climat


climat %>% 
  filter(Year == 2020) %>% 
  print() ->
  climat_2020

climat %>% 
  filter(Year == 2021) %>% 
  print() ->
  climat_2021

climat %>% 
  filter(Year == 2022) %>% 
  print() ->
  climat_2022

climat %>% 
  filter(Year == 2023) %>% 
  print() ->
  climat_2023

climat %>% 
  filter(Year == 2024) %>% 
  print() ->
  climat_2024


## Calculer la corrélation croisée entre Nb_Flo et Rain pour S.globulifera ##

# Convertir la colonne date en classe Date
Floraison_glb$date <- as.Date(Floraison_glb$date)
climat$date <- as.Date(climat$date)

# Sur les 4 ans
par(mar = c(3, 2, 2, 1) + 0.1)
correlation <- ccf(Floraison_glb$Nb_Flo, climat$Rain)
png("correlation_full.png", width = 800, height = 600)
plot(correlation)
dev.off()

# Pour 2020
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_2020 <- ccf(Flo_glb_2020$Nb_Flo, climat_2020$Rain)
png("correlation_2020.png", width = 800, height = 600)
plot(correlation_2020)
dev.off()

# Pour 2021
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_2021 <- ccf(Flo_glb_2021$Nb_Flo, climat_2021$Rain)
png("correlation_2021.png", width = 800, height = 600)
plot(correlation_2021)
dev.off()

# Pour 2022
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_2022 <- ccf(Flo_glb_2022$Nb_Flo, climat_2022$Rain)
png("correlation_2022.png", width = 800, height = 600)
plot(correlation_2022)
dev.off()

# Pour 2023
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_2023 <- ccf(Flo_glb_2023$Nb_Flo, climat_2023$Rain)
png("correlation_2023.png", width = 800, height = 600)
plot(correlation_2023)
dev.off()

# Pour 2024 (pas assez d'element pour avoir un graphique correct)
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_2024 <- ccf(Flo_glb_2024$Nb_Flo, climat_2024$Rain)
png("correlation_2024.png", width = 800, height = 600)
plot(correlation_2024)
dev.off()


## Calculer la corrélation croisée entre Nb_Flo et Rain pour S.sp1 ##

# Convertir la colonne date en classe Date
Floraison_sp1$date <- as.Date(Floraison_sp1$date)

# Sur les 4 ans
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_sp1 <- ccf(Floraison_sp1$Nb_Flo, climat$Rain)
png("correlation_sp1_full.png", width = 800, height = 600)
plot(correlation_sp1)
dev.off()

# Pour 2021
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_sp1_2021 <- ccf(Flo_sp1_2021$Nb_Flo, climat_2021$Rain)
png("correlation_sp1_2021.png", width = 800, height = 600)
plot(correlation_sp1_2021)
dev.off()

# Pour 2022
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_sp1_2022 <- ccf(Flo_sp1_2022$Nb_Flo, climat_2022$Rain)
png("correlation_sp1_2022.png", width = 800, height = 600)
plot(correlation_sp1_2022)
dev.off()

# Pour 2023
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_sp1_2023 <- ccf(Flo_sp1_2023$Nb_Flo, climat_2023$Rain)
png("correlation_sp1_2023.png", width = 800, height = 600)
plot(correlation_sp1_2023)
dev.off()

## Calculer la corrélation croisée entre Nb_Flo et Rain pour V.americana ##

# Convertir la colonne date en classe Date
Floraison_ame$date <- as.Date(Floraison_ame$date)

# Sur les 4 ans
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_am <- ccf(Floraison_ame$Nb_Flo, climat$Rain)
png("correlation_am_full.png", width = 800, height = 600)
plot(correlation_am)
dev.off()

# Pour 2023
par(mar = c(3, 2, 2, 1) + 0.1)
correlation_ame_2023 <- ccf(Flo_ame_2023$Nb_Flo, climat_2023$Rain)
png("correlation_ame_2023.png", width = 800, height = 600)
plot(correlation_ame_2023)
dev.off()



