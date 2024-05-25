# Titre : Analyses conjointe des variables climatiques et phénologiques
# Auteur : Tondra Typhaine
# Date de creation : 22/05/2024

# Packages
pacman::p_load ("tidyverse","plotly","strucchange","timeSeries","lubridate","bfast", "data.table",
                "ggfortify", "zoo", "readxl", "cluster", "stringr", "bookdown",
                "ggpubr", "kableExtra", "tibbletime", "pracma", "imputeTS",
                "TraMineR", "clValid", "FactoMineR", "factoextra", "dunn.test", "ggrepel")

pacman::p_load("tidyverse","lubridate","RColorBrewer","pracma", "vioplot","lmtest","zoo")


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
  filter(!is.na(Rain)) %>% 
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



### FLORAISON ET PLUVIOMETRIE ###

## Symphonia globulifera ##

# Proportions de floraison par jour pour S.globulifera #

LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Symphonia_globulifera",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_globu

# Selection de la colonne prop (proportion de phenophase) issue des donnees de signaux de floraison
data_signal_globu %>% 
  select(prop) %>% 
  pull() ->
  signal_globu

# Max de floraison par annee
data_signal_globu %>% 
  filter(Year == 2020) %>% 
  print() ->
  prop2020_glb

maxprop2020_glb_X <- which.max(prop2020_glb$prop)
maxprop2020_glb_Y <- max(prop2020_glb$prop)
minprop2020_glb_X <- which.min(prop2020_glb$prop)

data_signal_globu %>% 
  filter(Year == 2021) %>% 
  print() ->
  prop2021_glb

maxprop2021_glb_X <- which.max(prop2021_glb$prop)
maxprop2021_glb_Y <- max(prop2021_glb$prop)
minprop2021_glb_X <- which.min(prop2021_glb$prop)
minprop2021_glb_Y <- min(prop2021_glb$prop)

data_signal_globu %>% 
  filter(Year == 2022) %>% 
  print() ->
  prop2022_glb

maxprop2022_glb_X <- which.max(prop2022_glb$prop)
maxprop2022_glb_Y <- max(prop2022_glb$prop)
minprop2022_glb_X <- which.min(prop2022_glb$prop)
minprop2022_glb_Y <- min(prop2022_glb$prop)

data_signal_globu %>% 
  filter(Year == 2023) %>% 
  print() ->
  prop2023_glb

maxprop2023_glb_X <- which.max(prop2023_glb$prop)
maxprop2023_glb_Y <- max(prop2023_glb$prop)
minprop2023_glb_X <- which.min(prop2023_glb$prop)
minprop2023_glb_Y <- min(prop2023_glb$prop)

data_signal_globu %>% 
  filter(Year == 2024) %>% 
  print() ->
  prop2024_glb

maxprop2024_glb_X <- which.max(prop2024_glb$prop)
maxprop2024_glb_Y <- max(prop2024_glb$prop)
minprop2024_glb_X <- which.min(prop2024_glb$prop)
minprop2024_glb_Y <- min(prop2024_glb$prop)

# Pluviometrie cumulee #

# Full data pour chaque jour du suivi
dataB_resume %>% 
  mutate(`Temp(55)` = if_else(is.na(`Temp(55)`), 26,`Temp(55)`)) %>% 
  select(Year, Month, date, Rain,`Temp(55)`) %>% 
  group_by(Year, Month, date) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  print() ->
  Rain

sum(is.na(Rain$Rain))

# Calcul du cumule de pluviometrie tous les 15 j

Rain$Cumule_15J <- rollapply(Rain$Rain, width = 15, FUN = sum, fill = NA, align = "right")

# Calcul du cumule de pluviometrie tous les 30 j

Rain$Cumule_30J <- rollapply(Rain$Rain, width = 30, FUN = sum, fill = NA, align = "right")

# Verifications 
sum(Rain$Rain[1:15])
sum(Rain$Rain[2:16])
sum(Rain$Rain[3:17])

# Maximum et minimum de pluviometrie cumulee 15 j par annee 
Rain %>% 
  filter(Year == 2020) %>% 
  select(date, Cumule_15J) %>% 
  print() ->
  Cumule_15J_2020

max_15J__Y <- max(Cumule_15J_2020$Cumule_15J,na.rm = TRUE)
max_15J__X <- which.max(Cumule_15J_2020$Cumule_15J)
Cumule_15J_2020[117,]

min_15J_2020_Y <- min(Cumule_15J_2020$Cumule_15J,na.rm = TRUE)
min_15J_2020_X <- which.min(Cumule_15J_2020$Cumule_15J)
Cumule_15J_2020[min_15J_2020_X,]


Rain %>% 
  filter(Year == 2021) %>% 
  select(date,Cumule_15J) %>% 
  print() ->
  Cumule_15J_2021

max_15J_2021_Y <- max(Cumule_15J_2021$Cumule_15J,na.rm = TRUE)
max_15J_2021_X <- which.max(Cumule_15J_2021$Cumule_15J)
Cumule_15J_2021[148,]

min_15J_2021_Y <- min(Cumule_15J_2021$Cumule_15J,na.rm = TRUE)
min_15J_2021_X <- which.min(Cumule_15J_2021$Cumule_15J)
Cumule_15J_2021[min_15J_2021_X,]

Rain %>% 
  filter(Year == 2022) %>% 
  select(date,Cumule_15J) %>% 
  print() ->
  Cumule_15J_2022

max_15J_2022_Y <- max(Cumule_15J_2022$Cumule_15J,na.rm = TRUE)
max_15J_2022_X <- which.max(Cumule_15J_2022$Cumule_15J)
Cumule_15J_2022[58,]

min_15J_2022_Y <- min(Cumule_15J_2022$Cumule_15J,na.rm = TRUE)
min_15J_2022_X <- which.min(Cumule_15J_2022$Cumule_15J)
Cumule_15J_2022[min_15J_2022_X,]

Rain %>% 
  filter(Year == 2023) %>% 
  select(date,Cumule_15J) %>% 
  print() ->
  Cumule_15J_2023

max_15J_2023_Y <- max(Cumule_15J_2023$Cumule_15J,na.rm = TRUE)
max_15J_2023_X <- which.max(Cumule_15J_2023$Cumule_15J)
Cumule_15J_2023[57,]

min_15J_2023_Y <- min(Cumule_15J_2023$Cumule_15J,na.rm = TRUE)
min_15J_2023_X <- which.min(Cumule_15J_2023$Cumule_15J)
Cumule_15J_2023[min_15J_2023_X,]

Rain %>% 
  filter(Year == 2024) %>% 
  select(date,Cumule_15J) %>% 
  print() ->
  Cumule_15J_2024

max_15J_2024_Y <- max(Cumule_15J_2024$Cumule_15J,na.rm = TRUE)
max_15J_2024_X <- which.max(Cumule_15J_2024$Cumule_15J)
Cumule_15J_2024[1,]

min_15J_2024_Y <- min(Cumule_15J_2024$Cumule_15J,na.rm = TRUE)
min_15J_2024_X <- which.min(Cumule_15J_2024$Cumule_15J)
Cumule_15J_2024[min_15J_2024_X,]


# Maximum et minimum de pluviometrie cumulee 30 j par annee 
Rain %>% 
  filter(Year == 2020) %>% 
  select(date, Cumule_30J) %>% 
  print() ->
  Cumule_30J_2020

max30J_2020_Y <- max(Cumule_30J_2020$Cumule_30J,na.rm = TRUE)
max30J_2020_X <- which.max(Cumule_30J_2020$Cumule_30J)
Cumule_30J_2020[132,]

min30J_2020_Y <- min(Cumule_30J_2020$Cumule_30J,na.rm = TRUE)
min30J_2020_X <- which.min(Cumule_30J_2020$Cumule_30J)
Cumule_30J_2020[282,]


Rain %>% 
  filter(Year == 2021) %>% 
  select(date,Cumule_30J) %>% 
  print() ->
  Cumule_30J_2021

max30J_2021_Y <- max(Cumule_30J_2021$Cumule_30J,na.rm = TRUE)
max30J_2021_X <- which.max(Cumule_30J_2021$Cumule_30J)
Cumule_30J_2021[162,]

min30J_2021_Y <- min(Cumule_30J_2021$Cumule_30J,na.rm = TRUE)
min30J_2021_X <- which.min(Cumule_30J_2021$Cumule_30J)
Cumule_30J_2021[305,]

Rain %>% 
  filter(Year == 2022) %>% 
  select(date,Cumule_30J) %>% 
  print() ->
  Cumule_30J_2022

max30J_2022_Y <- max(Cumule_30J_2022$Cumule_30J,na.rm = TRUE)
max30J_2022_X <- which.max(Cumule_30J_2022$Cumule_30J)
Cumule_30J_2022[63,]

min30J_2022_Y <- min(Cumule_30J_2022$Cumule_30J,na.rm = TRUE)
min30J_2022_X <- which.min(Cumule_30J_2022$Cumule_30J)
Cumule_30J_2022[263,]

Rain %>% 
  filter(Year == 2023) %>% 
  select(date,Cumule_30J) %>% 
  print() ->
  Cumule_30J_2023

max30J_2023_Y <- max(Cumule_30J_2023$Cumule_30J,na.rm = TRUE)
max30J_2023_X <- which.max(Cumule_30J_2023$Cumule_30J)
Cumule_30J_2023[57,]

min30J_2023_Y <- min(Cumule_30J_2023$Cumule_30J,na.rm = TRUE)
min30J_2023_X <- which.min(Cumule_30J_2023$Cumule_30J)
Cumule_30J_2023[206,]

Rain %>% 
  filter(Year == 2024) %>% 
  select(date,Cumule_30J) %>% 
  print() ->
  Cumule_30J_2024

max30J_2024_Y <- max(Cumule_30J_2024$Cumule_30J,na.rm = TRUE)
max30J_2024_X <- which.max(Cumule_30J_2024$Cumule_30J)
Cumule_30J_2024[2,]

min30J_2024_Y <- min(Cumule_30J_2024$Cumule_30J,na.rm = TRUE)
min30J_2024_X <- which.min(Cumule_30J_2024$Cumule_30J)
Cumule_30J_2024[47,]


display.brewer.all(type = "div")
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
brewer.pal(n = 4, name = "Set1")


# Graphique pluviometrie cumulee 15J et pourcentage floraison #

x_date_4ans <- scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                               "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                               "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
                                               "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                               "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                               "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
                                               "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                               "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                               "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01",
                                               "2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                               "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                               "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01",
                                               "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                               "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                               "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
                            date_labels = "%Y-%m-%d") 

ggplot() + 
  #geom_point(data = data_signal_globu, aes(x = date, y = signal_globu, color = "% de floraison")) +
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  
  # Min et max pour l'annee 2020
  geom_vline(xintercept = Cumule_15J_2020$date[max_15J__X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_15J_2020$date[min_15J_2020_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_15J_2020, aes(x = Cumule_15J_2020$date[max_15J__X], y = max_15J__Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_15J_2020, aes(x = Cumule_15J_2020$date[min_15J_2020_X], y = min_15J_2020_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_15J_2020$date[max_15J__X], y = max_15J__Y+20, 
           label = max_15J__Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_15J_2020$date[min_15J_2020_X], y = min_15J_2020_Y-20, 
           label = min_15J_2020_Y, colour = "#4DAF4A",fontface = "bold", size = 3.5) +
  
  # Min et max pour l'annee 2021
  geom_vline(xintercept = Cumule_15J_2021$date[max_15J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_15J_2021$date[min_15J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_15J_2021, aes(x = Cumule_15J_2021$date[max_15J_2021_X], y = max_15J_2021_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_15J_2021, aes(x = Cumule_15J_2021$date[min_15J_2021_X], y = min_15J_2021_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_15J_2021$date[max_15J_2021_X], y = max_15J_2021_Y+20, 
           label = max_15J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_15J_2021$date[min_15J_2021_X], y = min_15J_2021_Y-20, 
           label = min_15J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2021_glb$date[maxprop2021_glb_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2021_glb$date[minprop2021_glb_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2021_glb, aes(x = prop2021_glb$date[maxprop2021_glb_X], y = maxprop2021_glb_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2021_glb, aes(x = prop2021_glb$date[minprop2021_glb_X], y = minprop2021_glb_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2021_glb$date[maxprop2021_glb_X], y = maxprop2021_glb_Y+20, 
           label = round(maxprop2021_glb_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2021_glb$date[minprop2021_glb_X], y = minprop2021_glb_Y-20, 
           label = minprop2021_glb_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +

  
  # Min et max pour l'annee 2022
  geom_vline(xintercept = Cumule_15J_2022$date[max_15J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_15J_2022$date[min_15J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_15J_2022, aes(x = Cumule_15J_2022$date[max_15J_2022_X], y = max_15J_2022_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_15J_2022, aes(x = Cumule_15J_2022$date[min_15J_2022_X], y = min_15J_2022_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_15J_2022$date[max_15J_2022_X], y = max_15J_2022_Y+20, 
           label = max_15J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_15J_2022$date[min_15J_2022_X], y = min_15J_2022_Y-20, 
           label = min_15J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2022_glb$date[maxprop2022_glb_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2022_glb$date[minprop2022_glb_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2022_glb, aes(x = prop2022_glb$date[maxprop2022_glb_X], y = maxprop2022_glb_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2022_glb, aes(x = prop2022_glb$date[minprop2022_glb_X], y = minprop2022_glb_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2022_glb$date[maxprop2022_glb_X], y = maxprop2022_glb_Y+20, 
           label = round(maxprop2022_glb_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2022_glb$date[minprop2022_glb_X], y = minprop2022_glb_Y-20, 
           label = minprop2022_glb_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +

  
  # Min et max pour l'annee 2023
  geom_vline(xintercept = Cumule_15J_2023$date[max_15J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_15J_2023$date[min_15J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_15J_2023, aes(x = Cumule_15J_2023$date[max_15J_2023_X], y = max_15J_2023_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_15J_2023, aes(x = Cumule_15J_2023$date[min_15J_2023_X], y = min_15J_2023_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_15J_2023$date[max_15J_2023_X], y = max_15J_2023_Y+20, 
           label = max_15J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_15J_2023$date[min_15J_2023_X], y = min_15J_2023_Y-20, 
           label = min_15J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2023_glb$date[maxprop2023_glb_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2023_glb$date[minprop2023_glb_X], col = "grey40", linetype = "dashed") +
  geom_point(data = prop2023_glb, aes(x = prop2023_glb$date[maxprop2023_glb_X], y = maxprop2023_glb_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2023_glb, aes(x = prop2023_glb$date[minprop2023_glb_X], y = minprop2023_glb_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2023_glb$date[maxprop2023_glb_X], y = maxprop2023_glb_Y+20, 
           label = round(maxprop2023_glb_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2023_glb$date[minprop2023_glb_X], y = minprop2023_glb_Y-20, 
           label = minprop2023_glb_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
    # Min et max pour l'annee 2024
  geom_vline(xintercept = Cumule_15J_2024$date[max_15J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_15J_2024$date[min_15J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_15J_2024, aes(x = Cumule_15J_2024$date[max_15J_2024_X], y = max_15J_2024_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_15J_2024, aes(x = Cumule_15J_2024$date[min_15J_2024_X], y = min_15J_2024_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_15J_2024$date[max_15J_2024_X], y = max_15J_2024_Y+20, 
           label = round(max_15J_2024_Y, digits = 2), colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_15J_2024$date[min_15J_2024_X], y = min_15J_2024_Y-20, 
           label = min_15J_2024_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +

  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )




# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  #geom_point(data = data_signal_globu, aes(x = date, y = signal_globu, color = "% de floraison")) +
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_30J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = Cumule_30J, color = "Pluviométrie cumulée")) +
  
  # Min et max pour l'annee 2020
  geom_vline(xintercept = Cumule_30J_2020$date[max30J_2020_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2020$date[min30J_2020_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2020, aes(x = Cumule_30J_2020$date[max30J_2020_X], y = max30J_2020_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2020, aes(x = Cumule_30J_2020$date[min30J_2020_X], y = min30J_2020_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2020$date[max30J_2020_X], y = max30J_2020_Y+20, 
           label = max30J_2020_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2020$date[min30J_2020_X], y = min30J_2020_Y-20, 
           label = min30J_2020_Y, colour = "#4DAF4A",fontface = "bold", size = 3.5) +
  
  # Min et max pour l'annee 2021
  geom_vline(xintercept = Cumule_30J_2021$date[max30J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2021$date[min30J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2021, aes(x = Cumule_30J_2021$date[max30J_2021_X], y = max30J_2021_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2021, aes(x = Cumule_30J_2021$date[min30J_2021_X], y = min30J_2021_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2021$date[max30J_2021_X], y = max30J_2021_Y+20, 
           label = max30J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2021$date[min30J_2021_X], y = min30J_2021_Y-20, 
           label = min30J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2021_glb$date[maxprop2021_glb_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2021_glb$date[minprop2021_glb_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2021_glb, aes(x = prop2021_glb$date[maxprop2021_glb_X], y = maxprop2021_glb_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2021_glb, aes(x = prop2021_glb$date[minprop2021_glb_X], y = minprop2021_glb_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2021_glb$date[maxprop2021_glb_X], y = maxprop2021_glb_Y+20, 
           label = round(maxprop2021_glb_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2021_glb$date[minprop2021_glb_X], y = minprop2021_glb_Y-20, 
           label = minprop2021_glb_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2022
  geom_vline(xintercept = Cumule_30J_2022$date[max30J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2022$date[min30J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2022, aes(x = Cumule_30J_2022$date[max30J_2022_X], y = max30J_2022_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2022, aes(x = Cumule_30J_2022$date[min30J_2022_X], y = min30J_2022_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2022$date[max30J_2022_X], y = max30J_2022_Y+20, 
           label = max30J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2022$date[min30J_2022_X], y = min30J_2022_Y-20, 
           label = min30J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2022_glb$date[maxprop2022_glb_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2022_glb$date[minprop2022_glb_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2022_glb, aes(x = prop2022_glb$date[maxprop2022_glb_X], y = maxprop2022_glb_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2022_glb, aes(x = prop2022_glb$date[minprop2022_glb_X], y = minprop2022_glb_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2022_glb$date[maxprop2022_glb_X], y = maxprop2022_glb_Y+20, 
           label = round(maxprop2022_glb_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2022_glb$date[minprop2022_glb_X], y = minprop2022_glb_Y-20, 
           label = minprop2022_glb_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2023
  geom_vline(xintercept = Cumule_30J_2023$date[max30J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2023$date[min30J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2023, aes(x = Cumule_30J_2023$date[max30J_2023_X], y = max30J_2023_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2023, aes(x = Cumule_30J_2023$date[min30J_2023_X], y = min30J_2023_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2023$date[max30J_2023_X], y = max30J_2023_Y+20, 
           label = max30J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2023$date[min30J_2023_X], y = min30J_2023_Y-20, 
           label = min30J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2023_glb$date[maxprop2023_glb_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2023_glb$date[minprop2023_glb_X], col = "grey40", linetype = "dashed") +
  geom_point(data = prop2023_glb, aes(x = prop2023_glb$date[maxprop2023_glb_X], y = maxprop2023_glb_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2023_glb, aes(x = prop2023_glb$date[minprop2023_glb_X], y = minprop2023_glb_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2023_glb$date[maxprop2023_glb_X], y = maxprop2023_glb_Y+20, 
           label = round(maxprop2023_glb_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2023_glb$date[minprop2023_glb_X], y = minprop2023_glb_Y-20, 
           label = minprop2023_glb_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2024
  geom_vline(xintercept = Cumule_30J_2024$date[max30J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2024$date[min30J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2024, aes(x = Cumule_30J_2024$date[max30J_2024_X], y = max30J_2024_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2024, aes(x = Cumule_30J_2024$date[min30J_2024_X], y = min30J_2024_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2024$date[max30J_2024_X], y = max30J_2024_Y+20, 
           label = round(max30J_2024_Y, digits = 2), colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2024$date[min30J_2024_X], y = min30J_2024_Y-20, 
           label = min30J_2024_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +

  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )















## Symphonia sp1 ##

# Proportions de floraison par jour pour S.globulifera #

LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Symphonia_sp.1",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_sp1

# Selection de la colonne prop (proportion de phenophase) issue des donnees de signaux de floraison
data_signal_sp1 %>% 
  select(prop) %>% 
  pull() ->
  signal_sp1

# Max de floraison par annee
data_signal_sp1 %>% 
  filter(Year == 2020) %>% 
  print() ->
  prop2020_sp1

maxprop2020_sp1_X <- which.max(prop2020_sp1$prop)
maxprop2020_sp1_Y <- max(prop2020_sp1$prop)
minprop2020_sp1_X <- which.min(prop2020_sp1$prop)

data_signal_sp1 %>% 
  filter(Year == 2021) %>% 
  print() ->
  prop2021_sp1

maxprop2021_sp1_X <- which.max(prop2021_sp1$prop)
maxprop2021_sp1_Y <- max(prop2021_sp1$prop)
minprop2021_sp1_X <- which.min(prop2021_sp1$prop)
minprop2021_sp1_Y <- min(prop2021_sp1$prop)

data_signal_sp1 %>% 
  filter(Year == 2022) %>% 
  print() ->
  prop2022_sp1

maxprop2022_sp1_X <- which.max(prop2022_sp1$prop)
maxprop2022_sp1_Y <- max(prop2022_sp1$prop)
minprop2022_sp1_X <- which.min(prop2022_sp1$prop)
minprop2022_sp1_Y <- min(prop2022_sp1$prop)

data_signal_sp1 %>% 
  filter(Year == 2023) %>% 
  print() ->
  prop2023_sp1

maxprop2023_sp1_X <- which.max(prop2023_sp1$prop)
maxprop2023_sp1_Y <- max(prop2023_sp1$prop)
minprop2023_sp1_X <- which.min(prop2023_sp1$prop)
minprop2023_sp1_Y <- min(prop2023_sp1$prop)

data_signal_sp1 %>% 
  filter(Year == 2024) %>% 
  print() ->
  prop2024_sp1

maxprop2024_sp1_X <- which.max(prop2024_sp1$prop)
maxprop2024_sp1_Y <- max(prop2024_sp1$prop)
minprop2024_sp1_X <- which.min(prop2024_sp1$prop)
minprop2024_sp1_Y <- min(prop2024_sp1$prop)

display.brewer.all(type = "div")
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
brewer.pal(n = 4, name = "Set1")


# Graphique pluviometrie cumulee 15J et pourcentage floraison #

x_date_4ans <- scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                               "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                               "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
                                               "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                               "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                               "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
                                               "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                               "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                               "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01",
                                               "2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                               "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                               "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01",
                                               "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                               "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                               "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
                            date_labels = "%Y-%m-%d") 

ggplot() + 
  #geom_point(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "% de floraison")) +
  geom_line(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  
  # Min et max pour l'annee 2020
  geom_vline(xintercept = Cumule_30J_2020$date[max_15J__X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2020$date[min_15J_2020_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2020, aes(x = Cumule_30J_2020$date[max_15J__X], y = max_15J__Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2020, aes(x = Cumule_30J_2020$date[min_15J_2020_X], y = min_15J_2020_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2020$date[max_15J__X], y = max_15J__Y+20, 
           label = max_15J__Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2020$date[min_15J_2020_X], y = min_15J_2020_Y-20, 
           label = min_15J_2020_Y, colour = "#4DAF4A",fontface = "bold", size = 3.5) +
  
  # Min et max pour l'annee 2021
  geom_vline(xintercept = Cumule_30J_2021$date[max_15J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2021$date[min_15J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2021, aes(x = Cumule_30J_2021$date[max_15J_2021_X], y = max_15J_2021_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2021, aes(x = Cumule_30J_2021$date[min_15J_2021_X], y = min_15J_2021_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2021$date[max_15J_2021_X], y = max_15J_2021_Y+20, 
           label = max_15J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2021$date[min_15J_2021_X], y = min_15J_2021_Y-20, 
           label = min_15J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2021_sp1$date[maxprop2021_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2021_sp1$date[minprop2021_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2021_sp1, aes(x = prop2021_sp1$date[maxprop2021_sp1_X], y = maxprop2021_sp1_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2021_sp1, aes(x = prop2021_sp1$date[minprop2021_sp1_X], y = minprop2021_sp1_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2021_sp1$date[maxprop2021_sp1_X], y = maxprop2021_sp1_Y+20, 
           label = round(maxprop2021_sp1_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2021_sp1$date[minprop2021_sp1_X], y = minprop2021_sp1_Y-20, 
           label = minprop2021_sp1_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2022
  geom_vline(xintercept = Cumule_30J_2022$date[max_15J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2022$date[min_15J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2022, aes(x = Cumule_30J_2022$date[max_15J_2022_X], y = max_15J_2022_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2022, aes(x = Cumule_30J_2022$date[min_15J_2022_X], y = min_15J_2022_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2022$date[max_15J_2022_X], y = max_15J_2022_Y+20, 
           label = max_15J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2022$date[min_15J_2022_X], y = min_15J_2022_Y-20, 
           label = min_15J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2022_sp1$date[maxprop2022_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2022_sp1$date[minprop2022_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2022_sp1, aes(x = prop2022_sp1$date[maxprop2022_sp1_X], y = maxprop2022_sp1_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2022_sp1, aes(x = prop2022_sp1$date[minprop2022_sp1_X], y = minprop2022_sp1_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2022_sp1$date[maxprop2022_sp1_X], y = maxprop2022_sp1_Y+20, 
           label = round(maxprop2022_sp1_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2022_sp1$date[minprop2022_sp1_X], y = minprop2022_sp1_Y-20, 
           label = minprop2022_sp1_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2023
  geom_vline(xintercept = Cumule_30J_2023$date[max_15J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2023$date[min_15J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2023, aes(x = Cumule_30J_2023$date[max_15J_2023_X], y = max_15J_2023_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2023, aes(x = Cumule_30J_2023$date[min_15J_2023_X], y = min_15J_2023_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2023$date[max_15J_2023_X], y = max_15J_2023_Y+20, 
           label = max_15J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2023$date[min_15J_2023_X], y = min_15J_2023_Y-20, 
           label = min_15J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2023_sp1$date[maxprop2023_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2023_sp1$date[minprop2023_sp1_X], col = "grey40", linetype = "dashed") +
  geom_point(data = prop2023_sp1, aes(x = prop2023_sp1$date[maxprop2023_sp1_X], y = maxprop2023_sp1_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2023_sp1, aes(x = prop2023_sp1$date[minprop2023_sp1_X], y = minprop2023_sp1_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2023_sp1$date[maxprop2023_sp1_X], y = maxprop2023_sp1_Y+20, 
           label = round(maxprop2023_sp1_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2023_sp1$date[minprop2023_sp1_X], y = minprop2023_sp1_Y-20, 
           label = minprop2023_sp1_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2024
  geom_vline(xintercept = Cumule_30J_2024$date[max_15J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2024$date[min_15J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2024, aes(x = Cumule_30J_2024$date[max_15J_2024_X], y = max_15J_2024_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2024, aes(x = Cumule_30J_2024$date[min_15J_2024_X], y = min_15J_2024_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2024$date[max_15J_2024_X], y = max_15J_2024_Y+20, 
           label = round(max_15J_2024_Y, digits = 2), colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2024$date[min_15J_2024_X], y = min_15J_2024_Y-20, 
           label = min_15J_2024_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2024_sp1$date[maxprop2024_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2024_sp1$date[minprop2024_sp1_X], col = "grey40", linetype = "dashed") + 
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )




# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  #geom_point(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "% de floraison")) +
  geom_line(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_30J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = Cumule_30J, color = "Pluviométrie cumulée")) +
  
  # Min et max pour l'annee 2020
  geom_vline(xintercept = Cumule_30J_2020$date[max30J_2020_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2020$date[min30J_2020_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2020, aes(x = Cumule_30J_2020$date[max30J_2020_X], y = max30J_2020_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2020, aes(x = Cumule_30J_2020$date[min30J_2020_X], y = min30J_2020_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2020$date[max30J_2020_X], y = max30J_2020_Y+20, 
           label = max30J_2020_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2020$date[min30J_2020_X], y = min30J_2020_Y-20, 
           label = min30J_2020_Y, colour = "#4DAF4A",fontface = "bold", size = 3.5) +
  
  # Min et max pour l'annee 2021
  geom_vline(xintercept = Cumule_30J_2021$date[max30J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2021$date[min30J_2021_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2021, aes(x = Cumule_30J_2021$date[max30J_2021_X], y = max30J_2021_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2021, aes(x = Cumule_30J_2021$date[min30J_2021_X], y = min30J_2021_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2021$date[max30J_2021_X], y = max30J_2021_Y+20, 
           label = max30J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2021$date[min30J_2021_X], y = min30J_2021_Y-20, 
           label = min30J_2021_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2021_sp1$date[maxprop2021_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2021_sp1$date[minprop2021_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2021_sp1, aes(x = prop2021_sp1$date[maxprop2021_sp1_X], y = maxprop2021_sp1_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2021_sp1, aes(x = prop2021_sp1$date[minprop2021_sp1_X], y = minprop2021_sp1_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2021_sp1$date[maxprop2021_sp1_X], y = maxprop2021_sp1_Y+20, 
           label = round(maxprop2021_sp1_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2021_sp1$date[minprop2021_sp1_X], y = minprop2021_sp1_Y-20, 
           label = minprop2021_sp1_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2022
  geom_vline(xintercept = Cumule_30J_2022$date[max30J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2022$date[min30J_2022_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2022, aes(x = Cumule_30J_2022$date[max30J_2022_X], y = max30J_2022_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2022, aes(x = Cumule_30J_2022$date[min30J_2022_X], y = min30J_2022_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2022$date[max30J_2022_X], y = max30J_2022_Y+20, 
           label = max30J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2022$date[min30J_2022_X], y = min30J_2022_Y-20, 
           label = min30J_2022_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2022_sp1$date[maxprop2022_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2022_sp1$date[minprop2022_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = prop2022_sp1, aes(x = prop2022_sp1$date[maxprop2022_sp1_X], y = maxprop2022_sp1_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2022_sp1, aes(x = prop2022_sp1$date[minprop2022_sp1_X], y = minprop2022_sp1_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2022_sp1$date[maxprop2022_sp1_X], y = maxprop2022_sp1_Y+20, 
           label = round(maxprop2022_sp1_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2022_sp1$date[minprop2022_sp1_X], y = minprop2022_sp1_Y-20, 
           label = minprop2022_sp1_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +
  
  
  # Min et max pour l'annee 2023
  geom_vline(xintercept = Cumule_30J_2023$date[max30J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2023$date[min30J_2023_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2023, aes(x = Cumule_30J_2023$date[max30J_2023_X], y = max30J_2023_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2023, aes(x = Cumule_30J_2023$date[min30J_2023_X], y = min30J_2023_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2023$date[max30J_2023_X], y = max30J_2023_Y+20, 
           label = max30J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2023$date[min30J_2023_X], y = min30J_2023_Y-20, 
           label = min30J_2023_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2023_sp1$date[maxprop2023_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2023_sp1$date[minprop2023_sp1_X], col = "grey40", linetype = "dashed") +
  geom_point(data = prop2023_sp1, aes(x = prop2023_sp1$date[maxprop2023_sp1_X], y = maxprop2023_sp1_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = prop2023_sp1, aes(x = prop2023_sp1$date[minprop2023_sp1_X], y = minprop2023_sp1_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = prop2023_sp1$date[maxprop2023_sp1_X], y = maxprop2023_sp1_Y+20, 
           label = round(maxprop2023_sp1_Y, digits = 1), colour = "#984EA3", fontface = "bold", size = 3.5) +
  annotate("text", x = prop2023_sp1$date[minprop2023_sp1_X], y = minprop2023_sp1_Y-20, 
           label = minprop2023_sp1_Y, colour = "#984EA3", fontface = "bold", size = 3.5) +

  # Min et max pour l'annee 2024
  geom_vline(xintercept = Cumule_30J_2024$date[max30J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = Cumule_30J_2024$date[min30J_2024_X], col = "grey40", linetype = "dashed") + 
  geom_point(data = Cumule_30J_2024, aes(x = Cumule_30J_2024$date[max30J_2024_X], y = max30J_2024_Y), 
             colour = "black", size = 1.3) + 
  geom_point(data = Cumule_30J_2024, aes(x = Cumule_30J_2024$date[min30J_2024_X], y = min30J_2024_Y),
             colour = "black", size = 1.3) + 
  annotate("text", x = Cumule_30J_2024$date[max30J_2024_X], y = max30J_2024_Y+20, 
           label = round(max30J_2024_Y, digits = 2), colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  annotate("text", x = Cumule_30J_2024$date[min30J_2024_X], y = min30J_2024_Y-20, 
           label = min30J_2024_Y, colour = "#4DAF4A", fontface = "bold", size = 3.5) +
  
  geom_vline(xintercept = prop2024_sp1$date[maxprop2024_sp1_X], col = "grey40", linetype = "dashed") + 
  geom_vline(xintercept = prop2024_sp1$date[minprop2024_sp1_X], col = "grey40", linetype = "dashed") + 
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )



