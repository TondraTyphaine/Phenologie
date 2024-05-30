# Titre : Analyses conjointe des variables climatiques et phénologiques
# Auteur : Tondra Typhaine
# Date de creation : 22/05/2024

# Packages
pacman::p_load ("tidyverse","plotly","strucchange","timeSeries","lubridate","bfast", "data.table",
                "ggfortify", "zoo", "readxl", "cluster", "stringr", "bookdown",
                "ggpubr", "kableExtra", "tibbletime", "pracma", "imputeTS",
                "TraMineR", "clValid", "FactoMineR", "factoextra", "dunn.test", "ggrepel")

pacman::p_load("RColorBrewer","vioplot","lmtest","zoo", "ade4", "psych")
install.packages("tidyverse")
library(tidyverse)

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
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            vpd55= mean(vpd55),
            Rain= sum(Rain),
            ETP = sum(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm)) %>% 
  print( ) ->
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

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Symphonia_globulifera",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_globu

# Selection de la colonne proportion de floraison 
data_signal_globu %>% 
  select(prop) %>% 
  pull() ->
  signal_globu

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moving_average(data_signal_globu %>% 
                 select(prop) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) ->
  moyenne_mobile_globu

# Dates 
data_signal_globu %>% 
  select(date) %>% 
  pull() ->
  dates_globu

# Maximum des pics de floraison
dates_max_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups =1)[,2])

# Debut des pics
dates_begin_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,3])

# Fin des pics
dates_end_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_globu = findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_globu = signal_globu[dates_max_globu]


# Pluviometrie cumulee #

# Full data pour chaque jour du suivi
dataB_resume %>% 
  filter(!is.na(Rain)) %>% 
  mutate(`Temp(55)` = if_else(is.na(`Temp(55)`), 26,`Temp(55)`)) %>% 
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

Rain %>% 
  mutate(Cumule_15J = if_else(is.na(Cumule_15J), 0, Cumule_15J)) %>% 
  mutate(Cumule_30J = if_else(is.na(Cumule_30J), 0, Cumule_30J)) %>% 
  print() ->
  Rain_bis

# Maximum et minimum de pluviometrie cumulee 15 j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(Rain_bis %>% 
                 select(Cumule_15J) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) %>% 
  print() ->
  moyenne_mobile_15J

# Dates 
Rain_bis %>% 
  select(date) %>% 
  pull() ->
  dates_rain

# Maximum des pics de floraison
dates_max_15J = sort(findpeaks(moyenne_mobile_15J,minpeakheight  = 430,nups =1)[,2])

# Debut des pics
dates_begin_15J = sort(findpeaks(moyenne_mobile_15J,minpeakheight  = 430,nups = 1)[,3])

# Fin des pics
dates_end_15J = sort(findpeaks(moyenne_mobile_15J,minpeakheight  = 430,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_15J = findpeaks(moyenne_mobile_15J,minpeakheight  = 430,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_15J = Rain_bis$Cumule_15J[dates_max_15J]



# Maximum et minimum de pluviometrie cumulee 30 j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(Rain_bis %>% 
                 select(Cumule_30J) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) %>% 
  print() ->
  moyenne_mobile_30J

# Dates 
dates_rain

# Maximum des pics de floraison
dates_max_30J = sort(findpeaks(moyenne_mobile_30J,minpeakheight  = 700,nups = 1)[,2])
indices <- c(1, 5, 8, 9)
dates_max_30J <- dates_max_30J[indices]

# Debut des pics
dates_begin_30J = sort(findpeaks(moyenne_mobile_30J,minpeakheight  = 700,nups = 1)[,3])

# Fin des pics
dates_end_30J = sort(findpeaks(moyenne_mobile_30J,minpeakheight  = 700,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_30J = findpeaks(moyenne_mobile_30J,minpeakheight  = 700,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_30J = Rain_bis$Cumule_30J[dates_max_30J]


# Graphique pluviometrie cumulee 15J et pourcentage floraison #
# Attention : utilisation de la methode de la moyenne mobile sur le signal de floraison

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
  geom_line(data = data_signal_globu, aes(x = date, y = prop, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_globu[dates_max_globu],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_globu[dates_begin_globu],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_globu[dates_end_globu],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_globu[dates_max_globu], 
           y= data_signal_globu$prop[dates_max_globu]+20,label = paste(round(amplitude_real_globu,1),"%"),
           col = "black") +
 
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_15J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_15J], 
           y= moyenne_mobile_15J[dates_max_15J]+20,label = paste(round(amplitude_real_15J,1)),
           col = "black") +
  
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
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_globu[dates_max_globu],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_globu[dates_begin_globu],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_globu[dates_end_globu],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_globu[dates_max_globu], 
           y= data_signal_globu$prop[dates_max_globu]+20,label = paste(round(amplitude_real_globu,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_30J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_30J], 
           y= moyenne_mobile_30J[dates_max_30J]+20,label = paste(round(amplitude_real_30J,1)),
           col = "black") +

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

# Proportions de floraison par jour pour S.p1 #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Symphonia_sp.1",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_sp1

# Selection de la colonne proportion de floraison 
data_signal_sp1 %>% 
  select(prop) %>% 
  pull() ->
  signal_sp1

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moving_average(data_signal_sp1 %>% 
                 select(prop) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) ->
  moyenne_mobile_sp1

# Dates 
data_signal_sp1 %>% 
  select(date) %>% 
  pull() ->
  dates_sp1

# Maximum des pics de floraison
dates_max_sp1 = sort(findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups =1)[,2])

# Debut des pics
dates_begin_sp1 = sort(findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups = 1)[,3])

# Fin des pics
dates_end_sp1 = sort(findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_sp1 = findpeaks(moyenne_mobile_sp1,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_sp1 = signal_sp1[dates_max_sp1]



ggplot() + 
  #geom_point(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "% de floraison")) +
  geom_line(data = data_signal_sp1, aes(x = date, y = prop, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_sp1[dates_max_sp1],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_sp1[dates_end_sp1],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_sp1[dates_max_sp1], 
           y= data_signal_sp1$prop[dates_max_sp1]+20,label = paste(round(amplitude_real_sp1,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_15J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_15J], 
           y= moyenne_mobile_15J[dates_max_15J]+20,label = paste(round(amplitude_real_15J,1)),
           col = "black") +
  
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
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_sp1[dates_max_sp1],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_sp1[dates_end_sp1],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_sp1[dates_max_sp1], 
           y= data_signal_sp1$prop[dates_max_sp1]+20,label = paste(round(amplitude_real_sp1,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_30J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_30J], 
           y= moyenne_mobile_30J[dates_max_30J]+20,label = paste(round(amplitude_real_30J,1)),
           col = "black") +
  
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


## Vouacapoua americana ##

# Proportions de floraison par jour pour V.americana #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Vouacapoua_americana",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_am


# Selection de la colonne proportion de floraison 
data_signal_am %>% 
  select(prop) %>% 
  pull() ->
  signal_am

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moving_average(data_signal_am %>% 
                 select(prop) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) ->
  moyenne_mobile_am

# Dates 
data_signal_am %>% 
  select(date) %>% 
  pull() ->
  dates_am

# Maximum des pics de floraison
dates_max_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups =1)[,2])

# Debut des pics
dates_begin_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 1)[,3])

# Fin des pics
dates_end_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_am = findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_am = signal_am[dates_max_am]


ggplot() + 
  #geom_point(data = data_signal_am, aes(x = date, y = signal_am, color = "% de floraison")) +
  geom_line(data = data_signal_am, aes(x = date, y = prop, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_am[dates_max_am],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_am[dates_begin_am],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_am[dates_end_am],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_am[dates_max_am], 
           y= data_signal_am$prop[dates_max_am]+20,label = paste(round(amplitude_real_am,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_15J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_15J], 
           y= moyenne_mobile_15J[dates_max_15J]+20,label = paste(round(amplitude_real_15J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Vouacapoua americana",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  #geom_point(data = data_signal_am, aes(x = date, y = signal_am, color = "% de floraison")) +
  geom_line(data = data_signal_am, aes(x = date, y = signal_am, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_30J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_am[dates_max_am],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_am[dates_begin_am],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_am[dates_end_am],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_am[dates_max_am], 
           y= data_signal_am$prop[dates_max_am]+20,label = paste(round(amplitude_real_am,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_30J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_30J], 
           y= moyenne_mobile_30J[dates_max_30J]+20,label = paste(round(amplitude_real_30J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Vouacapoua americana",
    x = "Dates",
    color = "Légende"
  )



## Couma guianensis ##

# Proportions de floraison par jour pour V.americana #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Couma_guianensis",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_gui


# Selection de la colonne proportion de floraison 
data_signal_gui %>% 
  select(prop) %>% 
  pull() ->
  signal_gui

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moving_average(data_signal_gui %>% 
                 select(prop) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) ->
  moyenne_mobile_gui

# Dates 
data_signal_gui %>% 
  select(date) %>% 
  pull() ->
  dates_gui

# Maximum des pics de floraison
dates_max_gui = sort(findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups =1)[,2])

# Debut des pics
dates_begin_gui = sort(findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups = 1)[,3])

# Fin des pics
dates_end_gui = sort(findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_gui = findpeaks(moyenne_mobile_gui,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_gui = signal_gui[dates_max_gui]


ggplot() + 
  #geom_point(data = data_signal_gui, aes(x = date, y = signal_gui, color = "% de floraison")) +
  geom_line(data = data_signal_gui, aes(x = date, y = prop, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_gui[dates_max_gui],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_gui[dates_begin_gui],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_gui[dates_end_gui],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_gui[dates_max_gui], 
           y= data_signal_gui$prop[dates_max_gui]+20,label = paste(round(amplitude_real_gui,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_15J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_15J], 
           y= moyenne_mobile_15J[dates_max_15J]+20,label = paste(round(amplitude_real_15J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  #geom_point(data = data_signal_gui, aes(x = date, y = signal_gui, color = "% de floraison")) +
  geom_line(data = data_signal_gui, aes(x = date, y = signal_gui, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_30J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_gui[dates_max_gui],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_gui[dates_begin_gui],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_gui[dates_end_gui],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_gui[dates_max_gui], 
           y= data_signal_gui$prop[dates_max_gui]+20,label = paste(round(amplitude_real_gui,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_30J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_30J], 
           y= moyenne_mobile_30J[dates_max_30J]+20,label = paste(round(amplitude_real_30J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )



## Moronobea coccinea ##

# Proportions de floraison par jour pour V.americana #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Moronobea_coccinea",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_cocci


# Selection de la colonne proportion de floraison 
data_signal_cocci %>% 
  select(prop) %>% 
  pull() ->
  signal_cocci

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moving_average(data_signal_cocci %>% 
                 select(prop) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) ->
  moyenne_mobile_cocci

# Dates 
data_signal_cocci %>% 
  select(date) %>% 
  pull() ->
  dates_cocci

# Maximum des pics de floraison
dates_max_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10,nups =0)[,2])
dates_max_cocci <- dates_max_cocci[1]

# Debut des pics
dates_begin_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10,nups = 0)[,3])

# Fin des pics
dates_end_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10,nups = 0)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_cocci = findpeaks(moyenne_mobile_cocci,minpeakheight  = 10,nups = 0)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_cocci = signal_cocci[dates_max_cocci]


ggplot() + 
  #geom_point(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "% de floraison")) +
  geom_line(data = data_signal_cocci, aes(x = date, y = prop, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_cocci[dates_max_cocci],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_cocci[dates_end_cocci],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_cocci[dates_max_cocci], 
           y= data_signal_cocci$prop[dates_max_cocci]+20,label = paste(round(amplitude_real_cocci,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_15J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_15J], 
           y= moyenne_mobile_15J[dates_max_15J]+20,label = paste(round(amplitude_real_15J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  #geom_point(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "% de floraison")) +
  geom_line(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_30J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_cocci[dates_max_cocci],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_cocci[dates_end_cocci],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_cocci[dates_max_cocci], 
           y= data_signal_cocci$prop[dates_max_cocci]+20,label = paste(round(amplitude_real_cocci,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_30J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_30J], 
           y= moyenne_mobile_30J[dates_max_30J]+20,label = paste(round(amplitude_real_30J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )


## Platonia insignis ##

# Proportions de floraison par jour pour V.americana #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data=pheno2 %>% 
             filter(Usable==1),
           Spec= "Platonia_insignis",
           Pattern=c("Fl"),
           Obs_Veg = "PPFlo")[[1]] %>% 
  mutate(Year = year(date)) %>% 
  print() ->
  data_signal_ins


# Selection de la colonne proportion de floraison 
data_signal_ins %>% 
  select(prop) %>% 
  pull() ->
  signal_ins

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moving_average(data_signal_ins %>% 
                 select(prop) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) ->
  moyenne_mobile_ins

# Dates 
data_signal_ins %>% 
  select(date) %>% 
  pull() ->
  dates_ins

# Maximum des pics de floraison
dates_max_ins = sort(findpeaks(moyenne_mobile_ins,minpeakheight  = 10,nups =1)[,2])

# Debut des pics
dates_begin_ins = sort(findpeaks(moyenne_mobile_ins,minpeakheight  = 10,nups = 1)[,3])

# Fin des pics
dates_end_ins = sort(findpeaks(moyenne_mobile_ins,minpeakheight  = 10,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_ins = findpeaks(moyenne_mobile_ins,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_ins = signal_ins[dates_max_ins]


ggplot() + 
  #geom_point(data = data_signal_ins, aes(x = date, y = signal_ins, color = "% de floraison")) +
  geom_line(data = data_signal_ins, aes(x = date, y = prop, color = "% de floraison")) +
  #geom_point(data = Rain, aes(x = date, y = Cumule_15J, color = "Pluviométrie cumulée")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_ins[dates_max_ins], 
           y= data_signal_ins$prop[dates_max_ins]+20,label = paste(round(amplitude_real_ins,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_15J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_15J], 
           y= moyenne_mobile_15J[dates_max_15J]+20,label = paste(round(amplitude_real_15J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "% de floraison")) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_ins[dates_max_ins], 
           y= data_signal_ins$prop[dates_max_ins]+20,label = paste(round(amplitude_real_ins,1),"%"),
           col = "black") +
  
  # Maximums des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_rain[dates_max_30J],
             col = "#4DAF4A" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_rain[dates_max_30J], 
           y= moyenne_mobile_30J[dates_max_30J]+20,label = paste(round(amplitude_real_30J,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Pluviométrie cumulée (mm)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Pluviométrie cumulée" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )






### Relation entre les variables climatiques ### 

dataB_resume %>% 
  filter(!is.na(`Temp(55)`)) %>%
  filter(!is.na(`Hr(55)`)) %>%
  filter(!is.na(vpd55)) %>%
  filter(!is.na(Rain)) %>%
  filter(!is.na(ETP)) %>%
  filter(!is.na(VWC_10cm)) %>%
  group_by(Year, Month, Day, date) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),`Hr(55)` = mean(`Hr(55)`), vpd55 = mean(vpd55), 
            Rain = sum(Rain),ETP = sum(ETP), VWC_10cm = mean(VWC_10cm)) %>% 
  print() ->
  climat2

# Visualisation des variables et de leurs relations
pairs.panels(climat2[,5:10], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)


install.packages("moments")
library(moments)
skewness(climat$Rain, na.rm = TRUE)
skewness(climat$VWC_10cm , na.rm = TRUE)

# Transformation des données de la variable Rain pour obtenir une distribution plus proche de la normalité
climat2$Rain <- log(sqrt(climat$Rain)+1)

# Standardisation des variables climatiques
climat2[,5:10]%>% 
  scale() %>% 
  print ->
  climat2_scaled

climat2_scaled <- as.data.frame(climat2_scaled)


# Visualisation des variables et de leurs relations
pairs.panels(climat2_scaled, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

# ACP #
acp_climat <- dudi.pca(climat2_scaled, , scale=T, center=T, scannf = F,nf=4)

# #Calcul des % de chaque axe :
pc<-round(acp_climat$eig/sum(acp_climat$eig)*100,2)

# % cumulés
cumsum(pc)

# BarPlot des % d'inertie
# Definir le min et max de l'axe des y
ylim <- c(0, 1.2*max(pc))
xx <- barplot(pc, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim, ylab = "% d'inertie")
## Ajout des valeurs de % en dessus des barres
text(x = xx, y = pc, label = pc, pos = 3, cex = 0.8, col = "black")
## Ajout des labels sur l'axe des x (ie. numero des axes factoriels)
axis(1, at=xx, labels=c(1:length(pc)), tick=FALSE, las=1, line=-0.5, cex.axis=0.8)


# cercle des correlations (pour une ACP normee)
s.corcircle(acp_climat$co)

# Valeurs des coefficients de correlation de Pearson : 
cor(climat2_scaled)

# representation sur les deux premiers axes
s.label(acp_climat$li[,1:2], clabel=0.5) 

# Coordonnees des individus sur l'axe 1 (axe des x) en fonction de leur valeur pour chaque variable (axe des y) : 
score(acp_climat)

# Calcul de la somme des cos2 des individus
cont=inertia.dudi(acp_climat, row.inertia = TRUE)
cont

# Calcul des cos2 :
cos2=abs(cont$row.rel)/10000

# Calcul des cos2 cumulees des axes 1-2 formant le premier plan factoriel :
c2p12=rowSums(cos2[,c(1,2)])
barplot(cos2[,1],las=2)
barplot(cos2[,2],las=2)
barplot(c2p12,las=2)
s.value(acp_climat$li[,1:2], c2p12, method="greylevel",csize=0.4)

# Autre representation :
install.packages("factoextra")
library(factoextra) 
fviz_cos2(acp_climat, choice = "ind", axe=1:2)
fviz_pca_biplot(acp_climat, col.ind = "cos2", gradient.cols=c("red","yellow","green"),repel = TRUE) 



### FLORAISON ET VPD ###

# VPD #

# Full data pour chaque jour du suivi
climat %>% 
  select(Year, Month, date, vpd55) %>% 
  filter(!is.na(vpd55)) %>% 
  mutate(vpd55 = vpd55*100) %>% # Multiplication par 100 pour mieux voir les variations
  print() ->
  VPD

sum(is.na(VPD$vpd55))

# Calcul du moyenne VPD tous les 15 j

VPD$vpd_15 <- rollapply(VPD$vpd55, width = 15, FUN = mean, fill = NA, align = "right")

# Calcul du moyenne VPD tous les 30 j

VPD$vpd_30 <- rollapply(VPD$vpd55, width = 30, FUN = mean, fill = NA, align = "right")


VPD %>% 
  mutate(vpd_15 = if_else(is.na(vpd_15), 0, vpd_15)) %>% 
  mutate(vpd_30 = if_else(is.na(vpd_30), 0, vpd_30)) %>% 
  print() ->
  VPD_bis


# Maximum VPD 15j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(VPD_bis %>% 
                 select(vpd_15) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) %>% 
  print() ->
  moyenne_mobile_VPD_15

# Dates 
VPD %>% 
  select(date) %>% 
  pull() ->
  dates_VPD

# Maximum des pics de floraison
dates_max_VPD_15 = sort(findpeaks(moyenne_mobile_VPD_15,minpeakheight  = 60,nups =1)[,2])
indices2 <- c(5, 7, 8, 10, 13, 15, 20, 27)
dates_max_VPD_15 <- dates_max_VPD_15[indices2]

# Debut des pics
dates_begin_VPD_15 = sort(findpeaks(moyenne_mobile_VPD_15,minpeakheight  = 60,nups = 1)[,3])

# Fin des pics
dates_end_VPD_15= sort(findpeaks(moyenne_mobile_VPD_15,minpeakheight  = 60,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_VPD_15 = findpeaks(moyenne_mobile_VPD_15,minpeakheight  = 60,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_VPD_15 = VPD_bis$vpd_15[dates_max_VPD_15]



# Maximum VPD 30j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(VPD_bis %>% 
                 select(vpd_30) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) %>% 
  print() ->
  moyenne_mobile_VPD_30

# Dates 
dates_VPD

# Maximum des pics de floraison
dates_max_VPD_30 = sort(findpeaks(moyenne_mobile_VPD_30,minpeakheight  = 60,nups =1)[,2])
indices2 <- c(5, 7, 8, 10, 13, 15, 20, 27)
dates_max_VPD_30 <- dates_max_VPD_30[indices2]

# Debut des pics
dates_begin_VPD_30 = sort(findpeaks(moyenne_mobile_VPD_30,minpeakheight  = 60,nups = 1)[,3])

# Fin des pics
dates_end_VPD_30= sort(findpeaks(moyenne_mobile_VPD_30,minpeakheight  = 60,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_VPD_30 = findpeaks(moyenne_mobile_VPD_30,minpeakheight  = 60,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_VPD_30 = VPD_bis$vpd_30[dates_max_VPD_30]



display.brewer.all(type = "div")
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
brewer.pal(n = 3, name = "Set2")

# Graphique VPD 15J
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "% de floraison"), linewidth = 0.7) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_15, color = "VPD"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_globu[dates_max_globu],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_globu[dates_begin_globu],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_globu[dates_end_globu],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_globu[dates_max_globu]+10, 
           y= data_signal_globu$prop[dates_max_globu]+7,label = paste(round(amplitude_real_globu,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
  geom_vline(xintercept = dates_VPD[dates_max_VPD_15],
             col = "#FC8D62" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_VPD[dates_max_VPD_15], 
           y= moyenne_mobile_VPD_15[dates_max_VPD_15]+5,label = paste(round(amplitude_real_VPD_15,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "VPD" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )


# Graphique VPD 30J
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "% de floraison"), linewidth = 0.7) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_30, color = "VPD"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_globu[dates_max_globu],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_globu[dates_begin_globu],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_globu[dates_end_globu],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_globu[dates_max_globu]+10, 
           y= data_signal_globu$prop[dates_max_globu]+7,label = paste(round(amplitude_real_globu,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
 # geom_vline(xintercept = dates_VPD[dates_max_VPD_30],
 #            col = "#FC8D62" , 
  #           linetype = "dashed", linewidth = 0.7) + 
  #annotate("text",x = dates_VPD[dates_max_VPD_30], 
  #         y= moyenne_mobile_VPD_30[dates_max_VPD_30]+5,label = paste(round(amplitude_real_VPD_30,1)),
  #        col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "VPD" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )




### FLORAISON ET TEMPERATURES ###

# Temperatures #

# Full data pour chaque jour du suivi
climat %>% 
  select(Year, Month, date, `Temp(55)`) %>% 
  filter(!is.na(`Temp(55)`)) %>% 
  print() ->
  Temperature

# Dates 
Temperature %>% 
  select(date) %>% 
  pull() ->
  dates_Temp

max(Temperature$`Temp(55)`)

# Maximum des pics de floraison
dates_max_Temp = sort(findpeaks(Temperature$`Temp(55)`, minpeakheight  = 28,nups =1)[,2])

# Debut des pics
dates_begin_Temp = sort(findpeaks(Temperature$`Temp(55)`,minpeakheight  = 28,nups = 1)[,3])

# Fin des pics
dates_end_Temp = sort(findpeaks(Temperature$`Temp(55)`,minpeakheight  = 28,nups = 1)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_Temp = findpeaks(Temperature$`Temp(55)`,minpeakheight  = 28,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_Temp = Temperature$`Temp(55)`[dates_max_Temp]




ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "% de floraison"), linewidth =1) +
  geom_line(data = Temperature, aes(x = date, y = `Temp(55)`, color = "Temperature")) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_globu[dates_max_globu],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_globu[dates_begin_globu],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_globu[dates_end_globu],
             col = "grey30", linetype = "dashed") +
  annotate("text",x = dates_globu[dates_max_globu], 
           y= data_signal_globu$prop[dates_max_globu]+20,label = paste(round(amplitude_real_globu,1),"%"),
           col = "black") +
  
  # Debut/maximum/fin des pics de pluviometrie cumulee
  geom_vline(xintercept = dates_Temp[dates_max_Temp],
             col = "#4DAF4A", 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_Temp[dates_max_Temp], 
           y= Temperature$`Temp(55)`[dates_max_Temp]+20,label = paste(round(amplitude_real_Temp,1)),
           col = "black") +
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD (kPa)")) +
  scale_color_manual(values = c("% de floraison" = "#984EA3", "Temperature" = "#999999")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Températures et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )





