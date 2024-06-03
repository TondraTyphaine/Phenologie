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


#### DATA FLORAISON ####

## Data brutes
pheno <- read_csv2("data/Synthese_Pheno_20230724.csv")

# On ajuste en supprimant les colonnes qu'on veut garder (dans ton jeux de données adapté tu as deux colonnes factices encore)
pheno <- pheno[,-c(1,4)]

# Formatage des donnees
pheno2 <- PrepPhase(pheno) #Preparation des données brutes

# Formatage des colonnes
pheno2 %>% 
  mutate(CrownID = as.factor(CrownID),
                           PPVeg = str_replace_all(PPVeg,"(NA|Na|Na;|NA;)", NA_character_), 
                           PPVeg = as.factor(PPVeg), 
                           Update = as.Date(Update,format = "%d/%m/%Y")) ->
  pheno2  


#### DATA CLIMAT (donnees de M.Bonal) ####

## Data 
dataB <- read_csv2("data/GX-METEO-2020 - 2024E - AK.csv")

## Reduction deu jeu de donnees
dataB %>% 
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            Rg = mean(Rg),
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


#### RELATION ENTRE LES VARIABLES CLIMATIQUES #### 

dataB_resume %>% 
  filter(!is.na(`Temp(55)`)) %>%
  filter(!is.na(`Hr(55)`)) %>%
  filter(!is.na(Rg)) %>%
  filter(!is.na(vpd55)) %>%
  filter(!is.na(Rain)) %>%
  filter(!is.na(ETP)) %>%
  filter(!is.na(VWC_10cm)) %>%
  group_by(Year, Month, Day, date) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),`Hr(55)` = mean(`Hr(55)`), Rg = mean(Rg), vpd55 = mean(vpd55), 
            Rain = sum(Rain),ETP = sum(ETP), VWC_10cm = mean(VWC_10cm)) %>% 
  print() ->
  climat

# Visualisation des variables et de leurs relations (variables non transformees)
pairs.panels(climat[,5:11], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

# Transformation des données de la variable Rain pour obtenir une distribution plus proche de la normalité
climat$Rain <- log(sqrt(climat$Rain)+1)

# Standardisation des variables climatiques
climat[,5:11]%>% 
  scale() %>% 
  print ->
  climat_scaled

climat_scaled <- as.data.frame(climat_scaled)


# Visualisation des variables et de leurs relations (variables transformees et centree-reduites)
pairs.panels(climat_scaled, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

## ACP ##

# Modele
acp_climat <- dudi.pca(climat_scaled, scale = T, center = T, scannf = F, nf = 4)

# Calcul des % de chaque axe
pc <- round(acp_climat$eig/sum(acp_climat$eig)*100, 2)

# % cumules
cumsum(pc)

# BarPlot des % d'inertie #

# Definir le min et max de l'axe des y
ylim <- c(0, 1.2*max(pc))

# Barplot
xx <- barplot(pc, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim, ylab = "% d'inertie")

# Ajout des valeurs de % en dessus des barres
text(x = xx, y = pc, label = pc, pos = 3, cex = 0.8, col = "black")

# Ajout des labels sur l'axe des x (ie. numero des axes factoriels)
axis(1, at = xx, labels = c(1:length(pc)), tick = FALSE, las = 1, line = -0.5, cex.axis = 0.8)


# cercle des correlations (pour une ACP normee) #
s.corcircle(acp_climat$co)

# Valeurs des coefficients de correlation de Pearson # 
cor(climat_scaled)

# representation sur les deux premiers axes #
s.label(acp_climat$li[,1:2], clabel = 0.5) 

# Coordonnees des individus sur l'axe 1 (axe des x) en fonction de leur valeur pour chaque variable (axe des y) : 
score(acp_climat)

# Calcul de la somme des cos2 des individus
cont <- inertia.dudi(acp_climat, row.inertia = TRUE)
cont

# Calcul des cos2 :
cos2 <- abs(cont$row.rel)/10000

# Calcul des cos2 cumulees des axes 1-2 formant le premier plan factoriel :
c2p12 <- rowSums(cos2[,c(1,2)])
barplot(cos2[,1],las=2)
barplot(cos2[,2],las=2)
barplot(c2p12,las=2)
s.value(acp_climat$li[,1:2], c2p12, method="greylevel",csize=0.4)

# Autre representation :
install.packages("factoextra")
library(factoextra) 
fviz_cos2(acp_climat, choice = "ind", axe=1:2)
fviz_pca_biplot(acp_climat, col.ind = "cos2", gradient.cols=c("red","yellow","green"),repel = TRUE) 





#### VISUALISATION FLORAISON ET PLUVIOMETRIE ####

## Symphonia globulifera ##

# Proportions de floraison par jour pour S.globulifera #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data = pheno2 %>% 
             filter(Usable == 1),
           Spec = "Symphonia_globulifera",
           Pattern = c("Fl"),
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
               filter = fpoids(n = 2,p = 2,q = 2)$y) ->
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

# Remplacement des NA par 0
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
               filter = fpoids(n= 2, p= 2, q= 2)$y) %>% 
  print() ->
  moyenne_mobile_15J

# Dates 
Rain_bis %>% 
  select(date) %>% 
  pull() ->
  dates_rain

# Maximum des pics de floraison
dates_max_15J = sort(findpeaks(moyenne_mobile_15J,minpeakheight  = 430,nups =1)[,2])

# Compilation des amplitudes relles des pics du signal
amplitude_real_15J = Rain_bis$Cumule_15J[dates_max_15J]


# Maximum et minimum de pluviometrie cumulee 30 j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(Rain_bis %>% 
                 select(Cumule_30J) %>% 
                 pull(),
               filter = fpoids(n= 2, p= 2, q= 2)$y) %>% 
  print() ->
  moyenne_mobile_30J

# Dates 
dates_rain

# Maximum des pics de floraison
dates_max_30J = sort(findpeaks(moyenne_mobile_30J,minpeakheight  = 700,nups = 1)[,2])
dates_max_30J <- dates_max_30J[c(1, 5, 8, 9)]
 
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
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée 15J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 15J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée 30J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 30J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )




## Symphonia sp1 ##

# Proportions de floraison par jour pour S.p1 #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data = pheno2 %>% 
             filter(Usable == 1),
           Spec = "Symphonia_sp.1",
           Pattern = c("Fl"),
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
               filter = fpoids(n= 2, p= 2, q= 2)$y) ->
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
  geom_line(data = data_signal_sp1, aes(x = date, y = prop, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_sp1, aes(x = date, y = moyenne_mobile_sp1, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée 15J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 15J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  geom_line(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_sp1, aes(x = date, y = moyenne_mobile_sp1, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée 30J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 30J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )


## Vouacapoua americana ##

# Proportions de floraison par jour pour V.americana #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data = pheno2 %>% 
             filter(Usable == 1),
           Spec = "Vouacapoua_americana",
           Pattern = c("Fl"),
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
               filter = fpoids(n= 2, p= 2, q= 2)$y) ->
  moyenne_mobile_am

# Dates 
data_signal_am %>% 
  select(date) %>% 
  pull() ->
  dates_am

# Maximum des pics de floraison
dates_max_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 0)[,2])

# Debut des pics
dates_begin_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 0)[,3])

# Fin des pics
dates_end_am = sort(findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 0)[,4])

# Pourcentage de floraison par pic
amplitude_peaks_am = findpeaks(moyenne_mobile_am,minpeakheight  = 5,nups = 0)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_am = signal_am[dates_max_am]

# Graphique 15J #
ggplot() + 
  geom_line(data = data_signal_am, aes(x = date, y = prop, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_am, aes(x = date, y = moyenne_mobile_am, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée 15J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 15J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Vouacapoua americana",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  geom_line(data = data_signal_am, aes(x = date, y = signal_am, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_am, aes(x = date, y = moyenne_mobile_am, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée 30J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 30J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Vouacapoua americana",
    x = "Dates",
    color = "Légende"
  )



## Couma guianensis ##

# Proportions de floraison par jour pour C.guianenesis #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data = pheno2 %>% 
             filter(Usable == 1),
           Spec= "Couma_guianensis",
           Pattern = c("Fl"),
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
               filter = fpoids(n= 2, p= 2, q= 2)$y) ->
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


# Graphique pluviometrie cumulee 15J et pourcentage floraison #
ggplot() + 
  geom_line(data = data_signal_gui, aes(x = date, y = prop, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_gui, aes(x = date, y = moyenne_mobile_gui, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée 15J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 15J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  geom_line(data = data_signal_gui, aes(x = date, y = signal_gui, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_gui, aes(x = date, y = moyenne_mobile_gui, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée 30J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 30J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )



## Moronobea coccinea ##

# Proportions de floraison par jour  #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data = pheno2 %>% 
             filter(Usable == 1),
           Spec = "Moronobea_coccinea",
           Pattern = c("Fl"),
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
               filter = fpoids(n= 2, p= 2, q= 2)$y) ->
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
dates_begin_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10,nups = 0)[,3])[1]

# Fin des pics
dates_end_cocci = sort(findpeaks(moyenne_mobile_cocci,minpeakheight  = 10,nups = 0)[,4])[3]

# Pourcentage de floraison par pic
amplitude_peaks_cocci = findpeaks(moyenne_mobile_cocci,minpeakheight  = 10,nups = 0)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_cocci = signal_cocci[dates_max_cocci]


# Graphique pluviometrie cumulee 15J et pourcentage floraison #
ggplot() + 
  geom_line(data = data_signal_cocci, aes(x = date, y = prop, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_cocci, aes(x = date, y = moyenne_mobile_cocci, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée 15J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 15J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  geom_line(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_cocci, aes(x = date, y = moyenne_mobile_cocci, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée 30J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 30J" = "#4DAF4A")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )


## Platonia insignis ##

# Proportions de floraison par jour #

# Nombre d'individus en fleur et pourcentage de floraison
LeafedOTim(Data = pheno2 %>% 
             filter(Usable == 1),
           Spec= "Platonia_insignis",
           Pattern = c("Fl"),
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
               filter = fpoids(n= 2, p= 2, q= 2)$y) ->
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


# Graphique pluviometrie cumulee 15J et pourcentage floraison #
ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = prop, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_15J, color = "Pluviométrie cumulée 15J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 15J" = "#4DAF4A")) +  
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )


# Graphique pluviometrie cumulee 30J et pourcentage floraison #

ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "Signal de floraison réel"),linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rain, aes(x = date, y = moyenne_mobile_30J, color = "Pluviométrie cumulée 30J"),linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "Pluviométrie cumulée 30J" = "#4DAF4A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie cumulée tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )








#### VISUALISATION FLORAISON ET VPD ####

# Symphonia globulifera #

# Full data pour chaque jour du suivi
climat %>% 
  select(Year, Month, date, vpd55) %>% 
  filter(!is.na(vpd55)) %>% 
  mutate(vpd55 = vpd55*100) %>% # Multiplication par 100 pour mieux voir les variations
  print() ->
  VPD

sum(is.na(VPD$vpd55))

# Calcul moyenne VPD tous les 15 j

VPD$VPD_15 <- rollapply(VPD$vpd55, width = 15, FUN = mean, fill = NA, align = "right")

# Calcul moyenne VPD tous les 30 j

VPD$VPD_30 <- rollapply(VPD$vpd55, width = 30, FUN = mean, fill = NA, align = "right")

# Remplacement des Na par 0
VPD %>% 
  mutate(VPD_15 = if_else(is.na(VPD_15), 0, VPD_15)) %>% 
  mutate(VPD_30 = if_else(is.na(VPD_30), 0, VPD_30)) %>% 
  print() ->
  VPD_bis


# Maximum VPD 15j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(VPD_bis %>% 
                 select(VPD_15) %>% 
                 pull(),
               filter = fpoids(n= 2, p= 2, q= 2)$y) %>% 
  print() ->
  moyenne_mobile_VPD_15

# Dates 
VPD %>% 
  select(date) %>% 
  pull() ->
  dates_VPD

# Maximum des pics de VPD
dates_max_VPD_15 = sort(findpeaks(moyenne_mobile_VPD_15,minpeakheight  = 60,nups =1)[,2])
dates_max_VPD_15 <- dates_max_VPD_15[c(5, 7, 8, 10, 13, 15, 20, 27)]

# Valeurs relles des pics
amplitude_real_VPD_15 <- VPD_bis$VPD_15[dates_max_VPD_15]



# Maximum VPD 30j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(VPD_bis %>% 
                 select(VPD_30) %>% 
                 pull(),
               filter = fpoids(n=2,p=2,q=2)$y) %>% 
  print() ->
  moyenne_mobile_VPD_30

# Dates 
dates_VPD

# Maximum des pics
dates_max_VPD_30 <- sort(findpeaks(moyenne_mobile_VPD_30,minpeakheight  = 60,nups = 1)[,2])
dates_max_VPD_30 <- dates_max_VPD_30[c(5, 6, 7, 8, 9, 13, 23)]

# Valeurs relles des pics
amplitude_real_VPD_30 <- VPD_bis$VPD_30[dates_max_VPD_30]



display.brewer.all(type = "div")
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
brewer.pal(n = 3, name = "Set2")

# Graphique VPD 15J et floraison
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_15, color = "VPD 15J"), linewidth = 0.7) +
  
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"VPD 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )


# Graphique VPD 30J et floraison
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_30, color = "VPD 30J"), linewidth = 0.7) +
  
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
 geom_vline(xintercept = dates_VPD[dates_max_VPD_30],
            col = "#FC8D62" ,
           linetype = "dashed", linewidth = 0.7) +
 annotate("text",x = dates_VPD[dates_max_VPD_30],
         y= moyenne_mobile_VPD_30[dates_max_VPD_30]+2,label = paste(round(amplitude_real_VPD_30,1)),
        col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"VPD 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )


# Symphonia sp1 #

# Graphique VPD 15J et floraison
ggplot() + 
  geom_line(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_sp1, aes(x = date, y = moyenne_mobile_sp1, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_15, color = "VPD 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_sp1[dates_max_sp1],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_sp1[dates_end_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_sp1[dates_max_sp1]+10, 
           y= data_signal_sp1$prop[dates_max_sp1]+7,label = paste(round(amplitude_real_sp1,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
  geom_vline(xintercept = dates_VPD[dates_max_VPD_15],
             col = "#FC8D62" , 
             linetype = "dashed", linewidth = 0.7) + 
  annotate("text",x = dates_VPD[dates_max_VPD_15], 
           y= moyenne_mobile_VPD_15[dates_max_VPD_15]+2,label = paste(round(amplitude_real_VPD_15,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "black", "VPD 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )


# Graphique VPD 30J et floraison
ggplot() + 
  geom_line(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_sp1, aes(x = date, y = moyenne_mobile_sp1, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_30, color = "VPD 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_sp1[dates_max_sp1],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_sp1[dates_end_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_sp1[dates_max_sp1]+10, 
           y= data_signal_sp1$prop[dates_max_sp1]+7,label = paste(round(amplitude_real_sp1,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
  geom_vline(xintercept = dates_VPD[dates_max_VPD_30],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VPD[dates_max_VPD_30],
           y= moyenne_mobile_VPD_30[dates_max_VPD_30]+5,label = paste(round(amplitude_real_VPD_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3",
                                "Signal de floraison traité" = "black", "VPD 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )



# Vouacapoua americana #

# Graphique VPD 15J et floraison 
ggplot() + 
  geom_line(data = data_signal_am, aes(x = date, y = signal_am, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_am, aes(x = date, y = moyenne_mobile_am, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_15, color = "VPD 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_am[dates_max_am],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_am[dates_begin_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_am[dates_end_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_am[dates_max_am]+10, 
           y= data_signal_am$prop[dates_max_am]+7,label = paste(round(amplitude_real_am,1),"%"),
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "VPD 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Vouacapoua americana",
    x = "Dates",
    color = "Légende"
  )


# Graphique VPD 30J et floraison 
ggplot() + 
  geom_line(data = data_signal_am, aes(x = date, y = signal_am, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_am, aes(x = date, y = moyenne_mobile_am, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_30, color = "VPD 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_am[dates_max_am],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_am[dates_begin_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_am[dates_end_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_am[dates_max_am]+10, 
           y= data_signal_am$prop[dates_max_am]+7,label = paste(round(amplitude_real_am,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
  geom_vline(xintercept = dates_VPD[dates_max_VPD_30],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VPD[dates_max_VPD_30],
           y= moyenne_mobile_VPD_30[dates_max_VPD_30]+5,label = paste(round(amplitude_real_VPD_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40", "VPD 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Vouacapoua americana",
    x = "Dates",
    color = "Légende"
  )



# Couma guianensis #

# Graphique VPD 15J
ggplot() + 
  geom_line(data = data_signal_gui, aes(x = date, y = signal_gui, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_gui, aes(x = date, y = moyenne_mobile_gui, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_15, color = "VPD moyen 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_gui[dates_max_gui],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_gui[dates_begin_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_gui[dates_end_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_gui[dates_max_gui]+10, 
           y= data_signal_gui$prop[dates_max_gui]+7,label = paste(round(amplitude_real_gui,1),"%"),
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3",
                                "Signal de floraison traité" = "grey40", "VPD moyen 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )


# Graphique VPD 30J
ggplot() + 
  geom_line(data = data_signal_gui, aes(x = date, y = signal_gui, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_gui, aes(x = date, y = moyenne_mobile_gui, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_30, color = "VPD moyen 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_gui[dates_max_gui],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_gui[dates_begin_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_gui[dates_end_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_gui[dates_max_gui]+10, 
           y= data_signal_gui$prop[dates_max_gui]+7,label = paste(round(amplitude_real_gui,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
  geom_vline(xintercept = dates_VPD[dates_max_VPD_30],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VPD[dates_max_VPD_30],
           y= moyenne_mobile_VPD_30[dates_max_VPD_30]+5,label = paste(round(amplitude_real_VPD_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3",
                                "Signal de floraison traité" = "grey40", "VPD moyen 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )


# Moronobea coccinea #

# Graphique VPD 15J
ggplot() + 
  geom_line(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_cocci, aes(x = date, y = moyenne_mobile_cocci, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_15, color = "VPD moyen 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_cocci[dates_max_cocci],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_cocci[dates_end_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_cocci[dates_max_cocci]+10, 
           y= data_signal_cocci$prop[dates_max_cocci]+7,label = paste(round(amplitude_real_cocci,1),"%"),
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3",
                                "Signal de floraison traité" = "grey40", "VPD moyen 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )


# Graphique VPD 30J
ggplot() + 
  geom_line(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_cocci, aes(x = date, y = moyenne_mobile_cocci, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_30, color = "VPD moyen 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_cocci[dates_max_cocci],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_cocci[dates_end_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_cocci[dates_max_cocci]+10, 
           y= data_signal_cocci$prop[dates_max_cocci]+7,label = paste(round(amplitude_real_cocci,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
  geom_vline(xintercept = dates_VPD[dates_max_VPD_30],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VPD[dates_max_VPD_30],
           y= moyenne_mobile_VPD_30[dates_max_VPD_30]+5,label = paste(round(amplitude_real_VPD_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3",
                                "Signal de floraison traité" = "grey40", "VPD moyen 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )



# Platonia insignis #

# Graphique VPD 15J
ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_15, color = "VPD moyen 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_ins[dates_max_ins]+10, 
           y= data_signal_ins$prop[dates_max_ins]+7,label = paste(round(amplitude_real_ins,1),"%"),
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
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3",
                                "Signal de floraison traité" = "grey40", "VPD moyen 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )


# Graphique VPD 30J
ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VPD, aes(x = date, y = moyenne_mobile_VPD_30, color = "VPD moyen 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_ins[dates_max_ins]+10, 
           y= data_signal_ins$prop[dates_max_ins]+7,label = paste(round(amplitude_real_ins,1),"%"),
           col = "black") +
  
  # Maximums des pics de VPD
  geom_vline(xintercept = dates_VPD[dates_max_VPD_30],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VPD[dates_max_VPD_30],
           y= moyenne_mobile_VPD_30[dates_max_VPD_30]+5,label = paste(round(amplitude_real_VPD_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VPD x100 (kPa)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3",
                                "Signal de floraison traité" = "grey40", "VPD moyen 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )



#### VISUALISATION FLORAISON ET HUMIDITE DU SOL ####

# Full data pour chaque jour du suivi
climat %>% 
  select(Year, Month, date, VWC_10cm) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  mutate(VWC_10cm = VWC_10cm*100) %>% # Multiplication par 100 pour mieux voir les variations
  print() ->
  VWC

sum(is.na(VWC$VWC_10cm))

# Calcul moyenne VWC tous les 15 j

VWC$VWC_15 <- rollapply(VWC$VWC_10cm, width = 15, FUN = mean, fill = NA, align = "right")

# Calcul moyenne VWC tous les 30 j

VWC$VWC_30 <- rollapply(VWC$VWC_10cm, width = 30, FUN = mean, fill = NA, align = "right")

# Remplacement des Na par 0
VWC %>% 
  mutate(VWC_15 = if_else(is.na(VWC_15), 0, VWC_15)) %>% 
  mutate(VWC_30 = if_else(is.na(VWC_30), 0, VWC_30)) %>% 
  print() ->
  VWC_bis


# Maximum VWC 15j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(VWC_bis %>% 
                 select(VWC_15) %>% 
                 pull(),
               filter = fpoids(n= 2, p= 2, q= 2)$y) %>% 
  print() ->
  moyenne_mobile_VWC_15

# Dates 
VWC %>% 
  select(date) %>% 
  pull() ->
  dates_VWC

# Maximum des pics de VWC
dates_max_VWC_15 <- sort(findpeaks(moyenne_mobile_VWC_15,minpeakheight  = 30, nups = 1)[,2])
dates_max_VWC_15 <- dates_max_VWC_15[c(2, 9, 24, 28)]

# Valeurs relles des pics
amplitude_real_VWC_15 = VWC_bis$VWC_15[dates_max_VWC_15]



# Maximum VWC 30j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(VWC_bis %>% 
                 select(VWC_30) %>% 
                 pull(),
               filter = fpoids(n= 2, p= 2, q= 2)$y) %>% 
  print() ->
  moyenne_mobile_VWC_30

# Dates 
dates_VWC

# Maximum des pics de VWC
dates_max_VWC_30J <- sort(findpeaks(moyenne_mobile_VWC_30,minpeakheight  = 30, nups = 1)[,2])
dates_max_VWC_30J <- dates_max_VWC_30J[c(2, 7, 12, 20)]

# Valeurs relles des pics
amplitude_real_VWC_30 = VWC_bis$VWC_30[dates_max_VWC_30J]



# Graphique VWC 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_15, color = "VWC 15J"), linewidth = 0.7) +

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
  
  # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_15],
           y= moyenne_mobile_VWC_15[dates_max_VWC_15]+5,label = paste(round(amplitude_real_VWC_15,1)),
           col = "black") +

  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"VWC 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité moyenne du sol de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )


# Graphique VWC 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_30, color = "VWC 30J"), linewidth = 0.7) +
  
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
  
  # # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_30J],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_30J],
           y= moyenne_mobile_VWC_30[dates_max_VWC_30J]+2,label = paste(round(amplitude_real_VWC_30,1)),
           col = "black") +

  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"VWC 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )



# Symphonia sp1 #

# Graphique VWC 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_sp1, aes(x = date, y = moyenne_mobile_sp1, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_15, color = "VWC 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_sp1[dates_max_sp1],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_sp1[dates_end_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_sp1[dates_max_sp1]+10, 
           y= data_signal_sp1$prop[dates_max_sp1]+7,label = paste(round(amplitude_real_sp1,1),"%"),
           col = "black") +
  
  # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_15],
           y= moyenne_mobile_VWC_15[dates_max_VWC_15]+5,label = paste(round(amplitude_real_VWC_15,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"VWC 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité moyenne du sol de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )


# Graphique VWC 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_sp1, aes(x = date, y = signal_sp1, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_sp1, aes(x = date, y = moyenne_mobile_sp1, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_30, color = "VWC 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_sp1[dates_max_sp1],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_sp1[dates_begin_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_sp1[dates_end_sp1],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_sp1[dates_max_sp1]+10, 
           y= data_signal_sp1$prop[dates_max_sp1]+7,label = paste(round(amplitude_real_sp1,1),"%"),
           col = "black") +
  
  # # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_30J],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_30J],
           y= moyenne_mobile_VWC_30[dates_max_VWC_30J]+2,label = paste(round(amplitude_real_VWC_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"VWC 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Symphonia sp1",
    x = "Dates",
    color = "Légende"
  )



# Vouacapoua americana #

# Graphique VWC 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_am, aes(x = date, y = signal_am, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_am, aes(x = date, y = moyenne_mobile_am, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_15, color = "VWC 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_am[dates_max_am],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_am[dates_begin_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_am[dates_end_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_am[dates_max_am]+10, 
           y= data_signal_am$prop[dates_max_am]+7,label = paste(round(amplitude_real_am,1),"%"),
           col = "black") +
  
  # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_15],
           y= moyenne_mobile_VWC_15[dates_max_VWC_15]+5,label = paste(round(amplitude_real_VWC_15,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"VWC 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité moyenne du sol de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Vouacapoua americana",
    x = "Dates",
    color = "Légende"
  )


# Graphique VWC 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_am, aes(x = date, y = signal_am, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_am, aes(x = date, y = moyenne_mobile_am, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_30, color = "VWC 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_am[dates_max_am],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_am[dates_begin_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_am[dates_end_am],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_am[dates_max_am]+10, 
           y= data_signal_am$prop[dates_max_am]+7,label = paste(round(amplitude_real_am,1),"%"),
           col = "black") +
  
  # # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_30J],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_30J],
           y= moyenne_mobile_VWC_30[dates_max_VWC_30J]+2,label = paste(round(amplitude_real_VWC_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"VWC 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )



# Couma guianensis #

# Graphique VWC 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_gui, aes(x = date, y = signal_gui, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_gui, aes(x = date, y = moyenne_mobile_gui, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_15, color = "VWC 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_gui[dates_max_gui],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_gui[dates_begin_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_gui[dates_end_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_gui[dates_max_gui]+10, 
           y= data_signal_gui$prop[dates_max_gui]+7,label = paste(round(amplitude_real_gui,1),"%"),
           col = "black") +
  
  # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_15],
           y= moyenne_mobile_VWC_15[dates_max_VWC_15]+5,label = paste(round(amplitude_real_VWC_15,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"VWC 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité moyenne du sol de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )


# Graphique VWC 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_gui, aes(x = date, y = signal_gui, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_gui, aes(x = date, y = moyenne_mobile_gui, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_30, color = "VWC 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_gui[dates_max_gui],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_gui[dates_begin_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_gui[dates_end_gui],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_gui[dates_max_gui]+10, 
           y = data_signal_gui$prop[dates_max_gui]+7,label = paste(round(amplitude_real_gui,1),"%"),
           col = "black") +
  
  # # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_30J],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_30J],
           y = moyenne_mobile_VWC_30[dates_max_VWC_30J]+2,label = paste(round(amplitude_real_VWC_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"VWC 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Couma guianensis",
    x = "Dates",
    color = "Légende"
  )



# Moronobea coccinea #

# Graphique VWC 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_cocci, aes(x = date, y = moyenne_mobile_cocci, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_15, color = "VWC 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_cocci[dates_max_cocci],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_cocci[dates_end_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_cocci[dates_max_cocci]+10, 
           y= data_signal_cocci$prop[dates_max_cocci]+7,label = paste(round(amplitude_real_cocci,1),"%"),
           col = "black") +
  
  # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_15],
           y= moyenne_mobile_VWC_15[dates_max_VWC_15]+5,label = paste(round(amplitude_real_VWC_15,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"VWC 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité moyenne du sol de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )


# Graphique VWC 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_cocci, aes(x = date, y = signal_cocci, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_cocci, aes(x = date, y = moyenne_mobile_cocci, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_30, color = "VWC 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_cocci[dates_max_cocci],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_cocci[dates_begin_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_cocci[dates_end_cocci],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_cocci[dates_max_cocci]+10, 
           y = data_signal_cocci$prop[dates_max_cocci]+7,label = paste(round(amplitude_real_cocci,1),"%"),
           col = "black") +
  
  # # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_30J],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_30J],
           y = moyenne_mobile_VWC_30[dates_max_VWC_30J]+2,label = paste(round(amplitude_real_VWC_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"VWC 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Moronobea coccinea",
    x = "Dates",
    color = "Légende"
  )



# Platonia insignis #

# Graphique VWC 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_15, color = "VWC 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_ins[dates_max_ins]+10, 
           y= data_signal_ins$prop[dates_max_ins]+7,label = paste(round(amplitude_real_ins,1),"%"),
           col = "black") +
  
  # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_15],
           y= moyenne_mobile_VWC_15[dates_max_VWC_15]+5,label = paste(round(amplitude_real_VWC_15,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"VWC 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité moyenne du sol de la dernière quinzaine de jour et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )


# Graphique VWC 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = VWC, aes(x = date, y = moyenne_mobile_VWC_30, color = "VWC 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_ins[dates_max_ins]+10, 
           y = data_signal_ins$prop[dates_max_ins]+7,label = paste(round(amplitude_real_ins,1),"%"),
           col = "black") +
  
  # # Maximums des pics de VWC
  geom_vline(xintercept = dates_VWC[dates_max_VWC_30J],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_VWC[dates_max_VWC_30J],
           y = moyenne_mobile_VWC_30[dates_max_VWC_30J]+2,label = paste(round(amplitude_real_VWC_30,1)),
           col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "VWC X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"VWC 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de vapeur moyen de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )


#### VISUALISATION FLORAISON ET RAYONNEMENT GLOBAL ####

# Full data pour chaque jour du suivi
climat2 %>% 
  select(Year, Month, date, Rg) %>% 
  filter(!is.na(Rg)) %>% 
  #mutate(Rg = Rg*100) %>% # Multiplication par 100 pour mieux voir les variations
  print() ->
  Rg

sum(is.na(Rg$Rg))

# Calcul moyenne Rg tous les 15 j

Rg$Rg_15 <- rollapply(Rg$Rg, width = 15, FUN = mean, fill = NA, align = "right")

# Calcul moyenne Rg tous les 30 j

Rg$Rg_30 <- rollapply(Rg$Rg, width = 30, FUN = mean, fill = NA, align = "right")

# Remplacement des Na par 0
Rg %>% 
  mutate(Rg_15 = if_else(is.na(Rg_15), 0, Rg_15)) %>% 
  mutate(Rg_30 = if_else(is.na(Rg_30), 0, Rg_30)) %>% 
  print() ->
  Rg_bis


# Maximum Rg 15j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(Rg_bis %>% 
                 select(Rg_15) %>% 
                 pull(),
               filter = fpoids(n= 2, p= 2, q= 2)$y) %>% 
  print() ->
  moyenne_mobile_Rg_15

# Dates 
Rg %>% 
  select(date) %>% 
  pull() ->
  dates_Rg

# Maximum des pics de Rg
dates_max_Rg_15 <- sort(findpeaks(moyenne_mobile_Rg_15,minpeakheight  = 250, nups = 1)[,2])
dates_max_Rg_15 <- dates_max_Rg_15[c(5, 6, 8, 16)]

# Valeurs relles des pics
amplitude_real_Rg_15 = Rg_bis$Rg_15[dates_max_Rg_15]



# Maximum Rg 30j par annee #

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
moving_average(Rg_bis %>% 
                 select(Rg_30) %>% 
                 pull(),
               filter = fpoids(n= 2, p= 2, q= 2)$y) %>% 
  print() ->
  moyenne_mobile_Rg_30

# Dates 
dates_Rg

# Maximum des pics de Rg
dates_max_Rg_30J <- sort(findpeaks(moyenne_mobile_Rg_30,minpeakheight  = 30, nups = 1)[,2])
dates_max_Rg_30J <- dates_max_Rg_30J[c(2, 7, 12, 20)]

# Valeurs relles des pics
amplitude_real_Rg_30 = Rg_bis$Rg_30[dates_max_Rg_30J]


# Symphonia globulifera #

# Graphique Rg 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rg, aes(x = date, y = moyenne_mobile_Rg_15, color = "Rg 15J"), linewidth = 0.7) +
  
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
  
  # Maximums des pics de Rg
  geom_vline(xintercept = dates_Rg[dates_max_Rg_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_Rg[dates_max_Rg_15],
           y= moyenne_mobile_Rg_15[dates_max_Rg_15]+5,label = paste(round(amplitude_real_Rg_15,1)),
           col = "black") +

  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Rg X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"Rg 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Rayonnement globale incident moyen tous les 15 jours  et pourcentage de floraison au cours du suivi phénologique de Symphonia globulifera",
    x = "Dates",
    color = "Légende"
  )


# Graphique Rg 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_globu, aes(x = date, y = signal_globu, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_globu, aes(x = date, y = moyenne_mobile_globu, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rg, aes(x = date, y = moyenne_mobile_Rg_30, color = "Rg 30J"), linewidth = 0.7) +
  
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
  
  # # # Maximums des pics de Rg
  # geom_vline(xintercept = dates_Rg[dates_max_Rg_30J],
  #            col = "#FC8D62" ,
  #            linetype = "dashed", linewidth = 0.7) +
  # annotate("text",x = dates_Rg[dates_max_Rg_30J],
  #          y= moyenne_mobile_Rg_30[dates_max_Rg_30J]+2,label = paste(round(amplitude_real_Rg_30,1)),
  #          col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Rg X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"Rg 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Rayonnement globale incident de la dernière trentaine de jour et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )

# Platonia insignis #

# Graphique Rg 15J et floraison #
ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rg, aes(x = date, y = moyenne_mobile_Rg_15, color = "Rg 15J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_ins[dates_max_ins]+10, 
           y= data_signal_ins$prop[dates_max_ins]+7,label = paste(round(amplitude_real_ins,1),"%"),
           col = "black") +
  
  # Maximums des pics de Rg
  geom_vline(xintercept = dates_Rg[dates_max_Rg_15],
             col = "#FC8D62" ,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",x = dates_Rg[dates_max_Rg_15],
           y= moyenne_mobile_Rg_15[dates_max_Rg_15]+5,label = paste(round(amplitude_real_Rg_15,1)),
           col = "black") +

  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Rg X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", 
                                "Signal de floraison traité" = "grey40" ,"Rg 15J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Rayonnement globale incident moyen tous les 15 jours et pourcentage de floraison au cours du suivi phénologique de Platonia insignis",
    x = "Dates",
    color = "Légende"
  )


# Graphique Rg 30J et floraison #
ggplot() + 
  geom_line(data = data_signal_ins, aes(x = date, y = signal_ins, color = "Signal de floraison réel"), linewidth = 0.7) +
  geom_line(data = data_signal_ins, aes(x = date, y = moyenne_mobile_ins, color = "Signal de floraison traité"), linewidth = 0.5) +
  geom_line(data = Rg, aes(x = date, y = moyenne_mobile_Rg_30, color = "Rg 30J"), linewidth = 0.7) +
  
  # Debut/maximum/fin des pics de floraison
  geom_vline(xintercept = dates_ins[dates_max_ins],
             col = "#984EA3" , 
             linetype = "dashed", linewidth = 0.7) + 
  geom_vline(xintercept = dates_ins[dates_begin_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  geom_vline(xintercept = dates_ins[dates_end_ins],
             col = "black", linetype = "dotdash", linewidth = 0.5) +
  annotate("text",x = dates_ins[dates_max_ins]+10, 
           y= data_signal_ins$prop[dates_max_ins]+7,label = paste(round(amplitude_real_ins,1),"%"),
           col = "black") +
  
  # # # Maximums des pics de Rg
  # geom_vline(xintercept = dates_Rg[dates_max_Rg_30J],
  #            col = "#FC8D62" ,
  #            linetype = "dashed", linewidth = 0.7) +
  # annotate("text",x = dates_Rg[dates_max_Rg_30J],
  #          y= moyenne_mobile_Rg_30[dates_max_Rg_30J]+2,label = paste(round(amplitude_real_Rg_30,1)),
  #          col = "black") +
  
  
  # Mise en forme
  x_date_4ans +
  scale_y_continuous(name = "Pourcentage d'arbre en fleur", sec.axis = sec_axis(~., name = "Rg X100 (m³/m³)")) +
  scale_color_manual(values = c("Signal de floraison réel" = "#984EA3", "Signal de floraison traité" = "black" ,"Rg 30J" = "#FC8D62")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Rayonnement globale incident moyen tous les 30 jours et pourcentage de floraison au cours du suivi phénologique de Symphonia inslifera",
    x = "Dates",
    color = "Légende"
  )









#### RELATION ENTRE FLORAISON ET VARIABLES CLIMATIQUES ####


## FLORAISON ET PLUVIOMETRIE/TEMPERATURE ##

# Symphonia globulifera #

# Toutes les dates
all_dates <- seq(min(data_signal_globu$date, Rain_bis$date), max(data_signal_globu$date, Rain_bis$date), by = "day")

# Compléter les tibbles avec les dates manquantes
data_signal_globu %>%
  right_join(tibble(date = all_dates), by = "date") %>% 
  print() ->
  Floraison_glb

Rain_bis %>%
  right_join(tibble(date = all_dates), by = "date") %>% 
  print() ->
  Rain_bis


Floraison_glb %>% 
  left_join(Rain_bis, by = "date") %>% 
  filter(!is.na(prop)) %>% 
  print() ->
  Flo_pluvio


Flo_pluvio %>%
  mutate(Temp_15J = rollapply(Flo_pluvio$`Temp(55)`, width = 15, FUN = sum, fill = NA, align = "right")) %>% 
  mutate(Temp_30J = rollapply(Flo_pluvio$`Temp(55)`, width = 30, FUN = sum, fill = NA, align = "right")) %>% 
  mutate(Temp_15J = if_else(is.na(Temp_15J), mean(`Temp(55)`), Temp_15J)) %>% 
  mutate(Temp_30J = if_else(is.na(Temp_30J), mean(`Temp(55)`), Temp_30J)) %>% 
  select(prop, Cumule_15J, Cumule_30J,Temp_15J,Temp_30J) %>%
  scale() ->
  Flo_pluvio

cor(Flo_pluvio)


# Visualisation des variables et de leurs relations
pairs.panels(Flo_pluvio, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

## ACP ##

ACP_glb <- dudi.pca(Flo_pluvio,scale = T, center = T, scannf = F, nf = 4 )

# Calcul des % de chaque axe
pc_glb <- round(ACP_glb$eig/sum(ACP_glb$eig)*100, 2)

# % cumules
cumsum(pc_glb)

# BarPlot des % d'inertie #

# Definir le min et max de l'axe des y
ylim <- c(0, 1.2*max(pc_glb))

# Barplot
xx <- barplot(pc_glb, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim, ylab = "% d'inertie")

# Ajout des valeurs de % en dessus des barres
text(x = xx, y = pc_glb, label = pc_glb, pos = 3, cex = 0.8, col = "black")

# Ajout des labels sur l'axe des x (ie. numero des axes factoriels)
axis(1, at = xx, labels = c(1:length(pc_glb)), tick = FALSE, las = 1, line = -0.5, cex.axis = 0.8)


# cercle des correlations (pour une ACP normee) #
s.corcircle(ACP_glb$co)

# Valeurs des coefficients de correlation de Pearson # 
cor(Flo_pluvio)

# representation sur les deux premiers axes #
s.label(ACP_glb$li[,1:2], clabel = 0.5) 

# Calcul de la somme des cos2 des individus
cont_glb <- inertia.dudi(ACP_glb, row.inertia = TRUE)
cont_glb

# Calcul des cos2 :
cos2_glb <- abs(cont_glb$row.rel)/10000

# Representation 
fviz_cos2(ACP_glb, choice = "ind", axe=1:2)
fviz_pca_biplot(ACP_glb, col.ind = "cos2", gradient.cols=c("red","yellow","green"),repel = TRUE) 












# 
# # Proportion de floraison de S.globulifera sur l'ensemble du suivi (variable réponse)
# signal_globu <- round(Flo_pluvio$prop)
# # signal_globu <- log(sqrt(signal_globu)+1)
# # signal_globu <- signal_globu[-c(1,41,44)]
# summary(signal_globu)
# mean(signal_globu)
# var(signal_globu)
# sd(signal_globu)
# 
# # Ajustement de la distribution
# fitdistr(signal_globu, "exponential")
# fitdistr(round(signal_globu), "poisson")
# 
# # Visualisation de la distribution des donnees
# hist(signal_globu, freq = F)
# lines(density(signal_globu), col = "red")
# 
# # Test de normalite
# shapiro.test(signal_globu) # Pas de normalité (p = 5.22e-09)
# 
# 
# 


# 
# par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# hist(signal_globu, freq = F)
# lines(density(signal_globu), col = "red")
# ks.test(signal_globu, "plnorm", mean(signal_globu), sd(signal_globu))
# 
# glb <- fitdistr(signal_globu, "poisson")
# 
# 
# qqnorm(signal_globu)
# qqline(signal_globu)
# 
# 
# # Pluviométrie cumulee tous les 15 jours sur la période du suivi (variable explicative)
# Pluvio_15 <-Flo_pluvio$Cumule_15J
# 
# # Transformation des données pour fit la normalite
# Pluvio_15 <- sqrt(Pluvio_15) 
# length(Pluvio_15)
# 
# mean(Pluvio_15)
# var(Pluvio_15)
# sd(Pluvio_15)
# 
# shapiro.test(Pluvio_15) # Pas de normalité mais plus proche que avant transformation
# hist(Pluvio_15)
# qqnorm(Pluvio_15)
# qqline(Pluvio_15)
# 
# var.test(signal_globu,Pluvio_15) # Heteroscedasticite
# 
# cov(signal_globu,Pluvio_15)
# 
# cor.test(signal_globu,Pluvio_15) # Corrélation significative et positive (0.48)
# 
# plot(signal_globu~Pluvio_15) # Visualition de la relation entre les variables
# 
# ggplot(Flo_pluvio, aes(x = Pluvio_15, y = signal_globu)) +
#   geom_point() + 
#   geom_smooth(method = "lm")
# 
# 
# 
# # Pluviométrie cumulee tous les 30 jours sur la période du suivi (variable explicative)
# Pluvio_30 <- Flo_pluvio$Cumule_30J
# 
# # Transformation des données pour fit la normalite
# Pluvio_30 <- sqrt(Pluvio_30) 
# length(Pluvio_30)
# 
# mean(Pluvio_30)
# var(Pluvio_30)
# sd(Pluvio_30)
# 
# shapiro.test(Pluvio_30) # Pas de normalité mais plus proche que avant transformation
# hist(Pluvio_30)
# qqnorm(Pluvio_30)
# qqline(Pluvio_30)
# 
# var.test(signal_globu,Pluvio_30) # Heteroscedasticite
# 
# cov(signal_globu,Pluvio_30)
# 
# cor.test(signal_globu,Pluvio_30) # Corrélation significative et positive (0.53)
# 
# plot(signal_globu~Pluvio_30) # Visualition de la relation entre les variables
# 
# ggplot(Flo_pluvio, aes(x = Pluvio_30, y = signal_globu)) +
#   geom_point() + 
#   geom_smooth(method = "lm")
# 
# 
# 
# # Model GLM
# GLM_15 <- glm(signal_globu~Pluvio_15, data = Flo_pluvio, family = "poisson")
# summary(GLM_15)
# 
# anova(GLM_15, test = "Chi")
# 
# par(mfrow = c(2,2))
# plot(GLM_15)
# 
# plot(signal_globu~Pluvio_15, data = Flo_pluvio, col="blue")
# abline(GLM_15, col="red")
# 
# # Model RLS
# mod_flo_pluvio_15 <- lm(signal_globu~Pluvio_15, data = Flo_pluvio)
# 
# # Residus
# mod_flo_pluvio_15_res <- residuals(mod_flo_pluvio_15)
# 
# dwtest(mod_flo_pluvio_15) # p < 0.05, autocorrelation des residus (pas d'independance)
# 
# par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# plot(mod_flo_pluvio_15)
# 
# shapiro.test(mod_flo_pluvio_15_res) # Pas de normalite
# bptest(mod_flo_pluvio_15) # p < 0.05, Homogeneite des residus
# 
# # Etude des parametres du model
# summary(mod_flo_pluvio_15)
# confint(mod_flo_pluvio_15)
# 
# plot(signal_globu~Pluvio_15, data = Flo_pluvio, col="blue")
# abline(mod_flo_pluvio_15, col="red")
# 
# # Etude table d'anova
# anova(mod_flo_pluvio_15)
# 
# # Etude intervalle de confiance
# pred<-predict(mod_flo_pluvio_15, interval = c( "confidence"),
#               level = 0.95, type = c("response"))
# pred





