# Titre : Analyses des donnees meteorologiques
# Auteur : Tondra Typhaine
# Date de creation : 03/05/2024
# Derniere modification : 03/05/2024


## Packages necessaires
install.packages("tidyverse")
install.packages("lubridate")
install.packages("RColorBrewer")
install.packages("pracma")
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(pracma)

#### Avec les donnees de M.Badouard ####

## Data 
#data<- read_csv2("data/TourAFlux-2004-2023.csv")

#" Reduction des donnees
# data %>% 
# filter(Year == 2020 : 2023) %>% 
#  select(Year,Month, Day,
#         `Temp(55)`, `Hr(55)`, `Temp(30)`, `Hr(30)`, `Temp(2.5)`, `Hr(2.5)`, 
#         vpd55,
#        Rain, 
#        `ETP penman (mm h-1) - OK`,
#        `ETP (mm h-1) NEW CNR4`,
#        CNR4_temp,
#        TempC_55_CR3000,
#        RH_55_CR3000) ->
# data


## Moyenne des variables par jour par annee et ajout colonne date

# Pour 2020
#data %>% 
#  filter(Year == 2020) %>% 
#  group_by(Year, Month, Day) %>% 
# summarise(`Temp(55)` = mean(`Temp(55)`),
#           `Hr(55)`= mean(`Hr(55)`),
#           `Temp(30)`= mean(`Temp(30)`),
#           `Hr(30)`= mean(`Hr(30)`),
#           `Temp(2.5)`= mean(`Temp(2.5)`),
#           `Hr(2.5)`= mean(`Hr(2.5)`),
#           vpd55= mean(vpd55),
#           Rain= mean(Rain),
#           `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
#           `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
#           CNR4_temp= mean(CNR4_temp),
#           TempC_55_CR3000= mean(TempC_55_CR3000),
#           RH_55_CR3000= mean(RH_55_CR3000))%>% 
# mutate(date = ymd("2020-01-01") + days(Day - 1)) %>%
# print() ->
# data2020


# Pour 2021
#data %>% 
# filter(Year == 2021) %>% 
# group_by(Year, Month, Day) %>% 
# summarise(`Temp(55)` = mean(`Temp(55)`),
#           `Hr(55)`= mean(`Hr(55)`),
#           `Temp(30)`= mean(`Temp(30)`),
#           `Hr(30)`= mean(`Hr(30)`),
#           `Temp(2.5)`= mean(`Temp(2.5)`),
#           `Hr(2.5)`= mean(`Hr(2.5)`),
#           vpd55= mean(vpd55),
#           Rain= mean(Rain),
#           `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
#           `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
#           CNR4_temp= mean(CNR4_temp),
#           TempC_55_CR3000= mean(TempC_55_CR3000),
#           RH_55_CR3000= mean(RH_55_CR3000))%>%
# mutate(date = ymd("2021-01-01") + days(Day - 1)) %>%
#  print() ->
# data2021

# Pour 2022
#data %>% 
# filter(Year == 2022) %>% 
# group_by(Year, Month, Day) %>% 
# summarise(`Temp(55)` = mean(`Temp(55)`),
#           `Hr(55)`= mean(`Hr(55)`),
#           `Temp(30)`= mean(`Temp(30)`),
#           `Hr(30)`= mean(`Hr(30)`),
#           `Temp(2.5)`= mean(`Temp(2.5)`),
#           `Hr(2.5)`= mean(`Hr(2.5)`),
#           vpd55= mean(vpd55),
#           Rain= mean(Rain),
#           `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
#           `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
#           CNR4_temp= mean(CNR4_temp),
#           TempC_55_CR3000= mean(TempC_55_CR3000),
#           RH_55_CR3000= mean(RH_55_CR3000))%>% 
# mutate(date = ymd("2022-01-01") + days(Day - 1)) %>%
# print() ->
# data2022

# Pour 2023
#data %>% 
# filter(Year == 2023) %>% 
# group_by(Year, Month, Day) %>% 
# summarise(`Temp(55)` = mean(`Temp(55)`),
#           `Hr(55)`= mean(`Hr(55)`),
#           `Temp(30)`= mean(`Temp(30)`),
#           `Hr(30)`= mean(`Hr(30)`),
#           `Temp(2.5)`= mean(`Temp(2.5)`),
#           `Hr(2.5)`= mean(`Hr(2.5)`),
#           vpd55= mean(vpd55),
#           Rain= mean(Rain),
#           `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
#           `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
#           CNR4_temp= mean(CNR4_temp),
#           TempC_55_CR3000= mean(TempC_55_CR3000),
#           RH_55_CR3000= mean(RH_55_CR3000))%>% 
# mutate(date = ymd("2023-01-01") + days(Day - 1)) %>%
# print() ->
# data2023


## Empilement des donnees
#bind_rows(data2020) %>% 
# bind_rows(data2021) %>% 
# bind_rows(data2022) %>% 
# bind_rows(data2023) ->
# data_resume



  
#### Avec les donnees de M.Bonal ####

## Data 
dataB<- read_csv2("data/GX-METEO-2020 - 2024E - AK.csv")

## Reduction deu jeu de donnees
dataB %>% 
  filter(!is.na(`Temp(55)`)) %>% 
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
            ETP = mean(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm)) ->
  dataB


## Data pour chaque annee

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


## PLUIE ET TEMPERATURES ##

## Somme de la pluviométrie entre le jour et la nuit et moyenne des temperatures

# Pour 2020
dataB2020 %>% 
  select(Year, Month, date, Rain,`Temp(55)`) %>% 
  group_by(Year, Month, date) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>% 
  print ->
  Rain2020

# Pour 2021
dataB2021 %>% 
  select(Year, Month, date, Rain,`Temp(55)`) %>% 
  group_by(Year, Month, date) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  print ->
  Rain2021

# Pour 2022
dataB2022 %>% 
  select(Year, Month, date, Rain,`Temp(55)`) %>% 
  group_by(Year, Month, date) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  print ->
  Rain2022

# Pour 2023
dataB2023 %>% 
  select(Year, Month, date, Rain,`Temp(55)`) %>% 
  group_by(Year, Month, date) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  print ->
  Rain2023

# Pour 2024
dataB2024 %>%
  filter(!is.na(Rain)) %>% 
  select(Year, Month, date, Rain,`Temp(55)`) %>% 
  group_by(Year, Month, date) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  print ->
  Rain2024


## Trouver les pics de pluie

# Pour 2020
pic2020 <- sort(findpeaks(Rain2020$Rain, minpeakheight = 100, nups = 1)[,2]) # Position du pic
pic2020_Y <- sort(findpeaks(Rain2020$Rain, minpeakheight = 100, nups = 1)[,1]) # Valeur du pic
dates2020 <- unique(Rain2020$date) # Extraction des dates
  
# Pour 2021
pic2021 <- sort(findpeaks(Rain2021$Rain, minpeakheight = 120, nups = 1)[,2])
pic2021_Y <- sort(findpeaks(Rain2021$Rain, minpeakheight = 120, nups = )[,1])
dates2021 <- Rain2021$date

# Pour 2022
pic2022 <- sort(findpeaks(Rain2022$Rain, minpeakheight = 86, nups = 1)[,2])
pic2022_Y <- sort(findpeaks(Rain2022$Rain, minpeakheight = 86, nups = )[,1])
dates2022 <- Rain2022$date

# Pour 2023
pic2023 <- sort(findpeaks(Rain2023$Rain, minpeakheight = 100, nups = 1)[,2])
pic2023_Y <- sort(findpeaks(Rain2023$Rain, minpeakheight = 100, nups = )[,1])
dates2023 <- Rain2023$date

# Pour 2024
pic2024 <- sort(findpeaks(Rain2024$Rain, minpeakheight = 29, nups = 1)[,2])
pic2024_Y <- sort(findpeaks(Rain2024$Rain, minpeakheight = 29, nups = 1 )[,1])
dates2024 <- Rain2024$date


## Identifier les mois secs selon la definition de Banyuls et Gaussen (P inferieure ou egale a deux fois la °C)

# Pour 2020 #

# Data par jour
Rain2020

# Data par mois
data_Dry2020 %>%
  group_by(Year, Month) %>%
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  
  # Pour arrondir les dates au 1er de chaque mois (specifie par "unit = month")
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  data_Dry_month2020

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2020, aes(x = date, y = Rain),colour = "#1B9E77")+
  geom_line(data = Rain2020, aes(x = date, y = `Temp(55)`), colour = "red") +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2020",
    x = "Dates"
    )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = data_Dry_month2020, aes(x = date , y = Rain),,colour = "#1B9E77")+
  geom_line(data = data_Dry_month2020, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = data_Dry_month2020, aes(x = date[5], y = max(data_Dry_month2020$Rain)))+
  geom_point(data = data_Dry_month2020, aes(x = date[10], y = min(data_Dry_month2020$Rain)))+
  annotate("text",x = data_Dry_month2020$date[5], 
           y =  max(data_Dry_month2020$Rain)+20,label =  max(data_Dry_month2020$Rain),
           col = "grey40", size = 3)+
  annotate("text",x = data_Dry_month2020$date[10], 
           y = min(data_Dry_month2020$Rain)-20,label =  min(data_Dry_month2020$Rain),
           col = "grey40", size = 3)+
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2020"
    )


# Pour 2021 #

# Data par jour
Rain2021

# Data par mois
data_Dry2021 %>%
  group_by(Year, Month) %>%
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  data_Dry_month2021

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2021, aes(x = date, y = Rain),colour = "#D95F02")+
  geom_line(data = Rain2021, aes(x = date, y = `Temp(55)`), colour = "red") +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2021",
    x = "Dates"
  )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = data_Dry_month2021, aes(x = date , y = Rain),,colour = "#D95F02")+
  geom_line(data = data_Dry_month2021, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = data_Dry_month2021, aes(x = date[4], y = max(data_Dry_month2021$Rain)))+
  geom_point(data = data_Dry_month2021, aes(x = date[10], y = min(data_Dry_month2021$Rain)))+
  annotate("text",x = data_Dry_month2021$date[4], 
           y = max(data_Dry_month2021$Rain)+20,label = max(data_Dry_month2021$Rain),
           col = "grey40", size = 3)+
  annotate("text",x = data_Dry_month2021$date[10], 
           y =  min(data_Dry_month2021$Rain)-20,label =  min(data_Dry_month2021$Rain),
           col = "grey40", size = 3)+
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2021"
  )


# Pour 2022 #

# Data par jour
Rain2022

# Data par mois
data_Dry2022 %>%
  group_by(Year, Month) %>%
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  data_Dry_month2022

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2022, aes(x = date, y = Rain),colour = "#7570B3")+
  geom_line(data = Rain2022, aes(x = date, y = `Temp(55)`), colour = "red") +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2021",
    x = "Dates"
  )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = data_Dry_month2022, aes(x = date , y = Rain),colour = "#7570B3")+
  geom_line(data = data_Dry_month2022, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = data_Dry_month2022, aes(x = date[2], y = max(data_Dry_month2022$Rain)))+
  geom_point(data = data_Dry_month2022, aes(x = date[9], y = min(data_Dry_month2022$Rain)))+
  annotate("text",x = data_Dry_month2022$date[2], 
           y = max(data_Dry_month2022$Rain)+20,label = max(data_Dry_month2022$Rain),
           col = "grey40", size = 3)+
  annotate("text",x = data_Dry_month2022$date[9], 
           y = min(data_Dry_month2022$Rain)-20,label =  min(data_Dry_month2022$Rain),
           col = "grey40", size = 3)+
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2021"
  )


# Pour 2023 #

# Data par jour
Rain2023

# Data par mois
Rain2023 %>%
  group_by(Year, Month) %>%
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  data_Dry_month2023

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2023, aes(x = date, y = Rain),colour = "#E7298A")+
  geom_line(data = Rain2023, aes(x = date, y = `Temp(55)`), colour = "red") +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2021",
    x = "Dates"
  )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = data_Dry_month2023, aes(x = date , y = Rain),colour = "#E7298A")+
  geom_line(data = data_Dry_month2023, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = data_Dry_month2023, aes(x = date[2], y = max(data_Dry_month2023$Rain)))+
  geom_point(data = data_Dry_month2023, aes(x = date[10], y = min(data_Dry_month2023$Rain)))+
  annotate("text",x = data_Dry_month2023$date[2], 
           y =  max(data_Dry_month2023$Rain)+20,label = max(data_Dry_month2023$Rain),
           col = "grey40", size = 3)+
  annotate("text",x = data_Dry_month2023$date[10], 
           y =  min(data_Dry_month2023$Rain)-20,label =  min(data_Dry_month2023$Rain),
           col = "grey40", size = 3)+
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2023"
  )


# Pour 2024 #

# Data par jour
Rain2024

# Data par mois
data_Dry2024 %>%
  group_by(Year, Month) %>%
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  data_Dry_month2024

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2024, aes(x = date, y = Rain),colour = "#66A61E")+
  geom_line(data = Rain2024, aes(x = date, y = `Temp(55)`), colour = "red") +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2021",
    x = "Dates"
  )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = data_Dry_month2024, aes(x = date , y = Rain),colour = "#66A61E")+
  geom_line(data = data_Dry_month2024, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = data_Dry_month2024, aes(x = date[1], y = max(data_Dry_month2024$Rain)))+
  geom_point(data = data_Dry_month2024, aes(x = date[4], y = min(data_Dry_month2024$Rain)))+
  annotate("text",x = data_Dry_month2024$date[1], 
           y = max(data_Dry_month2024$Rain)+20,label = max(data_Dry_month2024$Rain),
           col = "grey40", size = 3)+
  annotate("text",x = data_Dry_month2024$date[4], 
           y = min(data_Dry_month2024$Rain)-20,label =  min(data_Dry_month2024$Rain),
           col = "grey40", size = 3)+
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2023"
  )



## Graphiques de la pluviometrie 

# Vue d'ensemble sur les 4 ans

display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 5, name = "Dark2")
brewer.pal(n = 5, name = "RdBu")
brewer.pal(n = 5, name = "YlGnBu")
"#FFFFB2" "#FECC5C" "#FD8D3C" "#F03B20" "#BD0026"
"#FFFFD4" "#FED98E" "#FE9929" "#D95F0E" "#993404"
"#FFFFCC" "#A1DAB4" "#41B6C4" "#2C7FB8" "#253494"
ggplot() +
  geom_line(data = Rain2020, aes(x= date, y= Rain),colour = "#1B9E77") +
  geom_line(data = Rain2021, aes(x= date, y= Rain), colour = "#D95F02") +
  geom_line(data = Rain2022, aes(x= date, y= Rain), colour = "#7570B3") +
  geom_line(data = Rain2023, aes(x= date, y= Rain), colour = "#E7298A") +
  geom_line(data = Rain2024, aes(x= date, y= Rain), colour = "#66A61E") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_point(aes(x = dates2020[pic2020], y =  pic2020_Y)) +
  geom_point(aes(x = dates2021[pic2021], y =  pic2021_Y)) +
  geom_point(aes(x = dates2022[pic2022], y =  pic2022_Y)) +
  geom_point(aes(x = dates2023[pic2023], y =  pic2023_Y)) +
  geom_point(aes(x = dates2024[pic2024], y =  pic2024_Y)) +
  geom_vline(xintercept = dates2020[pic2020],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2021[pic2021],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[pic2022],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[pic2023],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[pic2024],
             col = "black", linetype = "dashed") +
  annotate("text", x = dates2020[pic2020]+30, y = pic2020_Y, label = pic2020_Y,col = "black", size = 3) +
  annotate("text", x = dates2021[pic2021]+30, y = pic2021_Y, label = pic2021_Y,col = "black", size = 3) +
  annotate("text", x = dates2022[pic2022]+30, y = pic2022_Y, label = pic2022_Y,col = "black", size = 3) +
  annotate("text", x = dates2023[pic2023]+30, y = pic2023_Y, label = pic2023_Y,col = "black", size = 3) +
  annotate("text", x = dates2024[pic2024]+30, y = pic2024_Y, label = pic2024_Y,col = "black", size = 3) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
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
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  
  labs(
    title = "Pluviométrie au cours des 4 années de suivies phénologique",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )


# Pour 2020
ggplot(Rain2020, aes(x = date, y = Rain)) +
  geom_line(colour = "#1B9E77") +
  geom_vline(xintercept = dates2020[pic2020],
             col = "black", linetype = "dashed")+

  # "Scale_x_date" pour specifier que des dates sont utilisees en abscisse 
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  
  # Mise en forme des labels de l'axe des abscisses
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  
  labs(
    title = "Pluviométrie au cours de l'année 2020",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )


# Pour 2021
ggplot(Rain2021, aes(x = date, y = Rain))+
  geom_line(colour = "#D95F02")+
  geom_vline(xintercept = dates2021[pic2021],
             col = "black", linetype = "dashed")+
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2021",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2022
ggplot(Rain2022, aes(x = date, y = Rain))+
  geom_line( colour = "#7570B3")+
  geom_vline(xintercept = dates2022[pic2022],
             col = "black", linetype = "dashed")+
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2022",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2023
ggplot(Rain2023, aes(x = date, y = Rain))+
  geom_line(colour = "#E7298A")+
  geom_vline(xintercept = dates2023[pic2023],
             col = "black", linetype = "dashed")+
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2023",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2024
ggplot(Rain2024, aes(x = date, y = Rain))+
  geom_line(colour = "#66A61E")+
  geom_vline(xintercept = dates2024[pic2024],
             col = "black", linetype = "dashed")+
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2024",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )



## HUMIDITE DE L'AIR (Hr55), ETP, HUMIDITE DU SOL (VWC_10cm) ET DEFICIT DE SATURATION (vpd55) ##

# Pour 2020 #
dataB2020 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) ->
  dataB2020_hum

# Data journalières
dataB2020_hum %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  print() ->
  humidity2020

dataB2020_hum %>% 
  filter(`J/N` == "J") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2020_J

dataB2020_hum %>% 
  filter(`J/N` == "N") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2020_N

#Data mensuelles
dataB2020_hum %>% 
  filter(`J/N` == "J") %>% 
  group_by(Year, Month) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print() ->
  humidity_month2020_J

dataB2020_hum %>% 
  filter(`J/N` == "N") %>% 
  group_by(Year, Month) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print() ->
  humidity_month2020_N

# Data pour chaque mois
dataB2020_hum %>% 
  filter(`J/N` == "N") %>% 
  summarise(date,`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  print() ->
  humidity_MONTH_2020_N

humidity_MONTH_2020_N %>% 
  filter(Month == 1) %>% 
  print() ->
  humidity_01_2020_N

humidity_MONTH_2020_N %>% 
  filter(Month == 2) %>% 
  print() ->
  humidity_02_2020_N

humidity_MONTH_2020_N %>%  
  filter(Month == 3) %>% 
  print() ->
  humidity_03_2020_N

humidity_MONTH_2020_N %>% 
  filter(Month == 4) %>% 
  print() ->
  humidity_04_2020_N

humidity_MONTH_2020_N %>% 
  filter(Month == 5) %>% 
  print() ->
  humidity_05_2020_N

dataB2020_hum %>%  
  filter(Month == 6) %>% 
  print() ->
  humidity_06_2020_N

dataB2020_hum %>%  
  filter(Month == 7) %>% 
  print() ->
  humidity_07_2020_N

dataB2020_hum %>% 
  filter(Month == 8) %>% 
  print() ->
  humidity_08_2020_N

dataB2020_hum %>% 
  filter(Month == 9) %>% 
  print() ->
  humidity_09_2020_N

dataB2020_hum %>% 
  filter(Month == 10) %>% 
  print() ->
  humidity_10_2020_N

dataB2020_hum %>% 
  filter(Month == 11) %>% 
  print() ->
  humidity_11_2020_N

dataB2020_hum %>% 
  filter(Month == 12) %>% 
  print() ->
  humidity_12_2020_N

# Moyennes, 1erQ et 3eQ
hum_sum_J_2020 <- summary(humidity2020_J)
air2020_J<- hum_sum_J_2020[,6]
min_air_J_2020 <- air2020_J[1]
max_air_J_2020 <- air2020_J[6]
med_air_J_2020 <-  air2020_J[3]

hum_sum_N_2020 <- summary(humidity2020_N)
air2020_N<- hum_sum_N_2020[,6]
min_air_N_2020 <- air2020_N[1]
max_air_N_2020 <- air2020_N[6]
med_air_N_2020 <-  air2020_N[3]

# Graphique Hr(55) V.1 (journalier)
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2020_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_line(data = humidity2020_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  geom_hline(yintercept = 98.92, col = "red", linetype = "dashed") +
  geom_hline(yintercept = 68.64, col = "red", linetype = "dashed") +
  geom_hline(yintercept = 81.66 , col = "black", linetype = "dashed") +
  geom_hline(yintercept = 99.65, col = "blue", linetype = "dashed") +
  geom_hline(yintercept = 77.13, col = "blue", linetype = "dashed") +
  geom_hline(yintercept = 92.46 , col = "black", linetype = "dashed") +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air de jour et de nuit",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique Hr(55) V.2
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_point(data = humidity2020_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_point(data = humidity2020_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  geom_smooth(data = humidity2020_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_smooth(data = humidity2020_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  geom_hline(yintercept = 98.92, col = "red", linetype = "dashed") +
  geom_hline(yintercept = 68.64, col = "red", linetype = "dashed") +
  geom_hline(yintercept = 81.66 , col = "black", linetype = "dashed") +
  geom_hline(yintercept = 99.65, col = "blue", linetype = "dashed") +
  geom_hline(yintercept = 77.13, col = "blue", linetype = "dashed") +
  geom_hline(yintercept = 92.46 , col = "black", linetype = "dashed") +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air de jour et de nuit",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique Hr(55) V.3 (au mois)
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity_month2020_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_line(data = humidity_month2020_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  geom_hline(yintercept = 98.92, col = "red", linetype = "dashed") +
  geom_hline(yintercept = 68.64, col = "red", linetype = "dashed") +
  geom_hline(yintercept = 81.66 , col = "black", linetype = "dashed") +
  geom_hline(yintercept = 99.65, col = "blue", linetype = "dashed") +
  geom_hline(yintercept = 77.13, col = "blue", linetype = "dashed") +
  geom_hline(yintercept = 92.46 , col = "black", linetype = "dashed") +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air de jour et de nuit",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique Hr(55) V.4 (graphique violon mois par mois pour la nuit)
install.packages("vioplot")
library(vioplot)

with(humidity2020 , {
  vioplot( 
    `Hr(55)`[Month== 1] ,`Hr(55)`[Month== 2] , `Hr(55)`[Month== 3] , `Hr(55)`[Month== 4] ,`Hr(55)`[Month== 5] ,
    `Hr(55)`[Month== 6] ,`Hr(55)`[Month== 7] ,`Hr(55)`[Month== 8] ,`Hr(55)`[Month== 9] ,`Hr(55)`[Month== 10] ,
    `Hr(55)`[Month== 11] ,`Hr(55)`[Month== 12] ,
    col= "#41B6C4" , names=c("Janvier","Février","Mars",
                                       "Avril","Mai","Juin",
                                       "Juillet","Août","Septembre",
                                       "Octobre","Novembre","Décembre"),
    xlab = "Mois",
    ylab = "Humidité de l'air (%)",
    main = "Humidité de l'air à 55m en 2022"
  )
 })


#ggplot() +
# geom_violin(data = humidity_01_2020_N, aes(x=date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_02_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_03_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_04_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_05_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_06_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_07_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_08_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_09_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_10_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_11_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# geom_violin(data = humidity_12_2020_N, aes(x = date, y = `Hr(55)`), draw_quantiles = c(0.25, 0.5, 0.75)) +
# annotate("text", x = humidity_01_2020_N$date[1] +15, y = median(humidity_01_2020_N$`Hr(55)`), 
#          label = round(median(humidity_01_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_02_2020_N$date[1] +15, y = median(humidity_02_2020_N$`Hr(55)`)+0.5, 
#          label = round(median(humidity_02_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_03_2020_N$date[1] +15, y = median(humidity_03_2020_N$`Hr(55)`)+0.7, 
#          label = round(median(humidity_03_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_04_2020_N$date[1] +15, y = median(humidity_04_2020_N$`Hr(55)`), 
#          label = round(median(humidity_04_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_05_2020_N$date[1] +15, y = median(humidity_05_2020_N$`Hr(55)`), 
#          label = round(median(humidity_05_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_06_2020_N$date[1] +15, y = median(humidity_06_2020_N$`Hr(55)`), 
#          label = round(median(humidity_06_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_07_2020_N$date[1] +15, y = median(humidity_07_2020_N$`Hr(55)`), 
#          label = round(median(humidity_07_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_08_2020_N$date[1] +15, y = median(humidity_08_2020_N$`Hr(55)`), 
#          label = round(median(humidity_08_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_09_2020_N$date[1] +15, y = median(humidity_09_2020_N$`Hr(55)`), 
#          label = round(median(humidity_09_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_10_2020_N$date[1] +15, y = median(humidity_10_2020_N$`Hr(55)`), 
#          label = round(median(humidity_10_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_11_2020_N$date[1] +15, y = median(humidity_11_2020_N$`Hr(55)`), 
#          label = round(median(humidity_11_2020_N$`Hr(55)`), digits = 2), size = 3) +
# annotate("text", x = humidity_12_2020_N$date[1] +15, y = median(humidity_12_2020_N$`Hr(55)`), 
#          label = round(median(humidity_12_2020_N$`Hr(55)`), digits = 2), size = 3) +
# scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                 "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                 "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
#              date_labels = "%Y-%m-%d") +
# theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
# labs(
#   title = "Humidité de l'air à 55m en 2020",
#   x = "Mois",
#   y = "Hr (%)"
#  )


# Graphique ETP
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2020_J, aes(x = date, y = ETP, color = "J")) +
  geom_line(data = humidity2020_N, aes(x = date, y = ETP, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Evapotranspiration potentielle de jour et de nuit",
    x = "Dates",
    y = "ETP (mm)"
  )


# Graphique VWC_10cm
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2020_J, aes(x = date, y = VWC_10cm, color = "J")) +
  geom_line(data = humidity2020_N, aes(x = date, y = VWC_10cm, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité du sol de jour et de nuit",
    x = "Dates",
    y = "Humidité du sol (m³/m³)"
  )

# Graphique vpd55
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2020_J, aes(x = date, y = vpd55, color = "J")) +
  geom_line(data = humidity2020_N, aes(x = date, y = vpd55, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Déficit de pression de vapeur de jour et de nuit",
    x = "Dates",
    y = "VPD (kPa)"
  )


# Pour 2021 #
dataB2021 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  print() ->
  humidity2021

dataB2021 %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "J") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2021_J

dataB2021 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "N") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2021_N


# Graphique Hr(55)
ggplot() +
  #geom_line(data = humidity2021, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2021_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_line(data = humidity2021_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air de jour et de nuit en 2021",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique ETP
ggplot() +
  #geom_line(data = humidity2021, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2021_J, aes(x = date, y = ETP, color = "J")) +
  geom_line(data = humidity2021_N, aes(x = date, y = ETP, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Evapotranspiration potentielle de jour et de nuit en 2021",
    x = "Dates",
    y = "ETP (mm)"
  )


# Graphique VWC_10cm
ggplot() +
  #geom_line(data = humidity2021, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2021_J, aes(x = date, y = VWC_10cm, color = "J")) +
  geom_line(data = humidity2021_N, aes(x = date, y = VWC_10cm, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité du sol de jour et de nuit en 2021",
    x = "Dates",
    y = "Humidité du sol (m³/m³)"
  )

# Graphique vpd55
ggplot() +
  #geom_line(data = humidity2021, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2021_J, aes(x = date, y = vpd55, color = "J")) +
  geom_line(data = humidity2021_N, aes(x = date, y = vpd55, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Déficit de pression de vapeur de jour et de nuit en 2021",
    x = "Dates",
    y = "VPD (kPa)"
  )



# Pour 2022 #
dataB2022 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  print() ->
  humidity2022

dataB2022 %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "J") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2022_J

dataB2022 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "N") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2022_N

# Graphique Hr(55)
ggplot() +
  #geom_line(data = humidity2022, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2022_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_line(data = humidity2022_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air de jour et de nuit en 2022",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique ETP
ggplot() +
  #geom_line(data = humidity2022, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2022_J, aes(x = date, y = ETP, color = "J")) +
  geom_line(data = humidity2022_N, aes(x = date, y = ETP, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Evapotranspiration potentielle de jour et de nuit en 2022",
    x = "Dates",
    y = "ETP (mm)"
  )


# Graphique VWC_10cm
ggplot() +
  #geom_line(data = humidity2022, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2022_J, aes(x = date, y = VWC_10cm, color = "J")) +
  geom_line(data = humidity2022_N, aes(x = date, y = VWC_10cm, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité du sol de jour et de nuit en 2022",
    x = "Dates",
    y = "Humidité du sol (m³/m³)"
  )

# Graphique vpd55
ggplot() +
  #geom_line(data = humidity2022, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2022_J, aes(x = date, y = vpd55, color = "J")) +
  geom_line(data = humidity2022_N, aes(x = date, y = vpd55, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Déficit de pression de vapeur de jour et de nuit en 2022",
    x = "Dates",
    y = "VPD (kPa)"
  )



# Pour 2023 #
dataB2023 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  print() ->
  humidity2023

dataB2023 %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "J") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2023_J

dataB2023 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "N") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2023_N

# Graphique Hr(55)
ggplot() +
  #geom_line(data = humidity2023, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2023_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_line(data = humidity2023_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air de jour et de nuit en 2023",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique ETP
ggplot() +
  #geom_line(data = humidity2023, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2023_J, aes(x = date, y = ETP, color = "J")) +
  geom_line(data = humidity2023_N, aes(x = date, y = ETP, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Evapotranspiration potentielle de jour et de nuit en 2023",
    x = "Dates",
    y = "ETP (mm)"
  )


# Graphique VWC_10cm
ggplot() +
  #geom_line(data = humidity2023, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2023_J, aes(x = date, y = VWC_10cm, color = "J")) +
  geom_line(data = humidity2023_N, aes(x = date, y = VWC_10cm, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité du sol de jour et de nuit en 2023",
    x = "Dates",
    y = "Humidité du sol (m³/m³)"
  )

# Graphique vpd55
ggplot() +
  #geom_line(data = humidity2023, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2023_J, aes(x = date, y = vpd55, color = "J")) +
  geom_line(data = humidity2023_N, aes(x = date, y = vpd55, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Déficit de pression de vapeur de jour et de nuit en 2023",
    x = "Dates",
    y = "VPD (kPa)"
  )



# Pour 2024 #
dataB2024 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  print() ->
  humidity2024

dataB2024 %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "J") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2024_J

dataB2024 %>% 
  filter(!is.na(`Hr(55)`)) %>% 
  filter(!is.na(ETP)) %>% 
  filter(!is.na(VWC_10cm)) %>% 
  filter(!is.na(vpd55)) %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  filter(`J/N` == "N") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity2024_N

# Graphique Hr(55)
ggplot() +
  #geom_line(data = humidity2024, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2024_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_line(data = humidity2024_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air de jour et de nuit en 2024",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique ETP
ggplot() +
  #geom_line(data = humidity2024, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2024_J, aes(x = date, y = ETP, color = "J")) +
  geom_line(data = humidity2024_N, aes(x = date, y = ETP, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Evapotranspiration potentielle de jour et de nuit en 2024",
    x = "Dates",
    y = "ETP (mm)"
  )


# Graphique VWC_10cm
ggplot() +
  #geom_line(data = humidity2024, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2024_J, aes(x = date, y = VWC_10cm, color = "J")) +
  geom_line(data = humidity2024_N, aes(x = date, y = VWC_10cm, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité du sol de jour et de nuit en 2024",
    x = "Dates",
    y = "Humidité du sol (m³/m³)"
  )

# Graphique vpd55
ggplot() +
  #geom_line(data = humidity2024, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity2024_J, aes(x = date, y = vpd55, color = "J")) +
  geom_line(data = humidity2024_N, aes(x = date, y = vpd55, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Déficit de pression de vapeur de jour et de nuit en 2024",
    x = "Dates",
    y = "VPD (kPa)"
  )


# Hr(55) sur les 4 annees de suivies #

ggplot() +
  geom_line(data = humidity2020, aes(x= date, y= `Hr(55)`),colour = "#1B9E77") +
  geom_line(data = humidity2021, aes(x= date, y= `Hr(55)`), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x= date, y= `Hr(55)`), colour = "#7570B3") +
  geom_line(data = humidity2023, aes(x= date, y= `Hr(55)`), colour = "#E7298A") +
  geom_line(data = humidity2024, aes(x= date, y= `Hr(55)`), colour = "#66A61E") +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
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
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  
  labs(
    title = "Humidité de l'air journalière au cours des 4 années de suivies phénologique",
    x = "Dates",
    y = "Humidité de l'air (%)"
  )


# ETP sur les 4 annees de suivies #

ggplot() +
  geom_line(data = humidity2020, aes(x= date, y= ETP),colour = "#1B9E77") +
  geom_line(data = humidity2021, aes(x= date, y= ETP), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x= date, y= ETP), colour = "#7570B3") +
  geom_line(data = humidity2023, aes(x= date, y= ETP), colour = "#E7298A") +
  geom_line(data = humidity2024, aes(x= date, y= ETP), colour = "#66A61E") +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
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
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  
  labs(
    title = "Evapotranspiration potentielle journalière au cours des 4 années de suivies phénologique",
    x = "Dates",
    y = "ETP (mm)"
  )


# VWC_10cm sur les 4 annees de suivies #

ggplot() +
  geom_line(data = humidity2020, aes(x= date, y= VWC_10cm),colour = "#1B9E77") +
  geom_line(data = humidity2021, aes(x= date, y= VWC_10cm), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x= date, y= VWC_10cm), colour = "#7570B3") +
  geom_line(data = humidity2023, aes(x= date, y= VWC_10cm), colour = "#E7298A") +
  geom_line(data = humidity2024, aes(x= date, y= VWC_10cm), colour = "#66A61E") +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
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
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  
  labs(
    title = "Humidité du sol journalière au cours des 4 années de suivies phénologique",
    x = "Dates",
    y = "VWC_10cm (m³/m³)"
  )


# vpd55 sur les 4 annees de suivies #

ggplot() +
  geom_line(data = humidity2020, aes(x= date, y= vpd55),colour = "#1B9E77") +
  geom_line(data = humidity2021, aes(x= date, y= vpd55), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x= date, y= vpd55), colour = "#7570B3") +
  geom_line(data = humidity2023, aes(x= date, y= vpd55), colour = "#E7298A") +
  geom_line(data = humidity2024, aes(x= date, y= VWC_10cm), colour = "#66A61E") +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
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
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  
  labs(
    title = "Déficit de pression de vapeur journalier au cours des 4 années de suivies phénologique",
    x = "Dates",
    y = "VPD (m³/m³)"
  )
