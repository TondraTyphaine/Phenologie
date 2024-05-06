# Titre : Analyses des donnees meteorologiques
# Auteur : Tondra Typhaine
# Date de creation : 03/05/2024
# Derniere modification : 03/05/2024


## Packages necessaires
install.packages("tidyverse")
install.packages("lubridate")
install.packages("RColorBrewer")
library(tidyverse)
library(lubridate)
library(RColorBrewer)

#### Avec les donnees de M.Badouard 

## Data 
data<- read_csv2("data/TourAFlux-2004-2023.csv")

#" Reduction des donnees
data %>% 
  filter(Year == 2020 : 2023) %>% 
  select(Year,Month, Day,
         `Temp(55)`, `Hr(55)`, `Temp(30)`, `Hr(30)`, `Temp(2.5)`, `Hr(2.5)`, 
         vpd55,
         Rain, 
         `ETP penman (mm h-1) - OK`,
         `ETP (mm h-1) NEW CNR4`,
         CNR4_temp,
         TempC_55_CR3000,
         RH_55_CR3000) ->
  data


## Moyenne des variables par jour par annee et ajout colonne date

# Pour 2020
data %>% 
  filter(Year == 2020) %>% 
  group_by(Year, Month, Day) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            `Temp(30)`= mean(`Temp(30)`),
            `Hr(30)`= mean(`Hr(30)`),
            `Temp(2.5)`= mean(`Temp(2.5)`),
            `Hr(2.5)`= mean(`Hr(2.5)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
            `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
            CNR4_temp= mean(CNR4_temp),
            TempC_55_CR3000= mean(TempC_55_CR3000),
            RH_55_CR3000= mean(RH_55_CR3000))%>% 
  mutate(date = ymd("2020-01-01") + days(Day - 1)) %>%
  print() ->
  data2020


# Pour 2021
data %>% 
  filter(Year == 2021) %>% 
  group_by(Year, Month, Day) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            `Temp(30)`= mean(`Temp(30)`),
            `Hr(30)`= mean(`Hr(30)`),
            `Temp(2.5)`= mean(`Temp(2.5)`),
            `Hr(2.5)`= mean(`Hr(2.5)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
            `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
            CNR4_temp= mean(CNR4_temp),
            TempC_55_CR3000= mean(TempC_55_CR3000),
            RH_55_CR3000= mean(RH_55_CR3000))%>%
  mutate(date = ymd("2021-01-01") + days(Day - 1)) %>%
  print() ->
  data2021

# Pour 2022
data %>% 
  filter(Year == 2022) %>% 
  group_by(Year, Month, Day) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            `Temp(30)`= mean(`Temp(30)`),
            `Hr(30)`= mean(`Hr(30)`),
            `Temp(2.5)`= mean(`Temp(2.5)`),
            `Hr(2.5)`= mean(`Hr(2.5)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
            `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
            CNR4_temp= mean(CNR4_temp),
            TempC_55_CR3000= mean(TempC_55_CR3000),
            RH_55_CR3000= mean(RH_55_CR3000))%>% 
  mutate(date = ymd("2022-01-01") + days(Day - 1)) %>%
  print() ->
  data2022

# Pour 2023
data %>% 
  filter(Year == 2023) %>% 
  group_by(Year, Month, Day) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            `Temp(30)`= mean(`Temp(30)`),
            `Hr(30)`= mean(`Hr(30)`),
            `Temp(2.5)`= mean(`Temp(2.5)`),
            `Hr(2.5)`= mean(`Hr(2.5)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            `ETP penman (mm h-1) - OK`= mean(`ETP penman (mm h-1) - OK`),
            `ETP (mm h-1) NEW CNR4`= mean(`ETP (mm h-1) NEW CNR4`),
            CNR4_temp= mean(CNR4_temp),
            TempC_55_CR3000= mean(TempC_55_CR3000),
            RH_55_CR3000= mean(RH_55_CR3000))%>% 
  mutate(date = ymd("2023-01-01") + days(Day - 1)) %>%
  print() ->
  data2023


## Empilement des donnees
bind_rows(data2020) %>% 
  bind_rows(data2021) %>% 
  bind_rows(data2022) %>% 
  bind_rows(data2023) ->
  data_resume



  
### Avec les donnees de M.Bonal ###

## Data 
dataB<- read_csv2("data/GX-METEO-2020 - 2024E - AK.csv")

## Reduction deu jeu de donnees
dataB %>% 
  select(Year,Month, Day,`J/N`, 
        `Temp(55)`, `Hr(55)`,
        vpd55,
        Rain, 
        ETP,
        VWC_10cm,
        T_10cm) ->
  dataB


## Moyenne des variables quantitatives par jour et ajout colonne date

# Pour 2020

dataB %>% 
  filter(Year == 2020) %>% 
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            ETP = mean(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm))%>% 
  mutate(date = ymd("2020-01-01") + days(Day - 1)) %>% 
  print() ->
  dataB2020

# Pour 2021
dataB %>% 
  filter(Year == 2021) %>% 
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            ETP = mean(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm))%>% 
  mutate(date = ymd("2021-01-01") + days(Day - 1)) %>%
  print() ->
  dataB2021

# Pour 2022
dataB %>% 
  filter(Year == 2022) %>% 
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            ETP = mean(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm))%>% 
  mutate(date = ymd("2022-01-01") + days(Day - 1)) %>%
  print() ->
  dataB2022

# Pour 2023
dataB %>% 
  filter(Year == 2023) %>% 
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            ETP = mean(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm))%>% 
  mutate(date = ymd("2023-01-01") + days(Day - 1)) %>%
  print() ->
  dataB2023

# Pour 2024
dataB %>% 
  filter(Year == 2024) %>% 
  group_by(Year, Month, Day,`J/N`) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),
            `Hr(55)`= mean(`Hr(55)`),
            vpd55= mean(vpd55),
            Rain= mean(Rain),
            ETP = mean(ETP),
            VWC_10cm = mean(VWC_10cm),
            T_10cm = mean(T_10cm))%>% 
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


## PLUIE ##

# Graphique de la pluviometrie par annee

# Vue d'ensemble sur les 4 ans

display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 5, name = "Dark2")

ggplot() +
  geom_line(aes(x= dataB2020$date, y= dataB2020$Rain), data = dataB2020, colour = "#1B9E77")+
  geom_line(aes(x= dataB2021$date, y= dataB2021$Rain), data = dataB2021, colour = "#D95F02")+
  geom_line(aes(x= dataB2022$date, y= dataB2022$Rain),data = dataB2022, colour = "#7570B3")+
  geom_line(aes(x= dataB2023$date, y= dataB2023$Rain),data = dataB2023, colour = "#E7298A")+
  geom_line(aes(x= dataB2024$date, y= dataB2024$Rain),data = dataB2024, colour = "#66A61E")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours des 4 années de suivies phénologique",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2020
ggplot() +
  geom_line(aes(x= dataB2020$date, y= dataB2020$Rain), data = dataB2020, colour = "#1B9E77")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2020",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2021
ggplot()+
  geom_line(aes(x= dataB2021$date, y= dataB2021$Rain), data = dataB2021, colour = "#D95F02")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2021",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2022
ggplot()+
  geom_line(aes(x= dataB2022$date, y= dataB2022$Rain),data = dataB2022, colour = "#7570B3")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2022",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2023
ggplot()+
  geom_line(aes(x= dataB2023$date, y= dataB2023$Rain),data = dataB2023, colour = "#E7298A")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2023",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2024
ggplot()+
  geom_line(aes(x= dataB2024$date, y= dataB2024$Rain),data = dataB2024, colour = "#66A61E")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2024",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )



