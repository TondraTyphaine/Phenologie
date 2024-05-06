# Titre : Analyses des donnees meteorologiques
# Auteur : Tondra Typhaine
# Date de creation : 03/05/2024
# Derniere modification : 03/05/2024


## Package necessaire
install.packages("tidyverse")
library(tidyverse)

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
  print() ->
  data2020

date2020= seq.Date(as.Date(paste(2020, "01-01", sep="-")), 
                   as.Date(paste(2020, "12-31", sep="-")), 
                   by="1 day")
date2020= data.frame(Jour_de_l_annee = 1:366, Date = date2020)

data2020 %>% 
  mutate(Date = date2020$Date) %>%
  print() ->
  data2020

seq(as.Date("2020-01-01", sep="-"), as.Date("2020-01-31", sep="-"), by="day")

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
  print() ->
  data2021

date2021 <- seq(as.Date(paste(2021, "01-01", sep="-")), as.Date(paste(2021, "12-31", sep="-")), by="day")
date2021<- data.frame(date= date2021)

data2021 %>% 
  mutate(Date= date2021) %>% 
  print()->
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
  print() ->
  data2023


## Empilement des donnees
bind_rows(data2020) %>% 
  bind_rows(data2021) %>% 
  bind_rows(data2022) %>% 
  bind_rows(data2023) ->
  data_resume

## Creation d'une colonne date
data_resume %>% 
  mutate(Date = as.Date(with(data_resume, paste(Year,Month, Day, sep = "/")))) %>% 
  print() ->
  data_resume

df$date <- as.Date(with(df, paste(year, month, day, sep="-")))  




  
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
  print() ->
  dataB2021

date2021 <- seq(as.Date(paste(2021, "01-01", sep="-")), as.Date(paste(2021, "12-31", sep="-")), by="day")
date2021<- data.frame(date= date2021)

data2021 %>% 
  mutate(Date= date2021) %>% 
  print()->
  data2021

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







