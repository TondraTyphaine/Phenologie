# Titre : Analyses des donnees meteorologiques
# Auteur : Tondra Typhaine
# Date de creation : 03/05/2024
# Derniere modification : 03/05/2024


# Package necessaire
install.packages("tidyverse")
library(tidyverse)

# Data
data<- read_csv2("data/TourAFlux-2004-2023.csv")

# Reduction des donnees
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

# Moyenne des variables par jour par annee

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

# Empilement des donnees
bind_rows(data2020) %>% 
  bind_rows(data2021) %>% 
  bind_rows(data2022) %>% 
  bind_rows(data2023) ->
  data_resume



  
  
  
  
  
  
  
  