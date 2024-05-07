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

## Graphiques de la pluviometrie 

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
ggplot(dataB2020, aes(x = date, y = Rain)) +
  geom_line(colour = "#1B9E77") +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie au cours de l'année 2020",
    x = "Dates",
    y = "Pluviométrie (mm)"
  )

# Pour 2021
ggplot(dataB2021, aes(x = date, y = Rain))+
  geom_line(colour = "#D95F02")+
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
ggplot(dataB2022, aes(x = date, y = Rain))+
  geom_line( colour = "#7570B3")+
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
ggplot(dataB2023, aes(x = date, y = Rain))+
  geom_line(colour = "#E7298A")+
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
ggplot(dataB2024, aes(x = date, y = Rain))+
  geom_line(colour = "#66A61E")+
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



