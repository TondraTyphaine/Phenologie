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

## Moyenne de la pluviométrie entre le jour et la nuit

# Pour 2020
dataB2020 %>% 
  select(Year, Month, Day,Rain,date) %>% 
  group_by(Year, Month, Day,date) %>% 
  summarise(Rain = mean(Rain)) %>% 
  print ->
  Rain2020
  
# Pour 2021
dataB2021 %>% 
  select(Year, Month, Day,Rain,date) %>% 
  group_by(Year, Month, Day,date) %>% 
  summarise(Rain = mean(Rain)) %>% 
  print ->
  Rain2021

# Pour 2022
dataB2022 %>% 
  select(Year, Month, Day,Rain,date) %>% 
  group_by(Year, Month, Day,date) %>% 
  summarise(Rain = mean(Rain)) %>% 
  print ->
  Rain2022

# Pour 2023
dataB2023 %>% 
  select(Year, Month, Day,Rain,date) %>% 
  group_by(Year, Month, Day,date) %>% 
  summarise(Rain = mean(Rain)) %>% 
  print ->
  Rain2023

# Pour 2024
dataB2024 %>%
  filter(!is.na(Rain)) %>% 
  select(Year, Month, Day,Rain,date) %>% 
  group_by(Year, Month, Day,date) %>% 
  summarise(Rain = mean(Rain)) %>% 
  print ->
  Rain2024


## Trouver les pics de pluie

# Pour 2020
pic2020 <- sort(findpeaks(Rain2020$Rain, minpeakheight = 2, nups = 1)[,2])
pic2020_Y <- sort(findpeaks(Rain2020$Rain, minpeakheight = 2.05, nups = )[,1])
dates2020 <- unique(Rain2020$date)
  
# Pour 2021
pic2021 <- sort(findpeaks(Rain2021$Rain, minpeakheight = 2, nups = 1)[,2])
pic2021_Y <- sort(findpeaks(Rain2021$Rain, minpeakheight = 2.4, nups = )[,1])
dates2021 <- unique(Rain2021$date)

# Pour 2022
pic2022 <- sort(findpeaks(Rain2022$Rain, minpeakheight = 1.75, nups = 1)[,2])
pic2022_Y <- sort(findpeaks(Rain2022$Rain, minpeakheight = 1.9, nups = )[,1])
dates2022 <- unique(Rain2022$date)

# Pour 2023
pic2023 <- sort(findpeaks(Rain2023$Rain, minpeakheight = 2, nups = 1)[,2])
pic2023_Y <- sort(findpeaks(Rain2023$Rain, minpeakheight = 2.1, nups = )[,1])
dates2023 <- unique(Rain2023$date)

# Pour 2024
pic2024 <- sort(findpeaks(Rain2024$Rain, minpeakheight = 0.50, nups = 1)[,2])
pic2024_Y <- sort(findpeaks(Rain2024$Rain, minpeakheight = 0.6, nups = 1 )[,1])
dates2024 <- Rain2024$date


## Graphiques de la pluviometrie 

# Vue d'ensemble sur les 4 ans

display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 5, name = "Dark2")

ggplot() +
  geom_line(data = Rain2020, aes(x= date, y= Rain),colour = "#1B9E77")+
  geom_line(data = Rain2021, aes(x= date, y= Rain), colour = "#D95F02")+
  geom_line(data = Rain2022, aes(x= date, y= Rain), colour = "#7570B3")+
  geom_line(data = Rain2023, aes(x= date, y= Rain), colour = "#E7298A")+
  geom_line(data = Rain2024, aes(x= date, y= Rain), colour = "#66A61E")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_vline(xintercept = dates2020[117],
             col = "black", linetype = "dashed")+
  geom_vline(xintercept = dates2021[152],
             col = "black", linetype = "dashed")+
  geom_hline(yintercept = pic2020_Y, 
             col = "#1B9E77", linetype = "dashed")+
  geom_hline(yintercept = pic2021_Y, 
             col = "#D95F02", linetype = "dashed")+
  geom_hline(yintercept = pic2022_Y, 
             col = "#7570B3", linetype = "dashed")+
  geom_hline(yintercept = pic2023_Y, 
             col = "#E7298A", linetype = "dashed")+
  geom_hline(yintercept = pic2024_Y, 
             col = "#66A61E", linetype = "dashed")+
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
  annotate("text",x = dates2020[117], 
           y= pic2020_Y,label = round(pic2020_Y, digits = 2),
           col = "black", size = 3)+
  annotate("text",x = dates2021[152], 
           y= pic2021_Y,label = round(pic2021_Y, digits = 2),
           col = "black", size = 3)+
  annotate("text",x = dates2022[152], 
           y= pic2022_Y,label = round(pic2022_Y, digits = 2),
           col = "black", size = 3)+
  annotate("text",x = dates2023[111], 
           y= pic2023_Y,label = round(pic2023_Y, digits = 2),
           col = "black", size = 3)+
  annotate("text",x = dates2023[57], 
           y= pic2023_Y,label = round(pic2023_Y, digits = 2),
           col = "black", size = 3)+
  annotate("text",x = dates2024[14], 
           y= pic2024_Y,label = round(pic2024_Y, digits = 2),
           col = "black", size = 3)+
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

  # Scale_x_date Pour specifier que des dates sont utilisees en abscisse 
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



