# Titre : Analyses des donnees meteorologiques
# Auteur : Tondra Typhaine
# Date de creation : 03/05/2024


## Packages necessaires
install.packages("pacman")
pacman::p_load("tidyverse","lubridate","RColorBrewer","pracma", "vioplot","lmtest")

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

## PLUIE ET TEMPERATURES ##

## Somme de la pluviométrie entre le jour et la nuit et moyenne des temperatures

# Full data par jour
dataB_resume %>% 
  select(Year, Month, date, Rain,`Temp(55)`) %>% 
  group_by(Year, Month, date) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  print() ->
  Rain

# Full data par mois
Rain %>% 
  group_by(Year, Month) %>% 
  summarise(Rain = sum(Rain), `Temp(55)`= mean(`Temp(55)`)) %>%
  print() ->
  Rain_month

# Pour 2020
Rain %>% 
  filter(Year == 2020) %>% 
  print() ->
  Rain2020

# Pour 2021
Rain %>% 
  filter(Year == 2021) %>% 
  print() ->
  Rain2021

# Pour 2022
Rain %>% 
  filter(Year == 2022) %>% 
  print() ->
  Rain2022

# Pour 2023
Rain %>% 
  filter(Year == 2023) %>% 
  print() ->
  Rain2023

# Pour 2024
Rain %>% 
  filter(Year == 2024) %>% 
  print() ->
  Rain2024


## Identifier les mois secs selon la definition de Banyuls et Gaussen (P inferieure ou egale a deux fois la °C)

# Pour 2020 #

# Data par jour
Rain2020

# Data par mois
Rain_month %>%
  filter(Year == 2020) %>% 
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%

  # Pour arrondir les dates au 1er de chaque mois (specifie par "unit = month")
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  Dry_2020

# Pic maximal journalier de pluie sur l'annee
pic2020_X <- which.max(Rain2020$Rain) # Position du pic
pic2020_Y <- max(Rain2020$Rain) # Valeur du pic
dates2020 <- unique(Rain2020$date) # Extraction des dates

# Pic maximal mensuel de pluie sur l'annee
max2020 <- which.max(Dry_2020$Rain) # Position du pic

#Pic minimum mensuel sur l'annee
min2020 <- which.min(Dry_2020$Rain)

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2020, aes(x = date, y = Rain),colour = "#E6AB02")+
  geom_line(data = Rain2020, aes(x = date, y = `Temp(55)`), colour = "red") +
  geom_point(data = Rain2020, aes(x = Rain2020$date[pic2020_X], y = pic2020_Y), 
             colour = "grey40", size = 0.8) +
  annotate("text", x = Rain2020$date[pic2020_X] , y = pic2020_Y+5, 
           label = pic2020_Y, colour = "grey40", size = 3.5) +
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
  geom_line(data = Dry_2020, aes(x = date , y = Rain),,colour = "#E6AB02")+
  geom_line(data = Dry_2020, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = Dry_2020, aes(x = date[max2020], y = Rain[max2020]),colour = "grey40", size = 0.8)+
  geom_point(data = Dry_2020, aes(x = date[min2020], y = Rain[min2020]),colour = "grey40", size = 0.8)+
  annotate("text",x = Dry_2020$date[max2020], 
           y =  Dry_2020$Rain[max2020]+20,label = Dry_2020$Rain[max2020],
           col = "grey40", size = 3)+
  annotate("text",x = Dry_2020$date[min2020], 
           y = Dry_2020$Rain[min2020]-20,label = Dry_2020$Rain[min2020],
           col = "grey40", size = 3.5)+
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2020",
    x = "Mois"
    )


# Pour 2021 #

# Data par jour
Rain2021

# Data par mois
Rain_month %>%
  filter(Year == 2021) %>% 
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  Dry_2021

# Pic maximal de pluie journalier sur l'annee
pic2021_X <- which.max(Rain2021$Rain)
pic2021_Y <- max(Rain2021$Rain)
dates2021 <- Rain2021$date

# Pic maximal mensuel de pluie sur l'annee
max2021 <- which.max(Dry_2021$Rain) # Position du pic

#Pic minimum mensuel sur l'annee
min2021 <- which.min(Dry_2021$Rain)

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2021, aes(x = date, y = Rain),colour = "#D95F02")+
  geom_line(data = Rain2021, aes(x = date, y = `Temp(55)`), colour = "red") +
  geom_point(data = Rain2021, aes(x = Rain2021$date[pic2021_X], y = pic2021_Y), 
             colour = "grey40", size = 0.8) +
  annotate("text", x = Rain2021$date[pic2021_X] , y = pic2021_Y+5, 
           label = pic2021_Y, colour = "grey40", size = 3.5) +
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
  geom_line(data = Dry_2021, aes(x = date , y = Rain),,colour = "#D95F02")+
  geom_line(data = Dry_2021, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = Dry_2021, aes(x = date[max2021], y = Rain[max2021]),colour = "grey40", size = 0.8)+
  geom_point(data = Dry_2021, aes(x = date[min2021], y = Rain[min2021]),colour = "grey40", size = 0.8)+
  annotate("text",x = Dry_2021$date[max2021], 
           y = Dry_2021$Rain[max2021]+20,label = Dry_2021$Rain[max2021],
           col = "grey40", size = 3)+
  annotate("text",x = Dry_2021$date[min2021], 
           y =  Dry_2021$ Rain[min2021]-20,label =  Dry_2021$ Rain[min2021],
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
Rain_month %>%
  filter(Year == 2022) %>% 
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  Dry_2022

# Pic maximal de pluie journalier sur l'annee
pic2022_X <- which.max(Rain2022$Rain)
pic2022_Y <- max(Rain2022$Rain)
dates2022 <- Rain2022$date

# Pic maximal mensuel de pluie sur l'annee
max2022 <- which.max(Dry_2022$Rain) # Position du pic

#Pic minimum mensuel sur l'annee
min2022 <- which.min(Dry_2022$Rain)


# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2022, aes(x = date, y = Rain),colour = "#E7298A")+
  geom_line(data = Rain2022, aes(x = date, y = `Temp(55)`), colour = "red") +
  geom_point(data = Rain2022, aes(x = Rain2022$date[pic2022_X], y = pic2022_Y), 
             colour = "grey40", size = 0.8) +
  annotate("text", x = Rain2022$date[pic2022_X] , y = pic2022_Y+5, 
           label = pic2022_Y, colour = "grey40", size = 3.5) +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2022",
    x = "Dates"
  )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Dry_2022, aes(x = date , y = Rain),colour = "#E7298A")+
  geom_line(data = Dry_2022, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = Dry_2022, aes(x = date[max2022], y = Rain[max2022]),colour = "grey40", size = 0.8)+
  geom_point(data = Dry_2022, aes(x = date[min2022], y = Rain[min2022]),colour = "grey40", size = 0.8)+
  annotate("text",x = Dry_2022$date[max2022], 
           y =  Dry_2022$Rain[max2022]+20,label = Dry_2022$Rain[max2022],
           col = "grey40", size = 3)+
  annotate("text",x = Dry_2022$date[min2022], 
           y = Dry_2022$Rain[min2022]-20,label =  Dry_2022$Rain[min2022],
           col = "grey40", size = 3)+
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01",
                                  "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01",
                                  "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2022"
  )


# Pour 2023 #

# Data par jour
Rain2023

# Data par mois
Rain_month %>%
  filter(Year == 2023) %>% 
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  Dry_2023

# Pic maximum journalier de pluie dans l'annee
pic2023_X <- which.max(Rain2023$Rain)
pic2023_Y <- max(Rain2023$Rain)
dates2023 <- Rain2023$date

# Pic maximal mensuel de pluie sur l'annee
max2023 <- which.max(Dry_2023$Rain) # Position du pic

#Pic minimum mensuel sur l'annee
min2023 <- which.min(Dry_2023$Rain)

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2023, aes(x = date, y = Rain),colour = "#7570B3")+
  geom_line(data = Rain2023, aes(x = date, y = `Temp(55)`), colour = "red") +
  geom_point(data = Rain2023, aes(x = Rain2023$date[pic2023_X], y = pic2023_Y), 
             colour = "grey40", size = 0.8) +
  annotate("text", x = Rain2023$date[pic2023_X] , y = pic2023_Y+5, 
           label = pic2023_Y, colour = "grey40", size = 3.5) +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01",
                                  "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01",
                                  "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2023",
    x = "Dates"
  )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Dry_2023, aes(x = date , y = Rain),colour = "#7570B3")+
  geom_line(data = Dry_2023, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = Dry_2023, aes(x = date[max2023], y = Rain[max2023]),colour = "grey40", size = 0.8)+
  geom_point(data = Dry_2023, aes(x = date[min2023], y = Rain[min2023]),colour = "grey40", size = 0.8)+
  annotate("text",x = Dry_2023$date[max2023], 
           y = Dry_2023$Rain[max2023]+20,label = Dry_2023$Rain[max2023],
           col = "grey40", size = 3)+
  annotate("text",x = Dry_2023$date[min2023], 
           y =  Dry_2023$Rain[min2023]-20,label =  Dry_2023$Rain[min2023],
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
Rain_month %>%
  filter(Year == 2024) %>% 
  mutate(Temp_X2 = 2 * `Temp(55)`) %>%
  mutate(Dry_month = Rain <= Temp_X2) %>%
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print ->
  Dry_2024

# Pic maximal journalier de pluie sur l'annee
pic2024_X <- which.max(Rain2024$Rain)
pic2024_Y <- max(Rain2024$Rain)
dates2024 <- Rain2024$date

# Pic maximal mensuel de pluie sur l'annee
max2024 <- which.max(Dry_2024$Rain)

#Pic minimum mensuel sur l'annee
min2024 <- which.min(Dry_2024$Rain)

# Graphique comparant la pluviometrie journaliere et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Rain2024, aes(x = date, y = Rain),colour = "#66A61E")+
  geom_line(data = Rain2024, aes(x = date, y = `Temp(55)`), colour = "red") +
  geom_point(data = Rain2024, aes(x = Rain2024$date[pic2024_X], y = pic2024_Y), 
             colour = "grey40", size = 0.8) +
  annotate("text", x = Rain2024$date[pic2024_X] , y = pic2024_Y +1, 
           label = pic2024_Y, colour = "grey40", size = 3.5) +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures journalières au cours de l'année 2024",
    x = "Dates"
  )

# Graphique comparant la pluviometrie moyenne par mois et les temperatures au cours de l'annee
ggplot()+
  geom_line(data = Dry_2024, aes(x = date , y = Rain),colour = "#66A61E")+
  geom_line(data = Dry_2024, aes(x = date , y = `Temp_X2`), colour = "red")+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  geom_point(data = Dry_2024, aes(x = date[max2024], y = Rain[max2024]),colour = "grey40", size = 0.8)+
  geom_point(data = Dry_2024, aes(x = date[min2024], y = Rain[min2024]),colour = "grey40", size = 0.8)+
  annotate("text",x = Dry_2024$date[max2024], 
           y = Dry_2024$Rain[max2024]+5,label = Dry_2024$Rain[max2024],
           col = "grey40", size = 3)+
  annotate("text",x = Dry_2024$date[4], 
           y = Dry_2024$Rain[min2024]+5,label = Dry_2024$Rain[min2024],
           col = "grey40", size = 3)+
  scale_x_date(breaks = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                                  "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                                  "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours de l'année 2024"
  )


# Vue d'ensemble sur les 4 ans #

display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 6, name = "Dark2")
brewer.pal(n = 5, name = "RdBu")
brewer.pal(n = 5, name = "YlGnBu")

dataB_resume %>% 
  select(Year, Month, Day, `Temp(55)`, date) %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`)) %>% 
  print() ->
  `Temp(55)_full`

bind_rows(Dry_2020) %>% 
  bind_rows(Dry_2021) %>% 
  bind_rows(Dry_2022) %>% 
  bind_rows(Dry_2023) %>% 
  bind_rows(Dry_2024) %>% 
  print() ->
  Dry_resum

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
# Journalier
ggplot() +
  geom_line(data = Rain2020, aes(x= date, y= Rain),colour = "#E6AB02") +
  geom_line(data = Rain2021, aes(x= date, y= Rain), colour = "#D95F02") +
  geom_line(data = Rain2022, aes(x= date, y= Rain), colour = "#E7298A") +
  geom_line(data = Rain2023, aes(x= date, y= Rain), colour = "#7570B3") +
  geom_line(data = Rain2024, aes(x= date, y= Rain), colour = "#66A61E") +
  geom_line(data = `Temp(55)_full`, aes(x = date, y = `Temp(55)`), colour = "red") +
  geom_point(aes(x = dates2020[pic2020_X], y =  pic2020_Y), ,colour = "black", size = 0.8) +
  geom_point(aes(x = dates2021[pic2021_X], y =  pic2021_Y), ,colour = "black", size = 0.8) +
  geom_point(aes(x = dates2022[pic2022_X], y =  pic2022_Y), size = 0.9) +
  geom_point(aes(x = dates2023[pic2023_X], y =  pic2023_Y), ,colour = "black", size = 0.8) +
  geom_point(aes(x = dates2024[pic2024_X], y =  pic2024_Y), ,colour = "black", size = 0.8) +
  geom_vline(xintercept = dates2020[pic2020_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2021[pic2021_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[pic2022_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[pic2023_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[pic2024_X],
             col = "black", linetype = "dashed") +
  annotate("text", x = dates2020[pic2020_X]+45, y = pic2020_Y, label = pic2020_Y,col = "black", size = 3) +
  annotate("text", x = dates2021[pic2021_X]+45, y = pic2021_Y, label = pic2021_Y,col = "black", size = 3) +
  annotate("text", x = dates2022[pic2022_X]+45, y = pic2022_Y+1, label = pic2022_Y,col = "black", size = 3) +
  annotate("text", x = dates2023[pic2023_X]+45, y = pic2023_Y, label = pic2023_Y,col = "black", size = 3) +
  annotate("text", x = dates2024[pic2024_X]+45, y = pic2024_Y+1, label = pic2024_Y,col = "black", size = 3) +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température (°C)")) +
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie et températures journalières au cours des 4 années de suivie phénologique",
    x = "Dates",
  )


# Mensuel
bind_rows(Dry_2020) %>% 
  bind_rows(Dry_2021[1,]) %>% 
  print() ->
  Dry_2020_bis

bind_rows(Dry_2021) %>% 
  bind_rows(Dry_2022[1,]) %>% 
  print() ->
  Dry_2021_bis

bind_rows(Dry_2022) %>% 
  bind_rows(Dry_2023[1,]) %>% 
  print() ->
  Dry_2022_bis

bind_rows(Dry_2023) %>% 
  bind_rows(Dry_2024[1,]) %>% 
  print() ->
  Dry_2023_bis


ggplot() + 
  geom_line(data = Dry_2020_bis, aes(x = date, y = Rain),colour = "#E6AB02") +
  geom_line(data = Dry_2021_bis, aes(x= date, y= Rain), colour = "#D95F02") +
  geom_line(data = Dry_2022_bis, aes(x= date, y= Rain), colour = "#E7298A") +
  geom_line(data = Dry_2023_bis, aes(x= date, y= Rain), colour = "#7570B3") +
  geom_line(data = Dry_2024, aes(x= date, y= Rain), colour = "#66A61E") +
  geom_line(data = Dry_resum, aes(x = date, y = Temp_X2), colour = "red") +
  geom_vline(xintercept = dates2021[1],
             col = "grey40", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "grey40", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "grey40", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "grey40", linetype = "dashed") +
  geom_point(aes(x = Dry_2020$date[max2020], y = Dry_2020$Rain[max2020]), size = 0.8)+
  geom_point(aes(x = Dry_2020$date[min2020], y = Dry_2020$Rain[min2020]), size = 0.8)+
  geom_point(aes(x = Dry_2021$date[max2021], y = Dry_2021$Rain[max2021]), size = 0.8)+
  geom_point(aes(x = Dry_2021$date[min2021], y = Dry_2021$Rain[min2021]), size = 0.8)+
  geom_point(aes(x = Dry_2022$date[max2022], y = Dry_2022$Rain[max2022]), size = 0.8)+
  geom_point(aes(x = Dry_2022$date[min2022], y = Dry_2022$Rain[min2022]), size = 0.8)+
  geom_point(aes(x = Dry_2023$date[max2023], y = Dry_2023$Rain[max2023]), size = 0.8)+
  geom_point(aes(x = Dry_2023$date[min2023], y = Dry_2023$Rain[min2023]), size = 0.8)+
  geom_point(aes(x = Dry_2024$date[max2024], y = Dry_2024$Rain[max2024]), size = 0.8)+
  geom_point(aes(x = Dry_2024$date[min2024], y = Dry_2024$Rain[min2024]), size = 0.8)+
  annotate("text",x = Dry_2020$date[max2020], 
           y =  Dry_2020$Rain[max2020]+20,label =  Dry_2020$Rain[max2020],
           col = "black", size = 3)+
  annotate("text",x = Dry_2020$date[min2020], 
           y = Dry_2020$Rain[min2020]-20,label =  Dry_2020$Rain[min2020],
           col = "black", size = 3)+
  annotate("text",x = Dry_2021$date[max2021], 
           y = Dry_2021$Rain[max2021]+20,label = Dry_2021$Rain[max2021],
           col = "black", size = 3)+
  annotate("text",x =  Dry_2021$date[min2021], 
           y =  Dry_2021$Rain[min2021]-20,label =  Dry_2021$Rain[min2021],
           col = "black", size = 3)+
  annotate("text",x = Dry_2022$date[max2022], 
           y = Dry_2022$Rain[max2022]+20,label = Dry_2022$Rain[max2022],
           col = "black", size = 3)+
  annotate("text",x =  Dry_2022$date[min2022], 
           y = Dry_2022$Rain[min2022]-20,label =  Dry_2022$Rain[min2022],
           col = "black", size = 3)+
  annotate("text",x = Dry_2023$date[max2023], 
           y =  Dry_2023$Rain[max2023]+20,label =Dry_2023$Rain[max2023],
           col = "black", size = 3)+
  annotate("text",x = Dry_2023$date[min2023], 
           y =   Dry_2023$Rain[min2023]-20,label =   Dry_2023$Rain[min2023],
           col = "black", size = 3)+
  annotate("text",x = Dry_2024$date[max2024]+30, 
           y = Dry_2024$Rain[max2024],label = Dry_2024$Rain[max2024],
           col = "black", size = 3)+
  annotate("text",x = Dry_2024$date[min2024], 
           y = Dry_2024$Rain[min2024]-20,label =  Dry_2024$Rain[min2024],
           col = "black", size = 3)+
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Température X2 (°C)")) +
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Pluviométrie et températures mensuelles au cours des 4 années de suivie phénologique",
    x = "Mois"
    )






## HUMIDITE DE L'AIR (Hr55), ETP, HUMIDITE DU SOL (VWC_10cm) ET DEFICIT DE SATURATION (vpd55) ##

# Data des 4 ans #

# Data de depart
dataB_resume %>% 
  select(Year, Month, Day, date,`J/N`, `Hr(55)`, ETP, VWC_10cm, vpd55) %>% 
  print() ->
  dataB_hum

sum(is.na(dataB_hum))

# Data 24h
dataB_hum %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  print() ->
  humidity

#Application de la méthode de la moyenne mobile sur les data 24h

humidity_av <- moving_average(humidity %>% 
                                      select(`Hr(55)`) %>%
                                      pull(),
                                    filter = fpoids(n=2,p=2,q=2)$y)


# Data de jour 
dataB_hum %>% 
  filter(`J/N` == "J") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity_J

# Data de nuit
dataB_hum %>%
  filter(`J/N` == "N") %>% 
  group_by(Year, Month, Day, date) %>% 
  print() ->
  humidity_N

# Data mois par mois
dataB_hum %>% 
  group_by(Year, Month) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print() ->
  humidity_month

# Data mois par mois de jour
humidity_J %>% 
  group_by(Year, Month) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print()->
  humidity_month_J

# Data mois par mois de nuit
humidity_N %>% 
  group_by(Year, Month) %>% 
  summarise(`Hr(55)` = mean(`Hr(55)`), ETP = sum(ETP), VWC_10cm = mean(VWC_10cm), vpd55 = mean(vpd55)) %>% 
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print()->
  humidity_month_N

# Data pour chaque mois (pas de distinction J/N)
humidity %>% 
  filter(Month == 1) %>% 
  print() ->
  humidity_01

humidity %>% 
  filter(Month == 2) %>% 
  print() ->
  humidity_02

humidity %>% 
  filter(Month == 3) %>% 
  print() ->
  humidity_03

humidity %>% 
  filter(Month == 4) %>% 
  print() ->
  humidity_04

humidity %>% 
  filter(Month == 5) %>% 
  print() ->
  humidity_05

humidity %>% 
  filter(Month == 6) %>% 
  print() ->
  humidity_06

humidity %>% 
  filter(Month == 7) %>% 
  print() ->
  humidity_07

humidity %>% 
  filter(Month == 8) %>% 
  print() ->
  humidity_08

humidity %>% 
  filter(Month == 9) %>% 
  print() ->
  humidity_09

humidity %>% 
  filter(Month == 10) %>% 
  print() ->
  humidity_10

humidity %>% 
  filter(Month == 11) %>% 
  print() ->
  humidity_11

humidity %>% 
  filter(Month == 12) %>% 
  print() ->
  humidity_12


# Pour 2020 #
dataB_hum %>% 
  filter(Year == 2020) %>% 
  print() ->
  dataB2020_hum

# Data journalières
humidity %>% 
  filter(Year == 2020) %>% 
  print() ->
  humidity2020

humidity_J %>% 
  filter(Year == 2020) %>% 
  print() ->
  humidity2020_J 

humidity_N %>% 
  filter(Year == 2020) %>% 
  print() ->
  humidity2020_N

#Data mensuelles générales, de jour et de nuit
humidity_month %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_month2020

humidity_month_J %>% 
  filter(Year ==2020) %>% 
  print()->
  humidity_month2020_J

humidity_month_N %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_month2020_N

# Data pour chaque jour de chaque mois (pas de distinction J/N)
humidity_01 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_01_2020

humidity_02 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_02_2020

humidity_03 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_03_2020

humidity_04 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_04_2020

humidity_05 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_05_2020

humidity_06 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_06_2020

humidity_07 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_07_2020

humidity_08 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_08_2020

humidity_09 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_09_2020

humidity_10 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_10_2020

humidity_11 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_11_2020

humidity_12 %>% 
  filter(Year == 2020) %>% 
  print()->
  humidity_12_2020

# Data pour chaque jour de chaque mois de jour
humidity2020_J %>% 
  filter(Month == 1) %>% 
  print() ->
  humidity_01_2020_J


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


# HR #

# Pic maximal journalier  sur l'annee
picHR2020_X <- which.max(humidity2020$`Hr(55)`) #position
picHR2020_Y <- max(humidity2020$`Hr(55)`) #valeur
dates2020 <- humidity2020$date

# Pic minimal journalier  sur l'annee
picminHR2020_X <- which.min(humidity2020$`Hr(55)`)
picminHR2020_Y <- min(humidity2020$`Hr(55)`)

# Pic maximal mensuel de pluie sur l'annee
maxHR2020_X <- which.max(humidity_month2020$`Hr(55)`)
maxHR2020_Y <- max(humidity_month2020$`Hr(55)`)
dates2020_bis <-  humidity_month2020$date

#Pic minimum mensuel sur l'annee
minHR2020_X <- which.min(humidity_month2020$`Hr(55)`)
minHR2020_Y <- min(humidity_month2020$`Hr(55)`)


# Graphique Hr(55) journalier J/N V.1
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


# Graphique Hr(55) journalier J/N V.2(moyenne mobile)
source("Source_custom_functions/Func_dataPrepExplo.R")
source("Source_custom_functions/Func_analyse.R")
source("Source_custom_functions/myTheme.R")

humidity2020_J_av <- moving_average(humidity2020_J %>% 
                                      select(`Hr(55)`) %>%
                                      pull(),
                                    filter = fpoids(n=2,p=2,q=2)$y)

humidity2020_N_av <- moving_average(humidity2020_N %>% 
                                      select(`Hr(55)`) %>%
                                      pull(),
                                    filter = fpoids(n=2,p=2,q=2)$y)

ggplot() +
  geom_line(data = humidity2020_J, aes(x =date, y = humidity2020_J_av, color = "J")) +
  geom_line(data = humidity2020_N, aes(x =date, y = humidity2020_N_av, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Variation de l'humidité de l'air de jour et de nuit en 2020 ",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )


# Graphique Hr(55) journalier J/N V.3
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

# Graphique Hr(55) mensuel J/N V.4 
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity_month2020_J, aes(x = date, y = `Hr(55)`, color = "J")) +
  geom_line(data = humidity_month2020_N, aes(x = date, y = `Hr(55)`, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air mensuelle de jour et de nuit",
    x = "Dates",
    y = "Humidité de l'air à 55m (%)"
  )

# Graphique Hr(55) mensuel V.5 
with(humidity2020 , {
  vioplot( 
    `Hr(55)`[Month== 1] ,`Hr(55)`[Month== 2] , `Hr(55)`[Month== 3] , `Hr(55)`[Month== 4] ,`Hr(55)`[Month== 5] ,
    `Hr(55)`[Month== 6] ,`Hr(55)`[Month== 7] ,`Hr(55)`[Month== 8] ,`Hr(55)`[Month== 9] ,`Hr(55)`[Month== 10] ,
    `Hr(55)`[Month== 11] ,`Hr(55)`[Month== 12] ,
    col= "#E6AB02" , names=c("Janvier","Février","Mars",
                                       "Avril","Mai","Juin",
                                       "Juillet","Août","Septembre",
                                       "Octobre","Novembre","Décembre"),
    xlab = "Mois",
    ylab = "Humidité de l'air (%)",
    main = "Humidité de l'air à 55m en 2022"
  )
 })



# ETP #

# Pic maximal journalier  sur l'annee
picETP2020_X <- which.max(humidity2020$ETP) #position
picETP2020_Y <- max(humidity2020$ETP) #valeur
dates2020 <- humidity2020$date

# Pic minimal journalier  sur l'annee
picminETP2020_X <- which.min(humidity2020$ETP)
picminETP2020_Y <- min(humidity2020$ETP)

# Pic maximal mensuel sur l'annee
maxETP2020_X <- which.max(humidity_month2020$ETP)
maxETP2020_Y <- max(humidity_month2020$ETP)
dates2020_bis <-  humidity_month2020$date

#Pic minimum mensuel sur l'annee
minETP2020_X <- which.min(humidity_month2020$ETP)
minETP2020_Y <- min(humidity_month2020$ETP)

# Graphique variations journalieres
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
    title = "Evapotranspiration potentielle journalière de jour et de nuit",
    x = "Dates",
    y = "ETP (mm)"
  )

# Graphique variations mensuelles
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity_month2020_J, aes(x = date, y = ETP, color = "J")) +
  geom_line(data = humidity_month2020_N, aes(x = date, y = ETP, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Evapotranspiration potentielle mensuelle de jour et de nuit",
    x = "Dates",
    y = "ETP (mm)"
  )


# VWC_10cm #

# Pic maximal journalier  sur l'annee
picVWC2020_X <- which.max(humidity2020$VWC_10cm) #position
picVWC2020_Y <- max(humidity2020$VWC_10cm) #valeur
dates2020 <- humidity2020$date

# Pic minimal journalier  sur l'annee
picminVWC2020_X <- which.min(humidity2020$VWC_10cm)
picminVWC2020_Y <- min(humidity2020$VWC_10cm)

# Pic maximal mensuel sur l'annee
maxVWC2020_X <- which.max(humidity_month2020$VWC_10cm)
maxVWC2020_Y <- max(humidity_month2020$VWC_10cm)
dates2020_bis <-  humidity_month2020$date

#Pic minimum mensuel sur l'annee
minVWC2020_X <- which.min(humidity_month2020$VWC_10cm)
minVWC2020_Y <- min(humidity_month2020$VWC_10cm)

# Graphique variations journalières
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
    title = "Humidité du sol journalière de jour et de nuit",
    x = "Dates",
    y = "Humidité du sol (m³/m³)"
  )

# Graphique variations mensuelles
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity_month2020_J, aes(x = date, y = VWC_10cm, color = "J")) +
  geom_line(data = humidity_month2020_N, aes(x = date, y = VWC_10cm, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité du sol mensuelle de jour et de nuit",
    x = "Dates",
    y = "Humidité du sol (m³/m³)"
  )


# VPD #

# Pic maximal journalier  sur l'annee
picVPD2020_X <- which.max(humidity2020$vpd55) #position
picVPD2020_Y <- max(humidity2020$vpd55) #valeur
dates2020 <- humidity2020$date

# Pic minimal journalier  sur l'annee
picminVPD2020_X <- which.min(humidity2020$vpd55)
picminVPD2020_Y <- min(humidity2020$vpd55)

# Pic maximal mensuel sur l'annee
maxVPD2020_X <- which.max(humidity_month2020$vpd55)
maxVPD2020_Y <- max(humidity_month2020$vpd55)
dates2020_bis <-  humidity_month2020$date

#Pic minimum mensuel sur l'annee
minVPD2020_X <- which.min(humidity_month2020$vpd55)
minVPD2020_Y <- min(humidity_month2020$vpd55)

# Graphique variations journalieres
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
    title = "Déficit de pression de vapeur journalier de jour et de nuit",
    x = "Dates",
    y = "VPD (kPa)"
  )

# Graphique variations mensuelles
ggplot() +
  #geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "black") +
  geom_line(data = humidity_month2020_J, aes(x = date, y = vpd55, color = "J")) +
  geom_line(data = humidity_month2020_N, aes(x = date, y = vpd55, color = "N")) +
  scale_color_manual(name = "Légende", values = c(J = "red", N ="blue")) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                                  "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01",
                                  "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Déficit de pression de vapeur mensuel de jour et de nuit",
    x = "Dates",
    y = "VPD (kPa)"
  )

# Pour 2021 #

dataB_hum %>% 
  filter(Year == 2021) %>% 
  print() ->
  dataB2021_hum

# Data journalières
humidity %>% 
  filter(Year == 2021) %>% 
  print() ->
  humidity2021

humidity_J %>% 
  filter(Year == 2021) %>% 
  print() ->
  humidity2021_J 

humidity_N %>% 
  filter(Year == 2021) %>% 
  print() ->
  humidity2021_N

#Data mensuelles générales, de jour et de nuit
humidity_month %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_month2021

humidity_month_J %>% 
  filter(Year ==2021) %>% 
  print()->
  humidity_month2021_J

humidity_month_N %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_month2021_N

# Data pour chaque jour de chaque mois (pas de distinction J/N)
humidity_01 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_01_2021

humidity_02 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_02_2021

humidity_03 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_03_2021

humidity_04 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_04_2021

humidity_05 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_05_2021

humidity_06 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_06_2021

humidity_07 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_07_2021

humidity_08 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_08_2021

humidity_09 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_09_2021

humidity_10 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_10_2020

humidity_11 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_11_2021

humidity_12 %>% 
  filter(Year == 2021) %>% 
  print()->
  humidity_12_2021

# Data pour chaque jour de chaque mois de jour
humidity2021_J %>% 
  filter(Month == 1) %>% 
  print() ->
  humidity_01_2021_J

# HR #

# Pic maximal journalier  sur l'annee
picHR2021_X <- which.max(humidity2021$`Hr(55)`) #position
picHR2021_Y <- max(humidity2021$`Hr(55)`) #valeur
dates2021 <- humidity2021$date

# Pic minimal journalier  sur l'annee
picminHR2021_X <- which.min(humidity2021$`Hr(55)`)
picminHR2021_Y <-min(humidity2021$`Hr(55)`)

# Pic maximal mensuel de pluie sur l'annee
maxHR2021_X <- which.max(humidity_month2021$`Hr(55)`)
maxHR2021_Y <- max(humidity_month2021$`Hr(55)`)
dates2021_bis <-  humidity_month2021$date

#Pic minimum mensuel sur l'annee
minHR2021_X <- which.min(humidity_month2021$`Hr(55)`)
minHR2021_Y <- min(humidity_month2021$`Hr(55)`)

# Graphique variations journalieres Hr(55)
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

# ETP #

# Pic maximal journalier  sur l'annee
picETP2021_X <- which.max(humidity2021$ETP) #position
picETP2021_Y <- max(humidity2021$ETP) #valeur

# Pic minimal journalier  sur l'annee
picminETP2021_X <- which.min(humidity2021$ETP)
picminETP2021_Y <- min(humidity2021$ETP)

# Pic maximal mensuel de pluie sur l'annee
maxETP2021_X <- which.max(humidity_month2021$ETP)
maxETP2021_Y <- max(humidity_month2021$ETP)

#Pic minimum mensuel sur l'annee
minETP2021_X <- which.min(humidity_month2021$ETP)
minETP2021_Y <- min(humidity_month2021$ETP)

# Graphique variations journalieres ETP
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


# VWC_10cm #

# Pic maximal journalier  sur l'annee
picVWC2021_X <- which.max(humidity2021$VWC_10cm) #position
picVWC2021_Y <- max(humidity2021$VWC_10cm) #valeur

# Pic minimal journalier  sur l'annee
picminVWC2021_X <- which.min(humidity2021$VWC_10cm)
picminVWC2021_Y <- min(humidity2021$VWC_10cm)

# Pic maximal mensuel de pluie sur l'annee
maxVWC2021_X <- which.max(humidity_month2021$VWC_10cm)
maxVWC2021_Y <- max(humidity_month2021$VWC_10cm)

#Pic minimum mensuel sur l'annee
minVWC2021_X <- which.min(humidity_month2021$VWC_10cm)
minVWC2021_Y <- min(humidity_month2021$VWC_10cm)

# Graphique variations journalieres VWC_10cm
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

# VPD #

# Pic maximal journalier sur l'annee
picVPD2021_X <- which.max(humidity2021$vpd55) #position
picVPD2021_Y <-max(humidity2021$vpd55) #valeur

# Pic minimal journalier  sur l'annee
picminVPD2021_X <- which.min(humidity2021$vpd55)
picminVPD2021_Y <- min(humidity2021$vpd55)

# Pic maximal mensuel de pluie sur l'annee
maxVPD2021_X <- which.max(humidity_month2021$vpd55)
maxVPD2021_Y <- max(humidity_month2021$vpd55)

#Pic minimum mensuel sur l'annee
minVPD2021_X <- which.min(humidity_month2021$vpd55)
minVPD2021_Y <- min(humidity_month2021$vpd55)

# Graphique variations journalieres vpd55
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
dataB_hum %>% 
  filter(Year == 2022) %>% 
  print() ->
  dataB2022_hum

# Data journalières
humidity %>% 
  filter(Year == 2022) %>% 
  print() ->
  humidity2022

humidity_J %>% 
  filter(Year == 2022) %>% 
  print() ->
  humidity2022_J 

humidity_N %>% 
  filter(Year == 2022) %>% 
  print() ->
  humidity2022_N

#Data mensuelles générales, de jour et de nuit
humidity_month %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_month2022

humidity_month_J %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_month2022_J

humidity_month_N %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_month2022_N

# Data pour chaque jour de chaque mois (pas de distinction J/N)
humidity_01 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_01_2022

humidity_02 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_02_2022

humidity_03 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_03_2022

humidity_04 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_04_2022

humidity_05 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_05_2022

humidity_06 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_06_2022

humidity_07 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_07_2022

humidity_08 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_08_2022

humidity_09 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_09_2022

humidity_10 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_10_2022

humidity_11 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_11_2022

humidity_12 %>% 
  filter(Year == 2022) %>% 
  print()->
  humidity_12_2022

# Data pour chaque jour de chaque mois de jour
humidity2022_J %>% 
  filter(Month == 1) %>% 
  print() ->
  humidity_01_2022_J

# HR #

# Pic maximal journalier  sur l'annee
picHR2022_X <- which.max(humidity2022$`Hr(55)`) #position
picHR2022_Y <- max(humidity2022$`Hr(55)`) #valeur
dates2022 <- humidity2022$date

# Pic minimal journalier  sur l'annee
picminHR2022_X <- which.min(humidity2022$`Hr(55)`)
picminHR2022_Y <- min(humidity2022$`Hr(55)`)

# Pic maximal mensuel de pluie sur l'annee
maxHR2022_X <- which.max(humidity_month2022$`Hr(55)`)
maxHR2022_Y <- max(humidity_month2022$`Hr(55)`)
dates2022_bis <-  humidity_month2022$date

#Pic minimum mensuel sur l'annee
minHR2022_X <- which.min(humidity_month2022$`Hr(55)`)
minHR2022_Y <- min(humidity_month2022$`Hr(55)`)

# Graphique variations journalieres Hr(55)
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

# ETP #

# Pic maximal journalier  sur l'annee
picETP2022_X <- which.max(humidity2022$ETP) #position
picETP2022_Y <- max(humidity2022$ETP) #valeur

# Pic minimal journalier  sur l'annee
picminETP2022_X <- which.min(humidity2022$ETP)
picminETP2022_Y <- min(humidity2022$ETP)


# Pic maximal mensuel de pluie sur l'annee
maxETP2022_X <- which.max(humidity_month2022$ETP)
maxETP2022_Y <- max(humidity_month2022$ETP)
                      
#Pic minimum mensuel sur l'annee
minETP2022_X <- which.min(humidity_month2022$ETP)
minETP2022_Y <- min(humidity_month2022$ETP)

# Graphique variations journalieres ETP
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

# VWC_10cm #

# Pic maximal journalier  sur l'annee
picVWC2022_X <- which.max(humidity2022$VWC_10cm) #position
picVWC2022_Y <- max(humidity2022$VWC_10cm) #valeur

# Pic minimal journalier  sur l'annee
picminVWC2022_X <- which.min(humidity2022$VWC_10cm)
picminVWC2022_Y <- min(humidity2022$VWC_10cm)

# Pic maximal mensuel de pluie sur l'annee
maxVWC2022_X <- which.max(humidity_month2022$VWC_10cm)
maxVWC2022_Y <- max(humidity_month2022$VWC_10cm)

#Pic minimum mensuel sur l'annee
minVWC2022_X <- which.min(humidity_month2022$VWC_10cm)
minVWC2022_Y <- min(humidity_month2022$VWC_10cm)

# Graphique variations journalieres VWC_10cm
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

# VPD #

# Pic maximal journalier sur l'annee
picVPD2022_X <- which.max(humidity2022$vpd55) #position
picVPD2022_Y <- max(humidity2022$vpd55) #valeur

# Pic minimal journalier  sur l'annee
picminVPD2022_X <- which.min(humidity2022$vpd55)
picminVPD2022_Y <- min(humidity2022$vpd55)

# Pic maximal mensuel de pluie sur l'annee
maxVPD2022_X <- which.max(humidity_month2022$vpd55)
maxVPD2022_Y <- max(humidity_month2022$vpd55)

#Pic minimum mensuel sur l'annee
minVPD2022_X <- which.min(humidity_month2022$vpd55)
minVPD2022_Y <- min(humidity_month2022$vpd55)

# Graphique variations journalieres vpd55
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
dataB_hum %>% 
  filter(Year == 2023) %>% 
  print() ->
  dataB2023_hum

# Data journalières
humidity %>% 
  filter(Year == 2023) %>% 
  print() ->
  humidity2023

humidity_J %>% 
  filter(Year == 2023) %>% 
  print() ->
  humidity2023_J 

humidity_N %>% 
  filter(Year == 2023) %>% 
  print() ->
  humidity2023_N

#Data mensuelles générales, de jour et de nuit
humidity_month %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_month2023

humidity_month_J %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_month2023_J

humidity_month_N %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_month2023_N

# Data pour chaque jour de chaque mois (pas de distinction J/N)
humidity_01 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_01_2023

humidity_02 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_02_2023

humidity_03 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_03_2023

humidity_04 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_04_2023

humidity_05 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_05_2023

humidity_06 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_06_2023

humidity_07 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_07_2023

humidity_08 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_08_2023

humidity_09 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_09_2023

humidity_10 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_10_2023

humidity_11 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_11_2023

humidity_12 %>% 
  filter(Year == 2023) %>% 
  print()->
  humidity_12_2023

# Data pour chaque jour de chaque mois de jour
humidity2023_J %>% 
  filter(Month == 1) %>% 
  print() ->
  humidity_01_2023_J


# HR #

# Pic maximal journalier  sur l'annee
picHR2023_X <- which.max(humidity2023$`Hr(55)`) #position
picHR2023_Y <- max(humidity2023$`Hr(55)`) #valeur
dates2023 <- humidity2023$date

# Pic minimal journalier  sur l'annee
picminHR2023_X <- which.min(humidity2023$`Hr(55)`)
picminHR2023_Y <- min(humidity2023$`Hr(55)`)

# Pic maximal mensuel de pluie sur l'annee
maxHR2023_X <- which.max(humidity_month2023$`Hr(55)`)
maxHR2023_Y <- max(humidity_month2023$`Hr(55)`)
dates2023_bis <-  humidity_month2023$date

#Pic minimum mensuel sur l'annee
minHR2023_X <- which.min(humidity_month2023$`Hr(55)`)
minHR2023_Y <- min(humidity_month2023$`Hr(55)`)

# Graphique variations journalieres Hr(55)
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

# ETP #

# Pic maximal journalier  sur l'annee
picETP2023_X <- which.max(humidity2023$ETP) #position
picETP2023_Y <- max(humidity2023$ETP) #valeur

# Pic minimal journalier  sur l'annee
picminETP2023_X <- which.min(humidity2023$ETP)
picminETP2023_Y <- min(humidity2023$ETP)

# Pic maximal mensuel de pluie sur l'annee
maxETP2023_X <- which.max(humidity_month2023$ETP)
maxETP2023_Y <- max(humidity_month2023$ETP)

#Pic minimum mensuel sur l'annee
minETP2023_X <- which.min(humidity_month2023$ETP)
minETP2023_Y <- min(humidity_month2023$ETP)

# Graphique variations journalieres ETP
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


# VWC_10cm #

# Pic maximal journalier  sur l'annee
picVWC2023_X <- which.max(humidity2023$VWC_10cm) #position
picVWC2023_Y <- max(humidity2023$VWC_10cm) #valeur

# Pic minimal journalier  sur l'annee
picminVWC2023_X <- which.min(humidity2023$VWC_10cm)
picminVWC2023_Y <- min(humidity2023$VWC_10cm)

# Pic maximal mensuel de pluie sur l'annee
maxVWC2023_X <- which.max(humidity_month2023$VWC_10cm)
maxVWC2023_Y <- max(humidity_month2023$VWC_10cm)

#Pic minimum mensuel sur l'annee
minVWC2023_X <- which.min(humidity_month2023$VWC_10cm)
minVWC2023_Y <- min(humidity_month2023$VWC_10cm)

# Graphique variations journalieres VWC_10cm
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

# VPD #

# Pic maximal journalier sur l'annee
picVPD2023_X <- which.max(humidity2023$vpd55) #position
picVPD2023_Y <- max(humidity2023$vpd55) #valeur

# Pic minimal journalier  sur l'annee
picminVPD2023_X <- which.min(humidity2023$vpd55)
picminVPD2023_Y <- min(humidity2023$vpd55)

# Pic maximal mensuel de pluie sur l'annee
maxVPD2023_X <- which.max(humidity_month2023$vpd55)
maxVPD2023_Y <- max(humidity_month2023$vpd55)

#Pic minimum mensuel sur l'annee
minVPD2023_X <- which.min(humidity_month2023$vpd55)
minVPD2023_Y <- min(humidity_month2023$vpd55)

# Graphique variations journalieres vpd55
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
dataB_hum %>% 
  filter(Year == 2024) %>% 
  print() ->
  dataB2024_hum

# Data journalières
humidity %>% 
  filter(Year == 2024) %>% 
  print() ->
  humidity2024

humidity_J %>% 
  filter(Year == 2024) %>% 
  print() ->
  humidity2024_J 

humidity_N %>% 
  filter(Year == 2024) %>% 
  print() ->
  humidity2024_N

#Data mensuelles générales, de jour et de nuit
humidity_month %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_month2024

humidity_month_J %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_month2024_J

humidity_month_N %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_month2024_N

# Data pour chaque jour de chaque mois (pas de distinction J/N)
humidity_01 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_01_2024

humidity_02 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_02_2024

humidity_03 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_03_2024

humidity_04 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_04_2024

humidity_05 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_05_2024

humidity_06 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_06_2024

humidity_07 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_07_2024

humidity_08 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_08_2024

humidity_09 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_09_2024

humidity_10 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_10_2024

humidity_11 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_11_2024

humidity_12 %>% 
  filter(Year == 2024) %>% 
  print()->
  humidity_12_2024

# Data pour chaque jour de chaque mois de jour
humidity2023_J %>% 
  filter(Month == 1) %>% 
  print() ->
  humidity_01_2024_J

# HR #

# Pic maximal journalier  sur l'annee
picHR2024_X <- which.max(humidity2024$`Hr(55)`) #position
picHR2024_Y <- max(humidity2024$`Hr(55)`) #valeur
dates2024 <- humidity2024$date

# Pic minimal journalier  sur l'annee
picminHR2024_X <- which.min(humidity2024$`Hr(55)`)
picminHR2024_Y <- min(humidity2024$`Hr(55)`)

# Pic maximal mensuel de pluie sur l'annee
maxHR2024_X <- which.max(humidity_month2024$`Hr(55)`)
maxHR2024_Y <- max(humidity_month2024$`Hr(55)`)
dates2024_bis <-  humidity_month2024$date

#Pic minimum mensuel sur l'annee
minHR2024_X <- which.min(humidity_month2024$`Hr(55)`)
minHR2024_Y <- min(humidity_month2024$`Hr(55)`)

# Graphique variations journalieres Hr(55)
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

# ETP #

# Pic maximal journalier  sur l'annee
picETP2024_X <- which.max(humidity2024$ETP) #position
picETP2024_Y <- max(humidity2024$ETP) #valeur

# Pic minimal journalier  sur l'annee
picminETP2024_X <- which.min(humidity2024$ETP)
picminETP2024_Y <- min(humidity2024$ETP)

# Pic maximal mensuel de pluie sur l'annee
maxETP2024_X <- which.max(humidity_month2024$ETP)
maxETP2024_Y <- max(humidity_month2024$ETP)

#Pic minimum mensuel sur l'annee
minETP2024_X <- which.min(humidity_month2024$ETP)
minETP2024_Y <- min(humidity_month2024$ETP)

# Graphique variations journalieres ETP
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


# VWC_10cm #

# Pic maximal journalier  sur l'annee
picVWC2024_X <- which.max(humidity2024$VWC_10cm) #position
picVWC2024_Y <- max(humidity2024$VWC_10cm) #valeur

# Pic minimal journalier  sur l'annee
picminVWC2024_X <- which.min(humidity2024$VWC_10cm)
picminVWC2024_Y <- min(humidity2024$VWC_10cm)

# Pic maximal mensuel de pluie sur l'annee
maxVWC2024_X <- which.max(humidity_month2024$VWC_10cm)
maxVWC2024_Y <- max(humidity_month2024$VWC_10cm)

#Pic minimum mensuel sur l'annee
minVWC2024_X <- which.min(humidity_month2024$VWC_10cm)
minVWC2024_Y <- min(humidity_month2024$VWC_10cm)

# Graphique variations journalieres VWC_10cm
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

# VPD #

# Pic maximal journalier sur l'annee
picVPD2024_X <- which.max(humidity2024$vpd55) #position
picVPD2024_Y <- max(humidity2024$vpd55) #valeur

# Pic minimal journalier  sur l'annee
picminVPD2024_X <- which.min(humidity2024$vpd55)
picminVPD2024_Y <- min(humidity2024$vpd55)

# Pic maximal mensuel de pluie sur l'annee
maxVPD2024_X <- which.max(humidity_month2024$vpd55)
maxVPD2024_Y <- max(humidity_month2024$vpd55)

#Pic minimum mensuel sur l'annee
minVPD2024_X <- which.min(humidity_month2024$vpd55)
minVPD2024_Y <- min(humidity_month2024$vpd55)

# Graphique variations journalieres vpd55
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


# Hr(55) sur les 4 annees de suivi #

# Graph variations journalieres de Hr(55) 
ggplot(data = humidity) +
  geom_line(data = humidity2020, aes(x = date, y = `Hr(55)`), colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x = date, y = `Hr(55)`), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x = date, y = `Hr(55)`), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x = date, y = `Hr(55)`), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x = date, y = `Hr(55)`), colour = "#66A61E") +
  
  # Pics maximaux
  geom_point(aes(x = dates2020[picHR2020_X], y = picHR2020_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2021[picHR2021_X], y = picHR2021_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2022[picHR2022_X], y = picHR2022_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2023[picHR2023_X], y = picHR2023_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2024[picHR2024_X], y = picHR2024_Y),
             colour = "grey40", size = 1) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020[picminHR2020_X], y = picminHR2020_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2021[picminHR2021_X], y = picminHR2021_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2022[picminHR2022_X], y = picminHR2022_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2023[picminHR2023_X], y = picminHR2023_Y),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2024[picminHR2024_X], y = picminHR2024_Y),
             colour = "grey40", size = 1) +
  
  # Annotation des maximums
  annotate("text", x = dates2020[picHR2020_X], y =picHR2020_Y+1,
           label = round(picHR2020_Y, digits = 2), colour = "grey40", size = 3) +
  annotate("text", x = dates2021[picHR2021_X], y = picHR2021_Y+1, 
           label = round(picHR2021_Y, digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2022[picHR2022_X]-5, y = picHR2022_Y+1, 
           label = round(picHR2022_Y, digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2023[picHR2023_X], y = picHR2023_Y+1, 
           label = round(picHR2023_Y, digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2024[picHR2024_X], y = picHR2024_Y+1, 
           label = round(picHR2024_Y, digits = 2),colour = "grey40", size = 3) +
  
  # Annotations des minimums
  annotate("text", x = dates2020[picminHR2020_X], y = picminHR2020_Y-1,
           label = round(picminHR2020_Y, digits = 2), colour = "grey40", size = 3) +
  annotate("text", x = dates2021[picminHR2021_X], y = picminHR2021_Y-1, 
           label = round(picminHR2021_Y, digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2022[picminHR2022_X], y = picminHR2022_Y-1, 
           label = round(picminHR2022_Y, digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2023[picminHR2023_X], y = picminHR2023_Y-1, 
           label = round(picminHR2023_Y, digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2024[picminHR2024_X], y = picminHR2024_Y-1, 
           label = round(picminHR2024_Y, digits = 2),colour = "grey40", size = 3) +
  
  # Marquage des maximums
  geom_vline(xintercept = dates2020[picHR2020_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2021[picHR2021_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[picHR2022_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[picHR2023_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[picHR2024_X],
             col = "black", linetype = "dashed") +
  
  # Mise en forme
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité de l'air journaliere au cours des 4 années de suivi phénologique",
    x = "Mois",
    y = "Hr(55) (%)"
  )

# Graph variations journalieres de Hr(55) avec moyenne mobile
humidity2020_av <- moving_average(humidity2020 %>% 
                                    select(`Hr(55)`) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2021_av <- moving_average(humidity2021 %>% 
                                    select(`Hr(55)`) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2022_av <- moving_average(humidity2022 %>% 
                                    select(`Hr(55)`) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2023_av <- moving_average(humidity2023 %>% 
                                    select(`Hr(55)`) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2024_av <- moving_average(humidity2024 %>% 
                                    select(`Hr(55)`) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)


ggplot() +
  geom_line(data = humidity2020, aes(x = date, y = humidity2020_av),colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x = date, y = humidity2021_av), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x = date, y = humidity2022_av), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x = date, y = humidity2023_av), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x = date, y = humidity2024_av), colour = "#66A61E") +
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité de l'air journaliere moyenne au cours des 4 années de suivie phénologique",
    x = "Mois",
    y = "Hr(55) (%)"
  )


# Graph variations mensuelles de Hr(55) 
bind_rows(humidity_month2020) %>% 
  bind_rows(humidity_month2021[1,]) %>% 
  print() ->
  humidity_month2020_bis

bind_rows(humidity_month2021) %>% 
  bind_rows(humidity_month2022[1,]) %>% 
  print() ->
  humidity_month2021_bis

bind_rows(humidity_month2022) %>% 
  bind_rows(humidity_month2023[1,]) %>% 
  print() ->
  humidity_month2022_bis

bind_rows(humidity_month2023) %>% 
  bind_rows(humidity_month2024[1,]) %>% 
  print() ->
  humidity_month2023_bis

ggplot() +
  geom_line(data = humidity_month2020_bis, aes(x = date, y = `Hr(55)`),colour = "#E6AB02") +
  geom_line(data = humidity_month2021_bis, aes(x = date, y = `Hr(55)`), colour = "#D95F02") +
  geom_line(data = humidity_month2022_bis, aes(x = date, y = `Hr(55)`), colour = "#E7298A") +
  geom_line(data = humidity_month2023_bis, aes(x = date, y = `Hr(55)`), colour = "#7570B3") +
  geom_line(data = humidity_month2024, aes(x = date, y = `Hr(55)`), colour = "#66A61E") +
  
  # Pics maximaux
  geom_point(aes(x = dates2020_bis[maxHR2020_X], y = humidity_month2020$`Hr(55)`[maxHR2020_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2021_bis[maxHR2021_X], y = humidity_month2021$`Hr(55)`[maxHR2021_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2022_bis[maxHR2022_X], y = humidity_month2022$`Hr(55)`[maxHR2022_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2023_bis[maxHR2023_X], y = humidity_month2023$`Hr(55)`[maxHR2023_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2024_bis[maxHR2024_X], y = humidity_month2024$`Hr(55)`[maxHR2024_X]),
             colour = "grey40", size = 1) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020_bis[minHR2020_X], y = humidity_month2020$`Hr(55)`[minHR2020_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2021_bis[minHR2021_X], y = humidity_month2021$`Hr(55)`[minHR2021_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2022_bis[minHR2022_X], y = humidity_month2022$`Hr(55)`[minHR2022_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2023_bis[minHR2023_X], y = humidity_month2023$`Hr(55)`[minHR2023_X]),
             colour = "grey40", size = 1) +
  geom_point(aes(x = dates2024_bis[minHR2024_X], y = humidity_month2024$`Hr(55)`[minHR2024_X]),
             colour = "grey40", size = 1) +
  
  # Annotations pics maximaux
  annotate("text", x = dates2020_bis[maxHR2020_X], y = humidity_month2020$`Hr(55)`[maxHR2020_X]+0.4, 
           label = round(humidity_month2020$`Hr(55)`[maxHR2020_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2021_bis[maxHR2021_X], y = humidity_month2021$`Hr(55)`[maxHR2021_X]+0.4, 
           label = round(humidity_month2021$`Hr(55)`[maxHR2021_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2022_bis[maxHR2022_X], y = humidity_month2022$`Hr(55)`[maxHR2022_X]+0.4, 
           label = round(humidity_month2022$`Hr(55)`[maxHR2022_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2023_bis[maxHR2023_X], y = humidity_month2023$`Hr(55)`[maxHR2023_X]+0.4, 
           label = round(humidity_month2023$`Hr(55)`[maxHR2023_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2024_bis[maxHR2024_X], y = humidity_month2024$`Hr(55)`[maxHR2024_X]+0.4, 
           label = round(humidity_month2024$`Hr(55)`[maxHR2024_X], digits = 2),colour = "grey40", size = 3) +
  
  # Annotations des pics minimaux
  annotate("text", x = dates2020_bis[minHR2020_X], y = humidity_month2020$`Hr(55)`[minHR2020_X]-0.4, 
           label = round(humidity_month2020$`Hr(55)`[minHR2020_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2021_bis[minHR2021_X], y = humidity_month2021$`Hr(55)`[minHR2021_X]-0.4, 
           label = round(humidity_month2021$`Hr(55)`[minHR2021_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2022_bis[minHR2022_X], y = humidity_month2022$`Hr(55)`[minHR2022_X]-0.4, 
           label = round(humidity_month2022$`Hr(55)`[minHR2022_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2023_bis[minHR2023_X], y = humidity_month2023$`Hr(55)`[minHR2023_X]-0.4, 
           label = round(humidity_month2023$`Hr(55)`[minHR2023_X], digits = 2),colour = "grey40", size = 3) +
  annotate("text", x = dates2024_bis[minHR2024_X], y = humidity_month2024$`Hr(55)`[minHR2024_X]-0.4, 
           label = round(humidity_month2024$`Hr(55)`[minHR2024_X], digits = 2),colour = "grey40", size = 3) +
  
  # Delimitation des annees
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  
  # Mise en forme
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité de l'air mensuelle au cours des 4 années de suivi phénologique",
    x = "Mois",
    y = "Hr(55) (%)"
  )


# ETP sur les 4 annees de suivi #

# Journalier
ggplot() +
  geom_line(data = humidity2020, aes(x= date, y= ETP),colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x= date, y= ETP), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x= date, y= ETP), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x= date, y= ETP), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x= date, y= ETP), colour = "#66A61E") +
  
  #Pics maximaux
  geom_point(aes(x = dates2020[picETP2020_X], y = picETP2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021[picETP2021_X], y = picETP2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022[picETP2022_X], y = picETP2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023[picETP2023_X], y = picETP2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024[picETP2024_X], y = picETP2024_Y), colour = "grey40", size = 0.8) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020[picminETP2020_X], y = picminETP2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021[picminETP2021_X], y = picminETP2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022[picminETP2022_X], y = picminETP2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023[picminETP2023_X], y = picminETP2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024[picminETP2024_X], y = picminETP2024_Y), colour = "grey40", size = 0.8) +
  
  
  #Pics maximaux
  annotate("text",x = dates2020[picETP2020_X]+30, y = picETP2020_Y, 
           label = picETP2020_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021[picETP2021_X]+30, y = picETP2021_Y, 
           label = picETP2021_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022[picETP2022_X]+30, y = picETP2022_Y, 
           label = picETP2022_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023[picETP2023_X]+30, y = picETP2023_Y, 
           label = picETP2023_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024[picETP2024_X]+30, y = picETP2024_Y, 
           label = picETP2024_Y, colour = "grey40", size = 3.5 ) +
  
  # Pics minimaux
  annotate("text",x = dates2020[picminETP2020_X], y = picminETP2020_Y-0.4, 
           label = picminETP2020_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021[picminETP2021_X], y = picminETP2021_Y-0.4, 
           label = picminETP2021_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022[picminETP2022_X], y = picminETP2022_Y-0.4, 
           label = picminETP2022_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023[picminETP2023_X], y = picminETP2023_Y-0.4, 
           label = picminETP2023_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024[picminETP2024_X], y = picminETP2024_Y-0.4, 
           label = picminETP2024_Y, colour = "grey40", size = 3.5 ) +
  
  # Marquage des maximums
  geom_vline(xintercept = dates2020[picETP2020_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2021[picETP2021_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[picETP2022_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[picETP2023_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[picETP2024_X],
             col = "black", linetype = "dashed") +
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  
  labs(
    title = "Evapotranspiration potentielle journalière au cours des 4 années de suivi phénologique",
    x = "Dates",
    y = "ETP (mm)"
  )

# Journalier moyenne mobile
humidity2020_av_ETP <- moving_average(humidity2020 %>% 
                                    select(ETP) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2021_av_ETP <- moving_average(humidity2021 %>% 
                                    select(ETP) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2022_av_ETP <- moving_average(humidity2022 %>% 
                                    select(ETP) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2023_av_ETP <- moving_average(humidity2023 %>% 
                                    select(ETP) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)

humidity2024_av_ETP <- moving_average(humidity2024 %>% 
                                    select(ETP) %>%
                                    pull(),
                                  filter = fpoids(n=2,p=2,q=2)$y)
ggplot() +
  geom_line(data = humidity2020, aes(x = date, y = humidity2020_av_ETP),colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x = date, y = humidity2021_av_ETP), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x = date, y = humidity2022_av_ETP), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x = date, y = humidity2023_av_ETP), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x = date, y = humidity2024_av_ETP), colour = "#66A61E") +
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Evapotranspiration potentielle journaliere moyenne au cours des 4 années de suivi phénologique",
    x = "Mois",
    y = "ETP (mm)"
  )

# Mensuel
ggplot() +
  geom_line(data = humidity_month2020_bis, aes(x = date, y = ETP),colour = "#E6AB02") +
  geom_line(data = humidity_month2021_bis, aes(x = date, y = ETP), colour = "#D95F02") +
  geom_line(data = humidity_month2022_bis, aes(x = date, y = ETP), colour = "#E7298A") +
  geom_line(data = humidity_month2023_bis, aes(x = date, y = ETP), colour = "#7570B3") +
  geom_line(data = humidity_month2024, aes(x = date, y = ETP), colour = "#66A61E") +
  
  # Pics maximaux
  geom_point(aes(x = dates2020_bis[maxETP2020_X], y = maxETP2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021_bis[maxETP2021_X], y = maxETP2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022_bis[maxETP2022_X], y = maxETP2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023_bis[maxETP2023_X], y = maxETP2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024_bis[maxETP2024_X], y = maxETP2024_Y), colour = "grey40", size = 0.8) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020_bis[minETP2020_X], y = minETP2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021_bis[minETP2021_X], y = minETP2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022_bis[minETP2022_X], y = minETP2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023_bis[minETP2023_X], y = minETP2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024_bis[minETP2024_X], y = minETP2024_Y), colour = "grey40", size = 0.8) +
  
  # Annotations pics maximaux
  annotate("text",x = dates2020_bis[maxETP2020_X], y = maxETP2020_Y+10, 
           label = round(maxETP2020_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021_bis[maxETP2021_X], y = maxETP2021_Y+10, 
           label = round(maxETP2021_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022_bis[maxETP2022_X], y = maxETP2022_Y+10, 
           label = round(maxETP2022_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023_bis[maxETP2023_X], y = maxETP2023_Y+10, 
           label = round(maxETP2023_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024_bis[maxETP2024_X], y = maxETP2024_Y+10, 
           label = round(maxETP2024_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  
  # Annotations pics minimaux
  annotate("text",x = dates2020_bis[minETP2020_X], y = minETP2020_Y-10, 
           label = round(minETP2020_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021_bis[minETP2021_X], y = minETP2021_Y-10, 
           label = round(minETP2021_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022_bis[minETP2022_X], y = minETP2022_Y-10, 
           label = round(minETP2022_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023_bis[minETP2023_X], y = minETP2023_Y-10, 
           label = round(minETP2023_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024_bis[minETP2024_X], y = minETP2024_Y-10, 
           label = round(minETP2024_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  
  # Delimitations des annees
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  
  # Mise en forme 
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Evapotranspiration potentielle mensuelle au cours des 4 années de suivi phénologique",
    x = "Mois",
    y = "ETP (mm)"
  )


# VWC_10cm sur les 4 annees de suivi #

#Journalier
ggplot() +
  geom_line(data = humidity2020, aes(x= date, y= VWC_10cm),colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x= date, y= VWC_10cm), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x= date, y= VWC_10cm), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x= date, y= VWC_10cm), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x= date, y= VWC_10cm), colour = "#66A61E") +
  
  #Pics maximaux
  geom_point(aes(x = dates2020[picVWC2020_X], y = picVWC2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021[picVWC2021_X], y = picVWC2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022[picVWC2022_X], y = picVWC2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023[picVWC2023_X], y = picVWC2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024[picVWC2024_X], y = picVWC2024_Y), colour = "grey40", size = 0.8) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020[picminVWC2020_X], y = picminVWC2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021[picminVWC2021_X], y = picminVWC2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022[picminVWC2022_X], y = picminVWC2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023[picminVWC2023_X], y = picminVWC2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024[picminVWC2024_X], y = picminVWC2024_Y), colour = "grey40", size = 0.8) +
  
  
  # Annotations pics maximaux
  annotate("text",x = dates2020[picVWC2020_X]+20, y = picVWC2020_Y +0.01, 
           label = round(picVWC2020_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2021[picVWC2021_X]+20, y = picVWC2021_Y+0.01, 
           label = round(picVWC2021_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2022[picVWC2022_X]+20, y = picVWC2022_Y+0.01, 
           label = round(picVWC2022_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2023[picVWC2023_X]+20, y = picVWC2023_Y+0.01, 
           label = round(picVWC2023_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2024[picVWC2024_X]+20, y = picVWC2024_Y+0.01, 
           label = round(picVWC2024_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  
  # Annotations pics minimaux
  annotate("text",x = dates2020[picminVWC2020_X], y = picminVWC2020_Y-0.01, 
           label =  round(picminVWC2020_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2021[picminVWC2021_X], y = picminVWC2021_Y-0.01, 
           label = round(picminVWC2021_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2022[picminVWC2022_X], y = picminVWC2022_Y-0.01, 
           label = round(picminVWC2022_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2023[picminVWC2023_X], y = picminVWC2023_Y-0.01, 
           label = round(picminVWC2023_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  annotate("text",x = dates2024[picminVWC2024_X], y = picminVWC2024_Y-0.01, 
           label = round(picminVWC2024_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  
  # Marquages des miximums
  geom_vline(xintercept = dates2020[picVWC2020_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2021[picVWC2021_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[picVWC2022_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[picVWC2023_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[picVWC2024_X],
             col = "black", linetype = "dashed") +

  # Mise en forme
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
    labs(
    title = "Humidité du sol journalière au cours des 4 années de suivi phénologique",
    x = "Dates",
    y = "VWC_10cm (m³/m³)"
  )

# Journalier moyenne mobile
humidity2020_av_VWC_10cm <- moving_average(humidity2020 %>% 
                                        select(VWC_10cm) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)

humidity2021_av_VWC_10cm <- moving_average(humidity2021 %>% 
                                        select(VWC_10cm) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)

humidity2022_av_VWC_10cm <- moving_average(humidity2022 %>% 
                                        select(VWC_10cm) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)

humidity2023_av_VWC_10cm <- moving_average(humidity2023 %>% 
                                        select(VWC_10cm) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)

humidity2024_av_VWC_10cm <- moving_average(humidity2024 %>% 
                                        select(VWC_10cm) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)
ggplot() +
  geom_line(data = humidity2020, aes(x = date, y = humidity2020_av_VWC_10cm),colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x = date, y = humidity2021_av_VWC_10cm), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x = date, y = humidity2022_av_VWC_10cm), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x = date, y = humidity2023_av_VWC_10cm), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x = date, y = humidity2024_av_VWC_10cm), colour = "#66A61E") +
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité du sol journaliere moyenne au cours des 4 années de suivie phénologique",
    x = "Mois",
    y = "VWC_10cm (m³/m³)"
  )

# Mensuel
ggplot() +
  geom_line(data = humidity_month2020_bis, aes(x = date, y = VWC_10cm),colour = "#E6AB02") +
  geom_line(data = humidity_month2021_bis, aes(x = date, y = VWC_10cm), colour = "#D95F02") +
  geom_line(data = humidity_month2022_bis, aes(x = date, y = VWC_10cm), colour = "#E7298A") +
  geom_line(data = humidity_month2023_bis, aes(x = date, y = VWC_10cm), colour = "#7570B3") +
  geom_line(data = humidity_month2024, aes(x = date, y = VWC_10cm), colour = "#66A61E") +
  
  # Pics maximaux
  geom_point(aes(x = dates2020_bis[maxVWC2020_X], y = maxVWC2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021_bis[maxVWC2021_X], y = maxVWC2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022_bis[maxVWC2022_X], y = maxVWC2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023_bis[maxVWC2023_X], y = maxVWC2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024_bis[maxVWC2024_X], y = maxVWC2024_Y), colour = "grey40", size = 0.8) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020_bis[minVWC2020_X], y = minVWC2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021_bis[minVWC2021_X], y = minVWC2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022_bis[minVWC2022_X], y = minVWC2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023_bis[minVWC2023_X], y = minVWC2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024_bis[minVWC2024_X], y = minVWC2024_Y), colour = "grey40", size = 0.8) +
  
  # Annotations pics maximaux
  annotate("text",x = dates2020_bis[maxVWC2020_X], y = maxVWC2020_Y+0.01, 
           label = round(maxVWC2020_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021_bis[maxVWC2021_X], y = maxVWC2021_Y+0.01, 
           label = round(maxVWC2021_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022_bis[maxVWC2022_X], y = maxVWC2022_Y+0.01, 
           label = round(maxVWC2022_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023_bis[maxVWC2023_X], y = maxVWC2023_Y+0.01, 
           label = round(maxVWC2023_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024_bis[maxVWC2024_X]+5, y = maxVWC2024_Y+0.01, 
           label = round(maxVWC2024_Y, digits = 2), colour = "grey20", size = 3.5 ) +
  
  # Annotations pics minimaux
  annotate("text",x = dates2020_bis[minVWC2020_X], y = minVWC2020_Y-0.01, 
           label = round(minVWC2020_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021_bis[minVWC2021_X], y = minVWC2021_Y-0.01, 
           label = round(minVWC2021_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022_bis[minVWC2022_X], y = minVWC2022_Y-0.01, 
           label = round(minVWC2022_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023_bis[minVWC2023_X], y = minVWC2023_Y-0.01, 
           label = round(minVWC2023_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024_bis[minVWC2024_X], y = minVWC2024_Y-0.01, 
           label = round(minVWC2024_Y, digits = 2), colour = "grey40", size = 3.5 ) +
  
  # Delimitation des annees
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  
  # Mise en forme
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Humidité du sol mensuelle au cours des 4 années de suivi phénologique",
    x = "Mois",
    y = "VWC_10cm (m³/m³)"
  )

# VPD sur les 4 annees de suivies #

# Journalier
ggplot() +
  geom_line(data = humidity2020, aes(x= date, y= vpd55),colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x= date, y= vpd55), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x= date, y= vpd55), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x= date, y= vpd55), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x= date, y= vpd55), colour = "#66A61E") +
  
  #Pics maximaux
  geom_point(aes(x = dates2020[picVPD2020_X], y = picVPD2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021[picVPD2021_X], y = picVPD2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022[picVPD2022_X], y = picVPD2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023[picVPD2023_X], y = picVPD2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024[picVPD2024_X], y = picVPD2024_Y), colour = "grey40", size = 0.8) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020[picminVPD2020_X], y = picminVPD2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021[picminVPD2021_X], y = picminVPD2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022[picminVPD2022_X], y = picminVPD2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023[picminVPD2023_X], y = picminVPD2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024[picminVPD2024_X], y = picminVPD2024_Y), colour = "grey40", size = 0.8) +

  
  # Annotations pics maximaux
  annotate("text",x = dates2020[picVPD2020_X]+30, y = picVPD2020_Y, 
           label = picVPD2020_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021[picVPD2021_X]+30, y = picVPD2021_Y, 
           label = picVPD2021_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022[picVPD2022_X]+30, y = picVPD2022_Y, 
           label = picVPD2022_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023[picVPD2023_X]+30, y = picVPD2023_Y, 
           label = picVPD2023_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024[picVPD2024_X]+30, y = picVPD2024_Y, 
           label = picVPD2024_Y, colour = "grey40", size = 3.5 ) +
  
  # Annotations pics minimaux
  annotate("text",x = dates2020[picminVPD2020_X], y = picminVPD2020_Y-0.02, 
           label = picminVPD2020_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2021[picminVPD2021_X], y = picminVPD2021_Y-0.02, 
           label = picminVPD2021_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2022[picminVPD2022_X], y = picminVPD2022_Y-0.02, 
           label = picminVPD2022_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2023[picminVPD2023_X], y = picminVPD2023_Y-0.02, 
           label = picminVPD2023_Y, colour = "grey40", size = 3.5 ) +
  annotate("text",x = dates2024[picminVPD2024_X], y = picminVPD2024_Y-0.02, 
           label = picminVPD2024_Y, colour = "grey40", size = 3.5 ) +
  
  # Marquages des miximums
  geom_vline(xintercept = dates2020[picVPD2020_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2021[picVPD2021_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[picVPD2022_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[picVPD2023_X],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[picVPD2024_X],
             col = "black", linetype = "dashed") +
  
  # Mise en forme
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Déficit de pression de vapeur journalier au cours des 4 années de suivi phénologique",
    x = "Dates",
    y = "VPD (kPa)"
  )

# Journalier moyenne mobile
humidity2020_av_vpd55 <- moving_average(humidity2020 %>% 
                                             select(vpd55) %>%
                                             pull(),
                                           filter = fpoids(n=2,p=2,q=2)$y)

humidity2021_av_vpd55 <- moving_average(humidity2021 %>% 
                                             select(vpd55) %>%
                                             pull(),
                                           filter = fpoids(n=2,p=2,q=2)$y)

humidity2022_av_vpd55 <- moving_average(humidity2022 %>% 
                                             select(vpd55) %>%
                                             pull(),
                                           filter = fpoids(n=2,p=2,q=2)$y)

humidity2023_av_vpd55 <- moving_average(humidity2023 %>% 
                                             select(vpd55) %>%
                                             pull(),
                                           filter = fpoids(n=2,p=2,q=2)$y)

humidity2024_av_vpd55 <- moving_average(humidity2024 %>% 
                                             select(vpd55) %>%
                                             pull(),
                                           filter = fpoids(n=2,p=2,q=2)$y)
ggplot() +
  geom_line(data = humidity2020, aes(x = date, y = humidity2020_av_vpd55),colour = "#E6AB02") +
  geom_line(data = humidity2021, aes(x = date, y = humidity2021_av_vpd55), colour = "#D95F02") +
  geom_line(data = humidity2022, aes(x = date, y = humidity2022_av_vpd55), colour = "#E7298A") +
  geom_line(data = humidity2023, aes(x = date, y = humidity2023_av_vpd55), colour = "#7570B3") +
  geom_line(data = humidity2024, aes(x = date, y = humidity2024_av_vpd55), colour = "#66A61E") +
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  labs(
    title = "Humidité du sol journaliere moyenne au cours des 4 années de suivi phénologique",
    x = "Mois",
    y = "VWC_10cm (m³/m³)"
  )

# Mensuel
ggplot() +
  geom_line(data = humidity_month2020_bis, aes(x = date, y = vpd55),colour = "#E6AB02") +
  geom_line(data = humidity_month2021_bis, aes(x = date, y = vpd55), colour = "#D95F02") +
  geom_line(data = humidity_month2022_bis, aes(x = date, y = vpd55), colour = "#E7298A") +
  geom_line(data = humidity_month2023_bis, aes(x = date, y = vpd55), colour = "#7570B3") +
  geom_line(data = humidity_month2024, aes(x = date, y = vpd55), colour = "#66A61E") +
  
  # Pics maximaux
  geom_point(aes(x = dates2020_bis[maxVPD2020_X], y = maxVPD2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021_bis[maxVPD2021_X], y = maxVPD2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022_bis[maxVPD2022_X], y = maxVPD2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023_bis[maxVPD2023_X], y = maxVPD2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024_bis[maxVPD2024_X], y = maxVPD2024_Y), colour = "grey40", size = 0.8) +
  
  # Pics minimaux
  geom_point(aes(x = dates2020_bis[minVPD2020_X], y = minVPD2020_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2021_bis[minVPD2021_X], y = minVPD2021_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2022_bis[minVPD2022_X], y = minVPD2022_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2023_bis[minVPD2023_X], y = minVPD2023_Y), colour = "grey40", size = 0.8) +
  geom_point(aes(x = dates2024_bis[minVPD2024_X], y = minVPD2024_Y), colour = "grey40", size = 0.8) +
  
  # Delimitations des annees
  geom_vline(xintercept = dates2021[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2022[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2023[1],
             col = "black", linetype = "dashed") +
  geom_vline(xintercept = dates2024[1],
             col = "black", linetype = "dashed") +
  
  # Annotations maximums
  annotate("text", x = dates2020_bis[maxVPD2020_X], y = maxVPD2020_Y+0.02, 
           label = round(maxVPD2020_Y, digits = 2), , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2021_bis[maxVPD2021_X], y = maxVPD2021_Y+0.02, 
           label = round(maxVPD2021_Y, digits = 2) , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2022_bis[maxVPD2022_X], y = maxVPD2022_Y+0.02, 
           label = round(maxVPD2022_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2023_bis[maxVPD2023_X], y = maxVPD2023_Y+0.02, 
           label = round(maxVPD2023_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2024_bis[maxVPD2024_X], y = maxVPD2024_Y+0.02, 
           label = round(maxVPD2024_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  
  # Annotations des minimums
  annotate("text", x = dates2020_bis[minVPD2020_X], y = minVPD2020_Y-0.02, 
           label = round(minVPD2020_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2021_bis[minVPD2021_X], y = minVPD2021_Y-0.02, 
           label = round(minVPD2021_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2022_bis[minVPD2022_X], y = minVPD2022_Y-0.02, 
           label = round(minVPD2022_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2023_bis[minVPD2023_X], y = minVPD2023_Y-0.02, 
           label = round(minVPD2023_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  annotate("text", x = dates2024_bis[minVPD2024_X], y = minVPD2024_Y-0.02, 
           label = round(minVPD2024_Y,digits = 2) , colour = "grey40", size = 3.5 ) +
  
  # Mise en forme
  x_date_4ans +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Déficit de pression de vapeur mensuelle au cours des 4 années de suivi phénologique",
    x = "Mois",
    y = "VPD (kPa)"
  )


# LIEN ENTRE LES VARIABLES CLIMATIQUES #
dataB_resume %>% 
  group_by(Year, Month, Day, date) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),`Hr(55)` = mean(`Hr(55)`), vpd55 = mean(vpd55), 
            Rain = sum(Rain),ETP = sum(ETP), VWC_10cm = mean(VWC_10cm)) %>% 
  print() ->
  climat

climat %>% 
  group_by(Year, Month) %>% 
  summarise(`Temp(55)` = mean(`Temp(55)`),`Hr(55)` = mean(`Hr(55)`), vpd55 = mean(vpd55), 
            Rain = sum(Rain),ETP = sum(ETP), VWC_10cm = mean(VWC_10cm)) %>% 
  mutate(date = floor_date(make_date(Year, Month, day = 1), unit = "month")) %>% 
  print() ->
  climat_month

Pluie <- climat$Rain
VPD <- climat$vpd55
VWC <- climat$VWC_10cm
ETP <- climat$ETP
Hr <- climat$`Hr(55)`
Temp <- climat$`Temp(55)`

# 1) Independance des variables

cov(Pluie,Temp) # Covariance négative, les variables ne sont pas independantes
cov(Pluie,VPD) # Idem
cov(Pluie,ETP) # Idem
cov(Pluie, Hr) # Covariance positive, les variables ne sont pas independantes
cov(Pluie, VWC) # Covariance nulle, les variables sont independantes
cov(ETP,Hr) # Non independance
cov(ETP, Temp) # Non independance


# 2) Correlation entre les variables

cor.test(Pluie,Temp) # Forte correlation negative et coeff significativement different de 0
cor.test(Pluie,VPD) # Idem
cor.test(Pluie,ETP) # Idem
cor.test(Pluie, Hr) # Forte correlation positive
cor.test(Pluie, VWC) # Correlation moderee non nulle
cor.test(ETP,Hr) # Forte correlation negative
cor.test(ETP, Temp) # Forte correlation positive


# 3) Representation des variables 2 à 2

ggplot(data = climat, aes(x = Temp, y = Pluie)) +
  geom_point()

ggplot(data = climat, aes(x = Hr, y = Pluie)) +
  geom_point()

ggplot(data = climat, aes(x = ETP, y = Pluie)) +
  geom_point()

ggplot(data = climat, aes(x = VPD, y = Pluie)) +
  geom_point()

ggplot(data = climat, aes(x = VWC, y = Pluie)) +
  geom_point()


# 4) Modele lineaire simple 

Pluie_Temp <- lm(Pluie ~ Temp, data = climat) # Modele
Pluie_Temp_res <- residuals(Pluie_Temp) # Residus
dwtest(Pluie_Temp) # Test d'independance des residus : autocorrelation positive
bptest(Pluie_Temp) # Non homogeneite des variances des residus
names(Pluie_Temp)

Pluie_Hr <- lm(Pluie ~ Hr, data = climat)
Pluie_Hr_res <- residuals(Pluie_Hr)
dwtest(Pluie_Hr) # Autocorrelation positive

Pluie_ETP <- lm(Pluie ~ ETP, data = climat)
Pluie_ETP_res <- residuals(Pluie_ETP)
dwtest(Pluie_ETP) # Autocorrelation positive

Pluie_VPD <- lm(Pluie ~ VPD, data = climat)
Pluie_VPD_res <- residuals(Pluie_VPD)
dwtest(Pluie_VPD) # Autocorrelation positive

Pluie_VWC <- lm(Pluie ~ VWC, data = climat)
Pluie_VWC_res <- residuals(Pluie_VWC)
dwtest(Pluie_VWC) # Autocorrelation positive

# 5) Representation graphique des modeles

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Pluie_Temp)

plot(Pluie_Hr)

plot(Pluie_ETP)

plot(Pluie_VPD)

plot(Pluie_VWC)


install.packages("ade4")
library(ade4)
install.packages("psych")
library(psych)

# Etude des variables et de leurs relations
pairs.panels(climat, 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

acp_climat <- dudi.pca(climat[,5:9], scale=T, center=T, scannf = F,nf=5)

valeurs_propres <- acp_climat$eig
mean(valeurs_propres) # mean = 1

var(acp_climat$li[,1])
var(acp_climat$li[,2])
var(acp_climat$li[,3])
var(acp_climat$li[,4])
var(acp_climat$li[,5])

pc<-round(acp_climat$eig/sum(acp_climat$eig)*100,2) # Inertie de chaque axe
cumsum(pc) # % cumule

# BarPlot des % d'inertie
# Definir le min et max de l'axe des y
ylim <- c(0, 1.2*max(pc))
xx <- barplot(pc, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim, ylab = "% d'inertie")
## Ajout des valeurs de % en dessus des barres
text(x = xx, y = pc, label = pc, pos = 3, cex = 0.8, col = "black")
## Ajout des labels sur l'axe des x (ie. numero des axes factoriels)
axis(1, at=xx, labels=c(1:length(pc)), tick=FALSE, las=1, line=-0.5, cex.axis=0.8)


s.corcircle(acp_climat$co) # Cercle des correlations

s.label(acp_climat$li[,1:2], clabel=0.5)

plot(acp_climat$li[,2],climat[,1], xlab="Axe 2", ylab=colnames(climat[1]))



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





# Journalier
ggplot(data = climat) +
  geom_line(aes(x = date, y = `Hr(55)`), colour = "#D73027") +
  geom_line(aes(x = date, y = Rain), colour = "#01665E") +
  geom_line(aes(x = date, y = ETP), colour = "black") +
  geom_line(aes(x = date, y = `Temp(55)`), colour = "#8E0152") +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Hr(%)")) 

# Mensuel
ggplot(data = climat_month) +
  geom_line(aes(x = date, y = `Hr(55)`), colour = "#D73027") +
  geom_line(aes(x = date, y = Rain), colour = "#01665E") +
  geom_line(aes(x = date, y = ETP), colour = "black") +
  geom_line(aes(x = date, y = `Temp(55)`), colour = "#8E0152") +
  scale_y_continuous(name = "Pluviométrie (mm)", sec.axis = sec_axis(~., name = "Hr(%)")) 

display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(n = 11, name = "PiYG")

library(lmtest)

plot(climat$Rain ~ climat$ETP, data = climat)
plot(climat$vpd55 ~ climat$ETP, data = climat)

shapiro.test(climat$Rain)
shapiro.test(climat$ETP)
qqnorm(climat$Rain)
qqline(climat$Rain)
qqnorm(climat$ETP)
qqline(climat$ETP)

var.test(climat$Rain,climat$ETP)

cov(climat$Rain,climat$ETP)

cor(climat$Rain,climat$ETP)
cor(climat$vpd55,climat$ETP)

cor.test(climat$Rain,climat$ETP)
cor.test(climat$vpd55,climat$ETP)

Rain_ETP <- lm(climat$Rain ~ climat$ETP, data = climat)
VPD_ETP <- lm(climat$vpd55 ~ climat$ETP, data = climat)

Rain_ETP_res <- residuals(Rain_ETP)

# Independance des residus
dwtest(Rain_ETP)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Rain_ETP)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(VPD_ETP)

summary(Rain_ETP)
summary(VPD_ETP)

