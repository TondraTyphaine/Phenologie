# Titre : Analyses des donnees meteorologiques
# Auteur : Tondra Typhaine
# Date de creation : 03/05/2024
# Derniere modification : 03/05/2024


# Package necessaire
install.packages("tidyverse")
library(tidyverse)

# Data
data<- read_csv2("data/TourAFlux-2004-2023.csv")

# Affinage des donnees
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
  data2






