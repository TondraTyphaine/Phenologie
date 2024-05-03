
### Code hors script PhenObs ###       

install.packages("tidyverse")
library(tidyverse)

## Data
## Lecture du jeu de données
read_csv2("data/Synthese_Pheno.csv") ->
  pheno

## Creation d'une nouvelle colonne "espece"
pheno %>%
  mutate(espece = paste(Genus, Species)) %>% 
  select(Num_crown,Usable,espece, Family:Species, `23/10/2020` : `23/01/2024`) %>% 
  print() ->
  pheno_sp

# Pivotement en tableau long
pheno_sp %>% 
  pivot_longer(
    cols =c(`23/10/2020` : `23/01/2024`),
    names_to = "date",
    values_to = "phenophases"
  ) %>%
  print()-> 
  pheno_fl

# Filtrer pour ne garder que S.globulifera
pheno_fl %>% 
  filter(espece == "Symphonia globulifera") %>% 
  print() ->
  globu


## Histogramme des phenophases par date d'observation

# Dates des observations
date <- globu$date

# Les phenophases observees
phenophases <- globu$phenophases


globu %>% 
  ggplot(aes(x = date, fill = phenophases)) +
  geom_bar() +
  
  # Pour affichicher toutes les valeurs de l'axe Y de 0 à 20 par pas de 1 (représente le nombre d'individus observés)
  scale_y_continuous(breaks = seq(0, 20, by = 1)) + 
  
  #permet d'afficher les labels de l'axe x en diagonal
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = " Observations des phenophases", y =" Individus")
