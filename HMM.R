### Chaine de Markov caché ###

# Package necessaire
install.packages("HMM")
library(HMM)

# Observations pour S.globulifera

pheno2[!is.na(PPraw)] %>% 
  filter(Genus_Spec=="Symphonia_globulifera", Usable == 1) %>% 
  as.character(pheno2$PPFlo)->
  pheno_globu

pheno2$PPFlo[is.na(pheno2$PPFlo)]<-"not_Fl"

observations_globu = as.character(pheno_globu$PPFlo)
print(observations_globu)

# Initialisation HMM pour S.globulifera

HMM = initHMM(c("Floraison","Végétatif"),observations_globu)
print(HMM)

viterbi(HMM,observations_globu)