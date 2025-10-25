
#### P01 Création des villes et ponderations de la DR de Tanger ####

rm(list=ls())

library(dplyr)
library(glue)

projetIPC <- "Tanger"

df_ville <- readRDS("Data_communes/libville.rds")

villes_dr <- df_ville %>% 
  filter(projet_IPC %in% c(projetIPC,"National") ) %>% 
  select(ville,libville,libvillesansaccent)

saveRDS(villes_dr,glue("Data_locales/lib_ville.rds") )

####

file.copy(from = "Data_communes/ponderation_08.rds",
          to = "Data_locales/ponderation_08.rds",
          overwrite = TRUE)

file.copy(from = "Data_communes/ponderation_10.rds",
          to = "Data_locales/ponderation_10.rds",
          overwrite = TRUE)

file.copy(from = "Data_communes/ponderation_17.rds",
          to = "Data_locales/ponderation_17.rds",
          overwrite = TRUE)

file.copy(from = "Data_communes/ponderation_99.rds",
          to = "Data_locales/ponderation_99.rds",
          overwrite = TRUE)

############### Fin de pgm ########
