###### Programme initialisant le fichier historique de l'IPC pour region 

######   P02_Initialisation_fichier_historiqueIPC.R

rm(list=ls())  # Vidage memoire


library(dplyr)
library(glue)

# Base nationale IPC (alim et non alim inclus) de 2017 à 2024
base_histo_initial <- readRDS("Data_communes/Base_historique_2017-2024.rds" )
# familles_produits.rds
familles_produits <- readRDS("Data_communes/familles_produits.rds" )

base_regional <- base_histo_initial %>% 
  select(-libelle) %>% 
  filter(ville %in% c("08","10","17","99")) %>% 
  left_join(familles_produits,by="code") %>% 
  select(ville,code,libelle_diff,everything()) 


# dernier mois traité
lis_var <- colnames(base_regional) 
lis_var2 <- lis_var[grepl("^V20", lis_var) & !grepl("^V.*99$", lis_var)]
last_date <- sort(lis_var2, decreasing = TRUE) [1]
last_date <- sub("^V", "", last_date)

saveRDS(base_regional,glue("Data_locales/IPC_histo_initial_2017_01_{last_date}.rds"))
saveRDS(base_regional,"Data_locales/IPC_histo_dernier.rds")


##### Fin de programme #######




 
