###### Programme de calcul de l'IPC alimentaire et non alimentaire pour un mois
###### Traitement pour chaque ville et le national

######   01_IPC_mois_alim_nonalim.R 
rm(list=ls())  # Vidage memoire

# parametres : exemple identifiant_periode <- "2025_01"
identifiant_periode <- "2025_09"

library(dplyr)
library(glue)
library(readxl)
library(janitor)

annee <- substr(identifiant_periode,1,4)
mois <- substr(identifiant_periode,6,7)

liste_ville <- readRDS(glue("Data_locales/lib_ville.rds")) %>% 
  select(ville) 
liste_ville <- as.vector(liste_ville$ville)

# Lecture du fichier familles_produits (ensemble des produits attendus)
familles_produits <- readRDS("Data_communes/familles_produits.rds")
# Lecture du fichier des 12 familles_produits pour calcul alim et non-alim
familles_produits_12 <- readRDS("Data_communes/familles_produits_12.rds")


#######################################################################
# Lecture du fichier mensuel des données excel envoyées par la centrale
# avec mise en forme et remplacement na par GENERAL pour coicop
#######################################################################

###-------------------------------###
### Lecture des données générales ###
###-------------------------------###

donnee_centrale <- readxl::read_excel(glue("Data_HCP/donnees_centrale_{annee}_{mois}.xlsx"), 
                                      col_types = c("text", "text", "text", 
                                                    "numeric", "numeric", "numeric"))

colnames(donnee_centrale) <- tolower(colnames(donnee_centrale))
colnames(donnee_centrale)[5]<-'ipc'
donnee_centrale <- donnee_centrale %>% 
  mutate(coicop = if_else( is.na(coicop) | coicop == "GR","000GEN",coicop) ) %>% 
  select(ville, coicop,ipc)

###------------------------------------------###
### TRAITEMENT POUR CHAQUE VILLE ET NATIONAL ###
###------------------------------------------###

for(i in liste_ville) {
# i<- "10"
  print(paste0("Traitement de ",i))
  
  ### Calcul IPC alim et non alim pour chaque ville ###
  ###-----------------------------------------------###
  
  donnee_centrale_ville <- donnee_centrale %>%
    filter(ville == i) %>% 
    select(-ville)

  # ajout de la ponderation aux 12 produits alim non-alim
    ponderation <- readRDS(glue("Data_locales/ponderation_",i,".rds"))   
    ponderation_12 <- familles_produits_12 %>% 
    left_join(ponderation,by="coicop")
  
  # Calcul ipc produits alim non-alim
    indices_ponderation_12 <- ponderation_12  %>% 
      left_join(donnee_centrale_ville, by="coicop") %>% 
      mutate(ipc_pond = ipc * ponderation)
  
ipc_alim_nonalim <- indices_ponderation_12 %>% 
  filter(! substr(coicop,1,3) %in% c("000","001") ) %>% 
  mutate(coicop = case_when(coicop %in% c("01","02") ~ "001ALIM" ,
                            coicop %in% c("03","04","05","06","07","08","09","10","11","12") ~ "001NONALIM" ,
                               TRUE ~ "HORS") ) %>% 
  group_by(coicop) %>% 
  summarise(ipc_pond = sum(ipc_pond, na.rm = T),
            ponderation = sum(ponderation,na.rm = T) )  %>% 
  mutate(ipc = round_half_up(ipc_pond / ponderation,1) )

  # Concatenation pour faire IPC pour tous les niveaux
  ipc_12 <- indices_ponderation_12 %>% 
    filter(!substr(coicop,1,3) == "001") %>% 
    bind_rows(ipc_alim_nonalim) %>%
    mutate(ville=i) %>% 
    arrange(coicop) 

  # Sauvegarde du résultat (ipc_12_2024_01_v10.rds)
  saveRDS(ipc_12,glue("Data_output/ipc_12_{identifiant_periode}_v{i}.rds"))

  ### Enrichissement des données centrales avec alim et non alim
  ipc_alim_nonalim_reduit <-  ipc_alim_nonalim %>% 
    select(coicop, ipc)
  
  donnee_centrale_enrichie <- familles_produits %>% 
    left_join(donnee_centrale_ville, by=c("code"="coicop")) %>% 
    left_join(ipc_alim_nonalim_reduit, by=c("code"="coicop")) %>% 
    mutate(ville=i,
           ipc = coalesce(ipc.x,ipc.y) ,
           id_periode = identifiant_periode) %>% 
    filter(!is.na(ipc)) %>% 
    select(id_periode, ville,code,libelle,ipc) 
  
  saveRDS(donnee_centrale_enrichie,
          glue("Data_output/Data_HCP_enrichie_{identifiant_periode}_v{i}.rds"))

}

##################################### 




