
#### --------------------------------------------------------------------
#### Production du rapport mensuelle d'aide à la rédaction de la note IPC Tanger
#### --------------------------------------------------------------------

rm(list=ls())

###--- Paramètres à renseigner analyse <- "aaaa_mm"          ------###
analyse <- "2026_02"
###----------------------------------------------------------------###

library(dplyr)
library(lubridate)
library(glue)
library(zip) 
library(openxlsx)

### Création d'une liste des villes de la région
ville_reg <- readRDS("Data_locales/lib_ville.rds") %>% 
  select(ville) %>%  
  arrange(ville)
liste_ville <- ville_reg %>% 
  pull()

### lecture du dernier fichier historique
ipc_dernier <- readRDS("Data_locales/IPC_histo_dernier.rds")

# recherche du dernier mois du fichier histo
lis_var <- colnames(ipc_dernier) 
lis_var2 <- lis_var[grepl("^V20", lis_var) & !grepl("^V.*99$", lis_var)]
lis_var2 <- sub("^V", "", lis_var2) 
last_date <- sort(lis_var2, decreasing = TRUE) [1]

### Création de la fonction produisant le rapport mensuel d'une ville ###
#### ---------------------------------------------------------------- ### 
f_rapport_mensuel <- function(ville) {
  print (glue("Traitement de la ville ",ville))
  
  ## Production du html pour une ville
  quarto::quarto_render(
    input = "Programmes/Sous_programmes/99_Modele.qmd",
    execute_params = list(p_ville = ville,
                          p_periode = analyse) )
  
  ## Renommage du html de sortie 
  file.rename("Programmes/Sous_programmes/99_Modele.html",
              glue("Rapports_mensuels/Mois{analyse}/Analyse_{analyse}_Ville{ville}.html"))
  
  ## Creation des illustrations pour note
  source("Programmes/Sous_programmes/Pgm_tableau_note.R",
         local = environment())
  source("Programmes/Sous_programmes/Pgm_graphique_note.R",
         local = environment())
  
  ## Creation des fichiers word
  print("Ajouts des documents word")
  source("Programmes/ipc_note_quarto_render.R",
         local = environment())
  
} ## Fin de la creation de la fonction f_rapport_mensuel

#### Controle de la date d'analyse ###
#### ----------------------------- ### 
# transformation en date
date_max_histo = ym(last_date) 
date_analyse = ym(analyse)

if (date_analyse > date_max_histo | year(date_analyse) < "2020") {
  # Interruption du traitement #
  print(glue("Le mois choisi n'est pas dans la table historique."))
  print("Vérifier votre mois !!!!")
}   else {
  
 # creation du reperoire pour les sorties s'il n'existe pas
  if (!dir.exists(glue("Rapports_mensuels/Mois{analyse}")) ) {
    dir.create(glue("Rapports_mensuels/Mois{analyse}"))
  }
  
 # creation table bds à vide  
  bds <- data.frame()
  saveRDS(bds,file = "Programmes/Sous_programmes/bds.rds")
  
  # Appel de la fonction pour tous les éléments de la liste avec lapply
  lapply(liste_ville, function(i) f_rapport_mensuel(ville=i))
}

##########################################
### Creation du fichier excel pour BDS ###
##########################################
bds <- readRDS("Programmes/Sous_programmes/bds.rds")

nom_export <- paste0("bds_ipc_12_", analyse, ".xlsx")
write.xlsx(
  bds,
  file = file.path(glue("Rapports_mensuels/Mois{analyse}/"),nom_export))




#### Nettoyage

if (file.exists("Programmes/Sous_programmes/bds.rds") ) {
  file.remove("Programmes/Sous_programmes/bds.rds")
}
if (file.exists("Programmes/Sous_programmes/illustrations.RData") ) {
  file.remove("Programmes/Sous_programmes/illustrations.RData")
}

###################################################
### Creation (ou enrichissement) du fichier zip ###
###################################################

if (file.exists(glue("Rapports_mensuels/Mois{analyse}/M{analyse}.zip")) ) {
  file.remove(glue("Rapports_mensuels/Mois{analyse}/M{analyse}.zip")) 
}


# Liste de tous les fichiers (avec sous-dossiers si besoin)
fichiers <- list.files(path=glue("Rapports_mensuels/Mois{analyse}") , 
                       full.names = TRUE, 
                       recursive = TRUE)

# Création de l'archive
zip::zip( zipfile = glue("Rapports_mensuels/Mois{analyse}/M{analyse}.zip"),
  files = fichiers
)

print("Fin des traitements")

