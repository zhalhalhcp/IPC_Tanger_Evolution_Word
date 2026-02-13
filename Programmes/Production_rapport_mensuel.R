
#### --------------------------------------------------------------------
#### Production du rapport mensuelle d'aide à la rédaction de la note IPC
#### --------------------------------------------------------------------

rm(list=ls())

###--- Paramètres à renseigner exemple : analyse <- "2025_01"------###
analyse <- "2025_11"
###----------------------------------------------------------------###

library(dplyr)
library(lubridate)
library(glue)
library(zip)

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
              glue("Rapports_mensuels/Analyse_{analyse}_Ville{ville}.html"))
  
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
  
  # ##################
  # liste_ville <-"99"
  # ##################
  
  # Appel de la fonction pour tous les éléments de la liste avec lapply
  lapply(liste_ville, function(i) f_rapport_mensuel(ville=i))
}

###################################################
### Creation (ou enrichissement) du fichier zip ###
###################################################

liste_fichiers_analyse <- list.files(path="Rapports_mensuels" ,
                                     full.names = FALSE,recursive = TRUE)
globalfile <- NULL
for(k in liste_fichiers_analyse) {
  globalfile <- c(globalfile,glue("Rapports_mensuels/{k}"))
}

zip(zipfile=glue("Rapports_mensuels.7z"), files=globalfile)

print("Fin des traitements")

