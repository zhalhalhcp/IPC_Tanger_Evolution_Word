
#### --------------------------------------------------------------------
#### Production de la note IPC
#### en français pour Tanger
#### --------------------------------------------------------------------

rm(list=ls())

###--- Paramètres à renseigner analyse <- "aaaa_mm"          ------###
analyse <- "2025_09"
###----------------------------------------------------------------###

library(dplyr)
library(lubridate)
library(glue)
library(zip)
library(openxlsx)
library(purrr)

# ### Création d'une liste des villes de la région
# ville_reg <- readRDS("Data_locales/lib_ville.rds") %>% 
#   select(ville) %>%  
#   arrange(ville)
# liste_ville <- ville_reg %>% 
#   pull()
# 
# # liste_langue <- c('fr','ar','an')
# liste_langue <- c('fr')

liste_ville <- "10"
liste_langue <- "fr"

### lecture du dernier fichier historique
ipc_mensuel <- readRDS("Pascal/IPC_mensuel.rds")

f_rapport_mensuel <- function(ville,langue) {
  print (glue("Fonction f_rapport_mensuel {ville} {langue}"))
  input_file <- glue("Pascal/ville_Modele_{langue}.qmd")

## Production du html pour une ville
quarto::quarto_render(
  input = input_file,
  execute_params = list(p_ville = ville,
                        p_periode = analyse,
                        p_langue = langue) )

  file.rename(glue("Pascal/ville_Modele_{langue}.docx"),
              glue("Pascal/Note_{analyse}_Ville{ville}_{langue}.docx"))
}

map(liste_langue, function(lan) {
  map(liste_ville, function(vil) {
    f_rapport_mensuel(ville = vil, langue = lan)
  })
})

print("OK")

# # recherche du dernier mois du fichier histo
# lis_var <- colnames(ipc_dernier) 
# lis_var2 <- lis_var[grepl("^V20", lis_var) & !grepl("^V.*99$", lis_var)]
# lis_var2 <- sub("^V", "", lis_var2) 
# last_date <- sort(lis_var2, decreasing = TRUE) [1]

### Création de la fonction produisant le rapport mensuel d'une ville ###
#### ---------------------------------------------------------------- ### 
# f_rapport_mensuel <- function(ville,langue) {
  # print (glue("Traitement de la ville ",ville))
  # 
  # if (ville == "99") {
  #   input_file <- glue("Programmes_validation_Pascal/WORD/Sous_programmes/99_Modele_{langue}.qmd")
  # } else {
  #   input_file <- glue("Programmes_validation_Pascal/WORD/Sous_programmes/ville_Modele_{langue}.qmd")
  #     }
  
  # ## Production du html pour une ville
  # quarto::quarto_render(
  #   input = input_file,
  #   execute_params = list(p_ville = ville,
  #                         p_periode = analyse,
  #                         p_langue = langue) )
  
#   ## Renommage du html de sortie
#   if (ville == "99") {
#   file.rename(glue("Programmes_validation_Pascal/WORD/Sous_programmes/99_Modele_{langue}.docx"),
#               glue("Programmes_validation_Pascal/WORD/Mois{analyse}/Note_{analyse}_National_{langue}.docx"))
#   } else {
#     file.rename(glue("Programmes_validation_Pascal/WORD/Sous_programmes/ville_Modele_{langue}.docx"),
#               glue("Programmes_validation_Pascal/WORD/Mois{analyse}/Note_{analyse}_Ville{ville}_{langue}.docx"))
#   }
# 
# } ## Fin de la creation de la fonction f_rapport_mensuel

#### Controle de la date d'analyse ###
#### ----------------------------- ### 
# transformation en date
# date_max_histo = ym(last_date) 
# date_analyse = ym(analyse)
# 
# if (date_analyse > date_max_histo | year(date_analyse) < "2020") {
#   # Interruption du traitement #
#   print(glue("Le mois choisi n'est pas dans la table historique."))
#   print("Vérifier votre mois !!!!")
# }   else {
#   
#  # creation du repertoire pour les sorties s'il n'existe pas
#   if (!dir.exists(glue("Programmes_validation_Pascal/WORD/Mois{analyse}")) ) {
#     dir.create(glue("Programmes_validation_Pascal/WORD/Mois{analyse}"))
#   }
#     
#   map(liste_langue, function(lan) {
#     map(liste_ville, function(vil) {
#       f_rapport_mensuel(ville = vil, langue = lan)
#     })
#   })
#  
# }
# 
# print("Fin des traitements")

