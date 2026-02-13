#### Mise à jour mensuelle de la base historique pour la DR de Tanger

# 02_MAJ_mensuelle_fichier_historiqueIPC.R

rm(list=ls())

library(glue)
library(dplyr)
library(lubridate)

# parametres : exemple periode_maj <- "2025_01"
periode_maj <- "2025_11"

# annee
annee <- substr(periode_maj,1,4)

# lecture du dernier fichier historique
ipc_dernier <- readRDS("Data_locales/IPC_histo_dernier.rds")
#ipc_dernier <- readRDS("Data_locales/IPC_histo_initial_2017_01_2024_12.rds")

# recherche du dernier mois du fichier histo
lis_var <- colnames(ipc_dernier) 
lis_var2 <- lis_var[grepl("^V20", lis_var) & !grepl("^V.*99$", lis_var)]
lis_var2 <- sub("^V", "", lis_var2) 
last_date <- sort(lis_var2, decreasing = TRUE) [1]

# transformation en date
date_attendue = ym(last_date) + months(1)
date_periode_maj = ym(periode_maj) 

# Controle de la date des données par rapport à la période attendue
# ----------------------------------------------------------------

if (date_attendue != date_periode_maj) {
  print(glue("La période choisie {format(date_periode_maj, '%B %Y')} ne correspond pas à la période attendue {format(date_attendue, '%B %Y')}."))
   } else { 
  print(glue("Intégration de {format(date_periode_maj, '%B %Y')}"))
     
 ### Traitement d'integration d'un mois
     # Liste des villes de la DR
     liste_ville <- readRDS("Data_locales/lib_ville.rds") %>% 
       select(ville) 
     liste_ville <- as.vector(liste_ville$ville)
     
     ipc_mois <- data.frame()
     controle <- 1
     for(i in liste_ville) {
       
       print (glue("Ville {i}"))
       
       if (!file.exists(glue("Data_Output/Data_HCP_enrichie_{periode_maj}_v{i}.rds")) ){
         print(glue("Le fichier donnee_centrale_enrichie_{periode_maj}_v{i}.rds n'existe pas."))
         controle <- controle * 0
       } else {
         print("Concatenation de l'IPC mensuel")
         
         ipc <- readRDS(glue("Data_Output/Data_HCP_enrichie_{periode_maj}_v{i}.rds") )
         ipc_mois <- bind_rows(ipc_mois, ipc) %>% 
           select(ville,code,ipc)
       }  # Fin traitement si fichier Data_HCP_enrichie existe
       
     } # Fin traitement une ville
                  
         ipc_maj <- ipc_dernier %>% 
           full_join(ipc_mois,by = c("ville","code")) 

         names(ipc_maj)[names(ipc_maj) == "ipc"] <- paste0("V",periode_maj)

         # si mois = 12, calcul en plus de l'ipc annuel identifée par variable aaaa_99
         if (substr(periode_maj,6,7) == "12"){
         print("Calcul de l'IPC annuel")
         nom_var <- paste0("V",annee, "_99")
         var_moys <- names(ipc_maj)[startsWith(names(ipc_maj), paste0("V", annee))]
           ipc_maj <- ipc_maj %>%
               mutate(!!nom_var := round_half_up(rowMeans(across(all_of(var_moys)), na.rm = TRUE),1) )

         }  # Fin traitement mois 12

 if (controle == 0) {
   print("INTERRUPTION DU TRAITEMENT")
 } else {
   lis_var <- colnames(ipc_maj) 
   lis_var <- lis_var[grepl("^V20", lis_var)] 
   lis_var <- sort(lis_var, decreasing = FALSE)
   
   ipc_maj <- ipc_maj %>%
     select(all_of(c("ville", "code", "libelle_diff",
                     lis_var)))
   saveRDS(ipc_maj,glue("Data_locales/IPC_histo_2017_01_{periode_maj}.rds"))
   saveRDS(ipc_maj,"Data_locales/IPC_histo_dernier.rds")
   print("TRAITEMENT TERMINE")
 }

   } # Fin intégration mois

##### FIN DE PROGRAMME  #######


