#setwd("../..")
library(glue)
library(stringr)
lib_mois <- readRDS(("Input/lib_mois.rds"))
lib_villes <- readRDS(("Input/lib_villes.rds"))

get_lib_ville <- function(code_ville,langue){
  lib_ville <- lib_villes %>% 
    filter(code == code_ville) %>% 
    select(langue) %>% 
    pull()
  if (langue == 'fr'){
    if (code_ville == '17'){
      ville <- paste0("ville d'",lib_ville)
    }
    else{
      ville <- paste("ville de",lib_ville)
    }
  }
  else{
    if (langue == 'ar'){
      ville <- paste("مدينة",lib_ville)
    }
    else{
      ville <- paste("city of",lib_ville)
    }
    
  }
  return (ville)
}


constituer_phrase <- function(text_evol,langue){
  sep_milieu <- case_when(
    langue =='ar'  ~ "،",
    TRUE ~ ","
  )
  sep_final <- case_when(
    langue =='ar'  ~ " و ",
    langue == 'fr'~ "et",
    TRUE ~ "and"
  )
  phrase_finale <- ''
  if (length(text_evol)>2){
    seuil <- length(text_evol)-2
    
    for (s in text_evol[1:seuil]){
      phrase_finale <- paste(phrase_finale,s,sep_milieu)
    }
  }
  
  if (length(text_evol)>1){
    seuil <- length(text_evol)-1
    phrase_finale <- paste(phrase_finale,text_evol[seuil],sep_final)
    phrase_finale <- paste(phrase_finale,text_evol[seuil+1])
  }
  
  if (length(text_evol)==1){
    phrase_finale <- paste(phrase_finale,text_evol[1])
  }
  return(phrase_finale)
}



get_df_pdt_alim <- function(df_evol){
  df_evol <- df_evol %>% rename(evolution = 4)
  lib_familles_produits <- read.xlsx("Data_communes/lib_familles_produits.xlsx")
  df_pdt_alim_evol <- df_evol %>% 
    filter(
      nchar(code) == 4 & substr(code,1,2) == "01" | code == "0220"
    ) %>%
    left_join(lib_familles_produits, by = c("code" = "code")) %>% 
    select(ville,fr,ar,ang,evolution)
  
  return(df_pdt_alim_evol)
}

get_df_div_non_alim <- function(df_evol){
  lib_familles_produits <- read.xlsx("Data_communes/lib_familles_produits.xlsx")
  df_evol_div_non_alim <- df_evol %>% 
    filter(
      nchar(code) == 2 &
        !substr(code, 1, 2) %in% c("01", "02")
    )

  df_evol_div_non_alim <- df_evol_div_non_alim %>%
    rename(evolution = 4) %>% 
    left_join(lib_familles_produits, by = c("code" = "code")) %>% 
    select(ville,fr,ar,ang,evolution)
  
  return(df_evol_div_non_alim)
}



#Fonction qui prends un dataframe et rassemble les 
#lignes ayant les memes evolutions en formant le texte adéquat
rassembler_meme_evolution <- function(df_evolution){
  resultats_liste <- list()
  
  # 2. Préparation des données
  df_evolution <- df_evolution %>% rename(evolution = 5)
  list_df <- df_evolution %>% group_split(evolution)

  for (i in seq_along(list_df)) {
    ldf <- list_df[[i]] %>% 
      mutate(
        fr = case_when(
          fr %in% c("Sante") ~ paste0("de la «", fr, "»"),
          fr %in% c("Enseignement") ~ paste0("de «l'", fr, "»"),
          TRUE ~ paste0("des «", fr, "»")
        ),
        ar = paste0("**", ar, "**"),
        ang = paste0("«", ang, "»")
      )
    # Traitement du groupe (fusion des lignes)
    groupe_traite <- ldf %>%
      summarise(
        ville = first(ville),
        fr = paste(fr, collapse = ", "),
        ar = paste(ar, collapse = " ، "),
        ang = paste(ang, collapse = ", "),
        evolution = first(evolution)
      ) %>%
      mutate(
        # Remplacement de la dernière virgule par le "et" / "و"
        fr = str_replace(fr, ",([^,]*)$", " et\\1"),
        ar = str_replace(ar, " ،([^،]*)$", " و\\1"), # Correction pour l'arabe
        ang = str_replace(ang, ",([^,]*)$", " and\\1") # "and" au lieu de "et" pour l'anglais
      )
    
    # 4. On ajoute le résultat du groupe dans la liste
    resultats_liste[[i]] <- groupe_traite
  }
  
  # 5. Fusion finale de tous les groupes en un seul DataFrame
  df_final_synthese <- bind_rows(resultats_liste)
  return (df_final_synthese)
  }

#fonction qui prend en param un dataframe ville,fr,ar,ang,evolution et retroune un dataframe contenant un phrase pour chaque langue
get_phrase <- function(df_resultats, langue) {
  
  # 1. Préparation des textes selon l'évolution
  df_temp <- df_resultats %>% 
    mutate(
      fr  = ifelse(evolution != 0, paste(fr, "de", evolution, "%"), fr),
      ar  = ifelse(evolution != 0, paste(ar, "ب", evolution, "% \u200F"), ar), # \u200F aide Word pour la direction
      ang = ifelse(evolution != 0, paste(ang, "by", evolution, "%"), ang)
    )
  
  # 2. Extraction de la colonne correspondant à la langue
  elements <- df_temp[[langue]]
  
  # 3. Construction de la phrase finale avec le bon séparateur
  if (langue == "ar") {
    phrase <- paste(elements, collapse = "، ") # Virgule arabe
  } else {
    phrase <- paste(elements, collapse = ", ") # Virgule latine
  }
  
  return(phrase)
}

get_p04_vars <- function(df_evol_1mois,langue){
  df_evol_pdt_alim_1mois <- get_df_pdt_alim(df_evol_1mois)
  df_evol_pdt_alim_1mois <- rassembler_meme_evolution(df_evol_pdt_alim_1mois)
  df_liste_pdt_alim_1mois_plus <- df_evol_pdt_alim_1mois %>% 
    filter(evolution > 0) %>% 
    arrange(desc(evolution))
  
  df_liste_pdt_alim_1mois_neutre <- df_evol_pdt_alim_1mois %>% 
    filter(evolution == 0)
  df_liste_pdt_alim_1mois_moins <- df_evol_pdt_alim_1mois %>% 
    filter(evolution < 0)%>% 
    arrange(evolution)
  liste_pdt_alim_1mois_plus <- get_phrase(df_liste_pdt_alim_1mois_plus,langue)
  liste_pdt_alim_1mois_neutre <- get_phrase(df_liste_pdt_alim_1mois_neutre,langue)
  liste_pdt_alim_1mois_moins <- get_phrase(df_liste_pdt_alim_1mois_moins,langue)
  return(c(liste_pdt_alim_1mois_plus,liste_pdt_alim_1mois_neutre,liste_pdt_alim_1mois_moins))
}

get_p06_vars <- function(df_evol_1mois,langue){
  df_evol_div_non_alim_1mois <- get_df_div_non_alim(df_evol_1mois)
  df_evol_div_non_alim_1mois <- rassembler_meme_evolution(df_evol_div_non_alim_1mois)
  df_liste_div_non_alim_plus <- df_evol_div_non_alim_1mois %>% 
    filter(evolution > 0) %>% 
    arrange(desc(evolution))
  
  df_liste_div_non_alim_neutre <- df_evol_div_non_alim_1mois %>% 
    filter(evolution == 0)
  df_liste_div_non_alim_moins <- df_evol_div_non_alim_1mois %>% 
    filter(evolution < 0)%>% 
    arrange(evolution)
  liste_div_non_alim_plus <- get_phrase(df_liste_div_non_alim_plus,langue)
  liste_div_non_alim_neutre <- get_phrase(df_liste_div_non_alim_neutre,langue)
  liste_div_non_alim_moins <- get_phrase(df_liste_div_non_alim_moins,langue)
  return(c(liste_div_non_alim_plus,liste_div_non_alim_neutre,liste_div_non_alim_moins))
}

get_p07_vars <- function(df_evol_12mois,langue){
  df_evol_pdt_alim_12mois <- get_df_pdt_alim(df_evol_12mois)
  df_evol_pdt_alim_12mois <- rassembler_meme_evolution(df_evol_pdt_alim_12mois)
  df_liste_pdt_alim_12mois_plus <- df_evol_pdt_alim_12mois %>% 
    filter(evolution > 0) %>% 
    arrange(desc(evolution))
  
  df_liste_pdt_alim_12mois_neutre <- df_evol_pdt_alim_12mois %>% 
    filter(evolution == 0)
  df_liste_pdt_alim_12mois_moins <- df_evol_pdt_alim_12mois %>% 
    filter(evolution < 0)%>% 
    arrange(evolution)
  liste_pdt_alim_12mois_plus <- get_phrase(df_liste_pdt_alim_12mois_plus,langue)
  liste_pdt_alim_12mois_neutre <- get_phrase(df_liste_pdt_alim_12mois_neutre,langue)
  liste_pdt_alim_12mois_moins <- get_phrase(df_liste_pdt_alim_12mois_moins,langue)
  return(c(liste_pdt_alim_12mois_plus,liste_pdt_alim_12mois_neutre,liste_pdt_alim_12mois_moins))
}

get_p10_vars <- function(df_evol_12mois,langue){
  df_evol_div_non_alim_12mois <- get_df_div_non_alim(df_evol_12mois)
  df_evol_div_non_alim_12mois <- rassembler_meme_evolution(df_evol_div_non_alim_12mois)
  
  df_liste_div_non_alim_12mois_plus <- df_evol_div_non_alim_12mois %>% 
    filter(evolution > 0) %>% 
    arrange(desc(evolution)) %>% 
    slice(1)
  

  df_liste_div_non_alim_12mois_moins <- df_evol_div_non_alim_12mois %>% 
    filter(evolution < 0)%>% 
    arrange(evolution) %>% 
    slice(1)
  liste_max_div_non_alim_12mois_plus <- get_phrase(df_liste_div_non_alim_12mois_plus,langue)
  liste_min_div_non_alim_12mois_moins <- get_phrase(df_liste_div_non_alim_12mois_moins,langue)
  
  return(c(liste_min_div_non_alim_12mois_moins,liste_max_div_non_alim_12mois_plus))
}

get_variable_texte <- function(df_redaction,variable_code,langue){
  variable_texte <- df_redaction %>% 
    filter(code==variable_code) %>% 
    select(3) %>% 
    pull()
  return (variable_texte)
}
get_evol_annuel_gra <- function(code_ville,ipc_histo,var_analyse){
  mois_graphe <- substr(p_mois_courant,8,9)
  liste_var_gra_ann <- 
    colnames(ipc_histo)[grepl(paste0(mois_graphe,"$"), colnames(ipc_histo))]  
  liste_var_gra_ann <- sort(unique(liste_var_gra_ann[liste_var_gra_ann <= var_analyse]))
  ipc_evol_annuel_gra <- ipc_histo %>% 
    filter(ville ==  code_ville) %>% 
    select("code","libelle_diff",all_of(liste_var_gra_ann))%>%
    filter(code == "000GEN")%>% 
    pivot_longer(
      cols = starts_with("V"),  # Colonnes à transformer
      names_to = "x_date",           # Nom de la nouvelle colonne contenant les noms de variables
      values_to = "ipc"         # Nom de la colonne avec les valeurs
    ) %>% 
    arrange (x_date) %>%
    mutate(x_date = paste0(substr(x_date,2,5),"/",substr(x_date,7,8)) )%>% 
    mutate(evol = round_half_up( ((ipc / lag(ipc)) -1 ) *100 , 1) ) %>%
    filter (!is.na(evol) )
  return(ipc_evol_annuel_gra)
}

get_evol_mensuel_gra <- function(code_ville,ipc_histo,var_analyse){
  mois_graphe <- substr(p_mois_courant,8,9)
  liste_var_gra_ann <- 
    colnames(ipc_histo)[grepl(paste0(mois_graphe,"$"), colnames(ipc_histo))]  
  liste_var_gra_ann <- sort(unique(liste_var_gra_ann[liste_var_gra_ann <= var_analyse]))
  ipc_evol_annuel_gra <- ipc_histo %>% 
    filter(ville ==  code_ville) %>% 
    select("code","libelle_diff",all_of(liste_var_gra_ann))%>%
    filter(code == "000GEN")%>% 
    pivot_longer(
      cols = starts_with("V"),  # Colonnes à transformer
      names_to = "x_date",           # Nom de la nouvelle colonne contenant les noms de variables
      values_to = "ipc"         # Nom de la colonne avec les valeurs
    ) %>% 
    arrange (x_date) %>%
    mutate(x_date = paste0(substr(x_date,2,5),"/",substr(x_date,7,8)) )%>% 
    mutate(evol = round_half_up( ((ipc / lag(ipc)) -1 ) *100 , 1) ) %>%
    filter (!is.na(evol) )
  return(ipc_evol_annuel_gra)
}