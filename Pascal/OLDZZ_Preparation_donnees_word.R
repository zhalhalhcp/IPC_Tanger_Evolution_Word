
### function get_lib_ville
get_lib_ville <- function(code_ville,langue){
  lib_ville <- readRDS("../../../Data_locales/lib_ville.rds") %>%
    filter(ville == code_ville) %>%
    select(libville) %>%
    pull()
  
  if (langue == 'fr'){
    if (code_ville == '17'){ville <- paste0("ville d'",lib_ville) }
    else{ ville <- paste("ville de",lib_ville)  }
  } else if (langue == 'ar'){ville <- paste("مدينة",lib_ville)
  } else {ville <- paste("city of",lib_ville) }
return (ville)
}

### function get_variable_texte
get_variable_texte <- function(df_redaction,variable_code){
  variable_texte <- df_redaction %>%
    filter(code==variable_code) %>%
    select(3) %>%
    pull()
  return (variable_texte)
}

### function concat_fr
concat_fr <- function(x) {
  n <- length(x)
  if (n == 0) return("")
  if (n == 1) return(x)
  if (n == 2) return(paste(x, collapse = " et "))
  paste0(paste(x[-n], collapse = ", "), " et ", x[n])
}

retrouver_donnees <- function(periode_analyse,code_ville,langue) {
  library(openxlsx)
  ville <- get_lib_ville(code_ville,langue)
  
  date_analyse <- ym(periode_analyse) 
  date_analyse_m1 <- date_analyse -months(1)  # date_analyse -1mois
  date_analyse_m2 <- date_analyse -months(2)  # date_analyse -2mois
  date_analyse_m3 <- date_analyse -months(3)  # date_analyse -3mois
  date_analyse_m12 <- date_analyse -years(1)  # date_analyse -12mois
  
  var_analyse <- format(date_analyse, "V%Y_%m")  # variable analyse
  var_analyse_m1 <- format(date_analyse_m1, "V%Y_%m") # variable analyse -1mois
  var_analyse_m2 <- format(date_analyse_m2, "V%Y_%m") # variable analyse -2mois
  var_analyse_m3 <- format(date_analyse_m3, "V%Y_%m") # variable analyse -3mois
  var_analyse_m12 <- format(date_analyse_m12, "V%Y_%m") # variable analyse -12mois
  
  lib_mois_annee_m <- tools::toTitleCase(format(date_analyse, "%B %Y"))
  lib_mois_annee_m1 <- tools::toTitleCase(format(date_analyse_m1, "%B %Y"))
  lib_mois_annee_m12 <- tools::toTitleCase(format(date_analyse_m12, "%B %Y"))
  
  ipc_histo <- readRDS("../../../Data_locales/IPC_histo_dernier.rds")
  liste_var <- c("ville","code","libelle_diff",
                 var_analyse_m12,var_analyse_m3,var_analyse_m2,var_analyse_m1,var_analyse)
  ipc_histo_note <- ipc_histo %>% 
    filter(ville ==  code_ville) %>% 
    select(all_of(liste_var)) %>% 
    mutate(
      evol_1mois = round(((.data[[var_analyse]] / .data[[var_analyse_m1]]) -1) *100,1),
      evol_2mois = round(((.data[[var_analyse]] / .data[[var_analyse_m2]]) -1) *100,1),
      evol_3mois = round(((.data[[var_analyse]] / .data[[var_analyse_m3]]) -1) *100,1),
      evol_12mois = round(((.data[[var_analyse]] / .data[[var_analyse_m12]]) -1) *100,1)
    ) 
  
  lib3_texte <- readRDS("../lib3_texte_produits.rds")
  
  pdt_alim_1mois_hausse <- ipc_histo_note %>% 
    filter(nchar(code) == 4 & substr(code,1,2) == "01" | code == "0220" ) %>% 
    left_join(lib3_texte, by = c("code" = "code")) %>% 
    select(ville,libelle_texte_fr,evol_1mois) %>% 
    filter(evol_1mois >0 ) %>% 
    arrange(desc(evol_1mois)) %>% 
    mutate(libelle_final = glue("{libelle_texte_fr} de {evol_1mois}%") ) %>% 
    pull(libelle_final)
  
  pdt_alim_1mois_baisse <- ipc_histo_note %>% 
    filter(nchar(code) == 4 & substr(code,1,2) == "01" | code == "0220" ) %>% 
    left_join(lib3_texte, by = c("code" = "code")) %>% 
    select(ville,libelle_texte_fr,evol_1mois) %>% 
    filter(evol_1mois <0 ) %>% 
    arrange(evol_1mois) %>% 
    mutate(libelle_final = glue("{libelle_texte_fr} de {evol_1mois}%") ) %>% 
    pull(libelle_final)
  
  pdt_nonalim_1mois_hausse <- ipc_histo_note %>% 
    filter(nchar(code) == 2 & !substr(code, 1, 2) %in% c("01", "02")) %>% 
    left_join(lib3_texte, by = c("code" = "code")) %>% 
    select(ville,libelle_texte_fr,evol_1mois) %>% 
    filter(evol_1mois >0 ) %>% 
    arrange(desc(evol_1mois)) %>% 
    mutate(libelle_final = glue("{libelle_texte_fr} de {evol_1mois}%") ) %>% 
    pull(libelle_final)  
 
  pdt_nonalim_1mois_baisse <- ipc_histo_note %>% 
    filter(nchar(code) == 2 & !substr(code, 1, 2) %in% c("01", "02")) %>% 
    left_join(lib3_texte, by = c("code" = "code")) %>% 
    select(ville,libelle_texte_fr,evol_1mois) %>% 
    filter(evol_1mois <0 ) %>% 
    arrange(evol_1mois) %>% 
    mutate(libelle_final = glue("{libelle_texte_fr} de {evol_1mois}%") ) %>% 
    pull(libelle_final) 
  
  pdt_alim_12mois_hausse <- ipc_histo_note %>% 
    filter(nchar(code) == 4 & substr(code,1,2) == "01" | code == "0220" ) %>% 
    left_join(lib3_texte, by = c("code" = "code")) %>% 
    select(ville,libelle_texte_fr,evol_12mois) %>% 
    filter(evol_12mois >0 ) %>% 
    arrange(desc(evol_12mois)) %>% 
    mutate(libelle_final = glue("{libelle_texte_fr} de {evol_12mois}%") ) %>% 
    pull(libelle_final)
  
  pdt_alim_12mois_baisse <- ipc_histo_note %>% 
    filter(nchar(code) == 4 & substr(code,1,2) == "01" | code == "0220" ) %>% 
    left_join(lib3_texte, by = c("code" = "code")) %>% 
    select(ville,libelle_texte_fr,evol_12mois) %>% 
    filter(evol_12mois <0 ) %>% 
    arrange(evol_12mois) %>% 
    mutate(libelle_final = glue("{libelle_texte_fr} de {evol_12mois}%") ) %>% 
    pull(libelle_final)
  
  texte_alim_1mois_hausse <- concat_fr(pdt_alim_1mois_hausse)
  texte_alim_1mois_baisse <- concat_fr(pdt_alim_1mois_baisse)
  texte_nonalim_1mois_hausse <- concat_fr(pdt_nonalim_1mois_hausse)
  texte_nonalim_1mois_baisse <- concat_fr(pdt_nonalim_1mois_baisse)
  
  texte_alim_12mois_hausse <- concat_fr(pdt_alim_12mois_hausse)
  texte_alim_12mois_baisse <- concat_fr(pdt_alim_12mois_baisse)
  
  #####   phrase01   #######
  evol_1mois <- ipc_histo_note %>% 
    filter(code=='000GEN') %>% 
    pull(evol_1mois)
  
  p01_variable <- case_when(evol_1mois > 0  ~ "p01_plus",
                            evol_1mois < 0  ~ "p01_moins",
                            TRUE ~ "p01_neutre" )
  
  #####   phrase02   #######
  evol_12mois <- ipc_histo_note %>%
    filter(code=='000GEN') %>%
    pull(evol_12mois)

  p02_variable <- case_when(evol_12mois > 0  ~ "p02_plus",
                            evol_12mois < 0  ~ "p02_moins",
                            TRUE ~ "p02_neutre"  )
  
  #####   phrase3   #######
  evol_alim_1mois <- ipc_histo_note %>% 
    filter(code=='001ALIM') %>%
    pull(evol_1mois)
  
  p03_variable <- case_when(evol_alim_1mois > 0  ~ "p03_plus",
                            evol_alim_1mois < 0  ~ "p03_moins",
                            TRUE ~ "p03_neutre")
  
  #####   phrase4   #######
  evol_nonalim_1mois <- ipc_histo_note %>% 
    filter(code=='001NONALIM') %>%
    pull(evol_1mois)
  
  p04_variable <- case_when(evol_nonalim_1mois > 0  ~ "p04_plus",
                            evol_nonalim_1mois < 0  ~ "p04_moins",
                            TRUE ~ "p04_neutre")
  
  #####   phrase5   ####### 
  evol_alim_12mois <- ipc_histo_note %>% 
    filter(code=='001ALIM') %>%
    pull(evol_12mois) 
  
  p05_variable <- case_when(evol_alim_12mois > 0  ~ "p05_plus",
                            evol_alim_12mois < 0  ~ "p05_moins",
                            TRUE ~ "p05_neutre")
 
  #####   phrase6   #######
  evol_nonalim_12mois <- ipc_histo_note %>% 
    filter(code=='001NONALIM') %>%
    pull(evol_12mois)
  
  p06_variable <- case_when(evol_nonalim_12mois > 0  ~ "p06_plus",
                            evol_nonalim_12mois < 0  ~ "p06_moins",
                            TRUE ~ "p06_neutre") 
  ##### lecture du fichier de redaction
  df_variables <- data.frame(var_redaction = c(p01_variable,
                                               p02_variable,
                                               p03_variable,
                                               p04_variable,
                                               p05_variable,
                                               p06_variable))
  
  readaction_template <- read.xlsx("../redaction_template.xlsx")
  df_redaction <- df_variables %>% 
      left_join(readaction_template, by = c("var_redaction" = "variable")) %>% 
      select("code","var_redaction",langue) 
  
  phrase01 <- glue(get_variable_texte(df_redaction,'phrase01'))
  phrase02 <- glue(get_variable_texte(df_redaction,'phrase02'))
  phrase03 <- glue(get_variable_texte(df_redaction,'phrase03'))
  phrase04 <- glue(get_variable_texte(df_redaction,'phrase04'))
  phrase05 <- glue(get_variable_texte(df_redaction,'phrase05'))
  phrase06 <- glue(get_variable_texte(df_redaction,'phrase06'))
  
  resultat <- list(
    ville = ville,
    lib_mois_annee_m = lib_mois_annee_m,
    lib_mois_annee_m1 = lib_mois_annee_m1,
    lib_mois_annee_m12 = lib_mois_annee_m12,
    texte_alim_1mois_hausse = texte_alim_1mois_hausse,
    texte_alim_1mois_baisse = texte_alim_1mois_baisse,
    texte_alim_12mois_hausse = texte_alim_12mois_hausse,
    texte_alim_12mois_baisse = texte_alim_12mois_baisse,
    phrase01 = phrase01,
    phrase02 = phrase02,
    phrase03 = phrase03,
    phrase04 = phrase04,
    phrase05 = phrase05,
    phrase06 = phrase06)
  
  return(resultat)
}

