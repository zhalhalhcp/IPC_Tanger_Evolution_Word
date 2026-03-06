setwd("../..")
source(file="Programmes/Sous_programmes/fonctions.R")
source(file="Programmes/Sous_programmes/fonctions_graphiques.R")

#retrouver_donnees(p_mois_courant,code_ville,langue)
retrouver_donnees <- function(p_mois_courant,code_ville,langue) {
  library(lubridate)
  library(dplyr)
  library(openxlsx)
  library(janitor)
  library(tidyverse)
  
  #cat(">>> p_mois_courant reçu dans le script:", p_mois_courant, "\n")
   # p_mois_courant = 'p_2025_11'
   # langue='fr'
   # code_ville='10'

  #Retrouver les variables à utiliser dans le GLUE final ou bien dans les fonctions
  ville <- get_lib_ville(code_ville,langue)
  periode_analyse <- substr(p_mois_courant,3,9)
  date_analyse <- ym(periode_analyse)
  var_analyse <- format(date_analyse, "V%Y_%m")
  date_analyse_m1 <- date_analyse - months(1)
  date_analyse_m2 <- date_analyse - months(2)
  date_analyse_m3 <- date_analyse - months(3)
  date_analyse_m12 <- date_analyse - years(1)
  date_analyse_m13 <- date_analyse - months(13)
  
  var_analyse <- format(date_analyse, "V%Y_%m")
  var_analyse_m1 <- format(date_analyse_m1, "V%Y_%m")
  var_analyse_m2 <- format(date_analyse_m2, "V%Y_%m")
  var_analyse_m3 <- format(date_analyse_m3, "V%Y_%m")
  var_analyse_m12 <- format(date_analyse_m12, "V%Y_%m")
  var_analyse_m13 <- format(date_analyse_m13, "V%Y_%m")

  #ipc_histo <- readRDS("../../Data_locales/IPC_histo_dernier.rds")
  ipc_histo <- readRDS("Data_locales/IPC_histo_dernier.rds")
  liste_var_tab <- c("ville","code","libelle_diff",
                     var_analyse_m12,var_analyse_m3,var_analyse_m2,var_analyse_m1,var_analyse)
  
  ipc_histo_tableau <- ipc_histo %>% 
    filter(ville ==  code_ville) %>% 
    select(all_of(liste_var_tab)) %>% 
    mutate(
      evol_1mois = round(((.data[[var_analyse]] / .data[[var_analyse_m1]]) -1) *100,1),
      evol_2mois = round(((.data[[var_analyse]] / .data[[var_analyse_m2]]) -1) *100,1),
      evol_3mois = round(((.data[[var_analyse]] / .data[[var_analyse_m3]]) -1) *100,1),
      evol_12mois = round(((.data[[var_analyse]] / .data[[var_analyse_m12]]) -1) *100,1)
    )
  
  readaction_template <- read.xlsx("Input/redaction_template.xlsx")
  
  #############################p01#######################
  #variables de p01
  evol_1mois <- ipc_histo_tableau %>% filter(code=='000GEN') %>% pull(evol_1mois)
  annee_courante <- substr(periode_analyse,1,4)
  mois_courant <- substr(periode_analyse,6,7)
  lib_mois_courant <- lib_mois%>% 
    filter(code == mois_courant) %>% 
    select(langue) %>% 
    pull()
  #texte du p01
  p01_variable <- case_when(
    evol_1mois > 0  ~ "p01_plus",
    evol_1mois < 0  ~ "p01_moins",
    TRUE ~ "p01_neutre"
  )
  
  #variables de p02
  evol_12mois <- ipc_histo_tableau %>% filter(code=='000GEN') %>% pull(evol_12mois)
  #texte du p02
  p02_variable <- case_when(
    evol_12mois > 0  ~ "p02_plus",
    evol_12mois < 0  ~ "p02_moins",
    TRUE ~ "p02_neutre"
  )
  

#############################P03#######################
  
  evol_alim_1mois <- ipc_histo_tableau %>% filter(code=='001ALIM') %>% pull(evol_1mois)
  p03_variable <- case_when(
    evol_alim_1mois > 0  ~ "p03_plus",
    evol_alim_1mois < 0  ~ "p03_moins",
    TRUE ~ "p03_neutre"
  )
  annee_m1 <- format(date_analyse_m1, "%Y")
  mois_m1 <- format(date_analyse_m1, "%m")
  lib_mois_m1 <- lib_mois%>% 
    filter(code == mois_m1) %>% 
    select(langue) %>% 
    pull()
  #############################p04#######################  
 
  p04_variable <- case_when(
    evol_alim_1mois > 0  ~ "p04_plus",
    evol_alim_1mois < 0  ~ "p04_moins",
    TRUE ~ "p04_neutre"
  )
  df_evol_1mois <- ipc_histo_tableau %>% 
    select(ville,code,libelle_diff,evol_1mois)
  p04_vars <- get_p04_vars(df_evol_1mois,langue)
  liste_pdt_alim_1mois_plus <- p04_vars[1]
  liste_pdt_alim_1mois_neutre <- p04_vars[2]
  liste_pdt_alim_1mois_moins <- p04_vars[3]



  #############################p05#######################
  evol_non_alim_1mois <- ipc_histo_tableau %>% filter(code=='001NONALIM') %>% pull(evol_1mois)
  p05_variable <- case_when(
    (evol_non_alim_1mois*evol_alim_1mois)<0 ~ "p05_moins",
    (evol_non_alim_1mois*evol_alim_1mois)>0 ~ "p05_plus",
    TRUE ~ "p05_neutre",
  )
  #############################p06#######################
  p06_variable <- case_when(
    evol_non_alim_1mois > 0  ~ "p06_plus",
    evol_non_alim_1mois < 0  ~ "p06_moins",
    TRUE ~ "p06_neutre"
  )
  p06_vars <- get_p06_vars(df_evol_1mois,langue)
  liste_div_non_alim_plus <- p06_vars[1]
  liste_div_non_alim_neutre <- p06_vars[2]
  liste_div_non_alim_moins <- p06_vars[3]
  
  #############################p07#######################
  
  evol_alim_12mois <- ipc_histo_tableau %>% filter(code=='001ALIM') %>% pull(evol_12mois)
  
  p07_variable <- case_when(
    evol_alim_12mois > 0  ~ "p07_plus",
    evol_alim_12mois < 0  ~ "p07_moins",
    TRUE ~ "p07_neutre"
  )
  annee_m12 <- format(date_analyse_m12, "%Y")
  df_evol_12mois <- ipc_histo_tableau %>% 
    select(ville,code,libelle_diff,evol_12mois)
  p07_vars <- get_p07_vars(df_evol_12mois,langue)
  liste_pdt_alim_12mois_plus <- p07_vars[1]
  liste_pdt_alim_12mois_neutre <- p07_vars[2]
  liste_pdt_alim_12mois_moins <- p07_vars[3]
  
  #############################p08#######################
  evol_non_alim_12mois <- ipc_histo_tableau %>% filter(code=='001NONALIM') %>% pull(evol_12mois)
  p08_variable <- case_when(
    (evol_alim_12mois*evol_non_alim_12mois)<0 ~ "p08_moins",
    (evol_alim_12mois*evol_non_alim_12mois)>0 ~ "p08_plus",
    TRUE ~ "p08_neutre",
  )
  
  #############################p09#######################
  p09_variable <- case_when(
    (evol_non_alim_12mois)<0 ~ "p09_moins",
    (evol_non_alim_12mois)>0 ~ "p09_plus",
    TRUE ~ "p09_neutre",
  )
  
  #############################p10#######################
  p10_variable = 'p10'
  p10_vars <- get_p10_vars(df_evol_12mois,langue)
  liste_min_div_non_alim_12mois_moins <- p10_vars[1]
  liste_max_div_non_alim_12mois_plus <- p10_vars[2]
  ############################p11#######################
  #graphique annuel
  #ipc_evol_annuel_gra <- get_evol_annuel_gra(code_ville,ipc_histo,var_analyse)
  graph_variation_annuelle <- dessiner_graphique_evol_annuel(ipc_evol_annuel_gra,langue)
  p11_num <- case_when(
    code_ville == '17'  ~ "1-",
    code_ville == '08'  ~ "5-",
    code_ville == '10'  ~ "3-",
    TRUE ~ ''
  )
  p11_variable = 'p11'
  p12 <- graph_variation_annuelle
  
  #############################p13#######################
  
  p13_variable <- case_when(
    evol_12mois > 0  ~ "p13_plus",
    evol_12mois < 0  ~ "p13_moins",
    TRUE ~ "p13_neutre"
  )

  
  ############################p14#######################
  #graphique mensuel
  #ipc_evol_mensuel_gra <- get_evol_mensuel_gra(code_ville,ipc_histo,var_analyse)
  graph_variation_mensuelle <- dessiner_graphique_evol_mensuel(ipc_evol_mensuel_gra,langue)
  p14_num <- case_when(
    code_ville == '17'  ~ "2-",
    code_ville == '08'  ~ "6-",
    code_ville == '10'  ~ "4-",
    TRUE ~ ''
  )
  p14_variable = 'p14'
  p15 <- graph_variation_mensuelle
  
  #############################p16#######################
  p16_variable <- case_when(
    evol_1mois > 0  ~ "p16_plus",
    evol_1mois < 0  ~ "p16_moins",
    TRUE ~ "p16_neutre"
  )
  
  #consituter un dataframe qui contient le code et le texte
  
  df_variables <- data.frame(var_redaction = c(
    p01_variable, 
    p02_variable,
    p03_variable,
    p04_variable,
    p05_variable,
    p06_variable,
    p07_variable,
    p08_variable,
    p09_variable,
    p10_variable,
    p11_variable,
    p13_variable,
    p14_variable,
    p16_variable
    )
    )
  readaction_template <- read.xlsx("Input/redaction_template.xlsx")
  df_redaction <- df_variables %>% 
    left_join(readaction_template, by = c("var_redaction" = "variable")) %>% 
    select("code","var_redaction",langue)
  
  p01 <- glue(get_variable_texte(df_redaction,'p01',langue))
  p02 <- glue(get_variable_texte(df_redaction,'p02',langue))
  p03 <- glue(get_variable_texte(df_redaction,'p03',langue))
  p04 <- glue(get_variable_texte(df_redaction,'p04',langue))
  p05 <- glue(get_variable_texte(df_redaction,'p05',langue))
  p06 <- glue(get_variable_texte(df_redaction,'p06',langue))
  p07 <- glue(get_variable_texte(df_redaction,'p07',langue))
  p08 <- glue(get_variable_texte(df_redaction,'p08',langue))
  p09 <- glue(get_variable_texte(df_redaction,'p09',langue))
  p10 <- glue(get_variable_texte(df_redaction,'p10',langue))
  p11 <- glue(get_variable_texte(df_redaction,'p11',langue))
  p12 <- p12
  p13 <- glue(get_variable_texte(df_redaction,'p13',langue))
  p14 <- glue(get_variable_texte(df_redaction,'p14',langue))
  p15 <- p15
  p16 <- glue(get_variable_texte(df_redaction,'p16',langue))
  
  if (p05=='NA') {
    p05 <- ''
  }
 
     
  resultat <- list(
    ville = ville,
    p01 = p01,
    p02 = p02,
    p03 = p03,
    p04 = p04,
    p05 = p05,
    p06 = p06,
    p07 = p07,
    p08 = p08,
    p09 = p09,
    p10 = p10,
    p11_num = p11_num,
    p11 = p11,
    p12 = p12,
    p13 = p13,
    p14_num = p14_num,
    p14 = p14,
    p15 = p15,
    p16 = p16
  )
  return(resultat)
}
