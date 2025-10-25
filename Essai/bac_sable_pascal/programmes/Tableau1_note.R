
################################################################
### Traitement fichier historique pour rapport mensuel (RM)  ###
################################################################

library(dplyr)
library(lubridate)
source("function_round_half.R")
# source("Produire_document_word_quarto/bac_sable_pascal/programmes/function_round_half.R")

periode_analyse <- "2025_07"
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

ipc_histo <- readRDS("../data_entree/IPC_histo_dernier.rds")
ipc_pond <- readRDS("../data_entree/ponderation_99.rds")

# ipc_histo <- readRDS("Produire_document_word_quarto/bac_sable_pascal/data_entree/IPC_histo_dernier.rds")
# ipc_pond <- readRDS("Produire_document_word_quarto/bac_sable_pascal/data_entree/ponderation_99.rds")

liste_var_synth <- c(var_analyse_m12,
                     var_analyse_m3,var_analyse_m2,var_analyse_m1,
                     var_analyse  )

ipc_histo_reduit <- ipc_histo %>% 
  filter(ville ==  "99") %>% 
  select("code","libelle_diff",all_of(liste_var_synth))

ipc_pond <- ipc_pond %>% 
  filter(coicop %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))
ipc_pond_reg <- ipc_pond %>%
  mutate(code = if_else(coicop %in% c("01","02") , "001ALIM","001NONALIM") ) %>%
  group_by(code) %>%
  summarise(ponderation= sum(ponderation, na.rm=T))
icp_pond_ens <- ipc_pond %>%
  rename(code = coicop ) %>%
  bind_rows(ipc_pond_reg)

liste_var_sortie_synth<-  c("libelle_diff_tab_syn","ponderation",
                            var_analyse_m12,var_analyse_m3,
                            var_analyse_m2, var_analyse_m1,
                            var_analyse,
                            "evol_1mois","evol_3mois","evol_12mois")

ipc_tableau <- ipc_histo_reduit %>%
  filter(code %in% c("000GEN","001ALIM","001NONALIM",
                     "01","02","03","04","05","06","07","08","09","10","11","12"))  %>% 
  left_join(icp_pond_ens, by = "code" ) %>% 
  mutate(ligne = case_when(code == "001ALIM" ~ "l01",
                           code == "01" ~ "l02",
                           code == "02" ~ "l03",
                           code == "001NONALIM" ~ "l04",
                           code == "03" ~ "l05",
                           code == "04" ~ "l06",
                           code == "05" ~ "l07",
                           code == "06" ~ "l08",
                           code == "07" ~ "l09",
                           code == "08" ~ "l10",
                           code == "09" ~ "l11",
                           code == "10" ~ "l12",
                           code == "11" ~ "l13",
                           code == "12" ~ "l14",
                           code == "GENERAL" ~ "l15" ,
                           TRUE ~ "lxx"),
         libelle_diff_tab_syn = if_else(code %in% c("000GEN","001ALIM","001NONALIM"),
                                        libelle_diff,
                                        paste0(code," - ", libelle_diff) ),
         evol_1mois = round_half(((.data[[var_analyse]] / .data[[var_analyse_m1]]) -1) *100
                                 , 1 ),
         evol_3mois = round_half(((.data[[var_analyse]] / .data[[var_analyse_m3]]) -1) *100
                                 , 1 ),
         evol_12mois = round_half(((.data[[var_analyse]] / .data[[var_analyse_m12]]) -1) *100
                                  , 1 ) ) %>% 
      arrange(ligne) %>%
      select(all_of(liste_var_sortie_synth))   
  

#####################

