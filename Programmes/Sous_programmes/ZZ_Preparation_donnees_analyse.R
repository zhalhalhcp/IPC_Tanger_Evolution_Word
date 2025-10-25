
### Script de preparation des données 
### s'execute dans le chunk preparation_donnees
# programme : ZZ_Preparation_donnees_analyse.R

##############################################################
### Traitement des dates utiles pour rapport mensuel (RM)  ###
##############################################################
date_analyse <- ym(periode_analyse) 
date_analyse_m1 <- date_analyse -months(1)  # date_analyse -1mois
date_analyse_m2 <- date_analyse -months(2)  # date_analyse -2mois
date_analyse_m3 <- date_analyse -months(3)  # date_analyse -3mois
date_analyse_m12 <- date_analyse -years(1)  # date_analyse -12mois
date_analyse_m13 <- date_analyse -months(13)  # date_analyse -13mois

### Valeur récupérées
mois_analyse <- format(date_analyse, "%m")

var_analyse <- format(date_analyse, "V%Y_%m")  # varaiable analyse
var_analyse_m1 <- format(date_analyse_m1, "V%Y_%m") # varaiable analyse -1mois
var_analyse_m2 <- format(date_analyse_m2, "V%Y_%m") # varaiable analyse -2mois
var_analyse_m3 <- format(date_analyse_m3, "V%Y_%m") # varaiable analyse -3mois
var_analyse_m12 <- format(date_analyse_m12, "V%Y_%m") # varaiable analyse -12mois
var_analyse_m13 <- format(date_analyse_m13, "V%Y_%m") # varaiable analyse -13mois

#####################################################
## Libellé de la ville pour rapport mensuel (RM)  ###
#####################################################
l_ville <- readRDS("../../Data_locales/lib_ville.rds") %>%
  filter(ville == code_ville) %>%
  select(libville) %>%
  pull()

########################################################
###  Libelle des mois et années pour rapport mensuel ###
########################################################

libmois <- readRDS("../../Data_communes/libmois.rds")
#### l_mois
l_mois <- libmois %>%
  filter(mois == mois_analyse)  %>%
  select(libmois) %>%
  pull()

#### l_mois1
l_mois1 <- libmois %>%
  filter(mois == substr(as.character(date_analyse_m1),6,7))  %>%
  select(libmois) %>%
  pull()
a_mois1 <- substr(as.character(date_analyse_m1),1,4)

#### l_mois2
l_mois2 <- libmois %>%
  filter(mois == substr(as.character(date_analyse_m2),6,7))  %>%
  select(libmois) %>%
  pull()
a_mois2 <- substr(as.character(date_analyse_m2),1,4)

#### l_mois3
l_mois3 <- libmois %>%
 filter(mois == substr(as.character(date_analyse_m3),6,7))  %>%
 select(libmois) %>%
 pull()
a_mois3 <- substr(as.character(date_analyse_m3),1,4)

#### l_mois12
l_mois12 <- libmois %>%
  filter(mois == substr(as.character(date_analyse_m12),6,7))  %>%
  select(libmois) %>%
  pull()
a_mois12 <- substr(as.character(date_analyse_m12),1,4)

################################################################
### Traitement fichier historique pour rapport mensuel (RM)  ###
################################################################

ipc_histo <- readRDS("../../Data_locales/IPC_histo_dernier.rds")

### Variables à conserver dans IPC_histo
# pour T01 à T07
liste_var_tab <- c(var_analyse_m12,var_analyse_m1,var_analyse)
# pour G01 : toutes les années du même mois avant la date du mois
liste_var_gra_ann <- 
  colnames(ipc_histo)[grepl(paste0(mois_analyse,"$"), colnames(ipc_histo))]  
liste_var_gra_ann <- sort(unique(liste_var_gra_ann[liste_var_gra_ann <= var_analyse]))
# pour G02 : les 13 mois avant la date du mois
liste_var_gra_men <- 
  colnames(ipc_histo)[grepl("^V", colnames(ipc_histo)) 
                      & !grepl("99$", colnames(ipc_histo))]
liste_var_gra_men <- sort(unique(liste_var_gra_men[liste_var_gra_men >= var_analyse_m13 &
                                                   liste_var_gra_men <= var_analyse]))
# pour T10 : synthese
liste_var_synth <- c(var_analyse_m12,
                       var_analyse_m3,var_analyse_m2,var_analyse_m1,
                       var_analyse  )
# Regroupement
liste_variables <- sort(unique(c("code","libelle_diff",
                                 liste_var_tab,
                                 liste_var_gra_ann,
                                 liste_var_gra_men,
                                 liste_var_synth)))
##############################


ipc_histo_reduit <- ipc_histo %>% 
  filter(ville ==  code_ville) %>% 
  select(all_of(liste_variables))

### DF : ipc_ensemble :
### T01 : Ensemble
### Evolutions mensuelles et annuelles par grand secteur d'activité
### ---------------------------------------------------------------

ipc_ensemble <- ipc_histo_reduit %>%
  filter(code %in% c("000GEN","001ALIM","001NONALIM")) %>% 
  mutate(evol_mens = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m1]]) -1) *100
                           , 1 ) ,
         evol_an = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m12]]) -1) *100
                           , 1 ) ) %>% 
  select (libelle_diff, evol_mens, evol_an)
 
### DF : ipc_alim - Secteur alimentaire
### T02 : Evolutions mensuelles et annuelles pour les produits alimentaires
### T03 : Les évolutions mensuelles décroissantes des produits alimentaires
### T04 : Les évolutions annuelles décroissantes des produits alimentaires
### ---------------------------------------------------------------------------

ipc_alim <- ipc_histo_reduit %>%
  filter ( code == "001ALIM" |
           (nchar(code) == 4 & substr(code,1,2) %in% c("01","02") ) ) %>% 
  mutate(evol_mens = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m1]]) -1) *100
                           , 1 ) ,
         evol_an = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m12]]) -1) *100
                         , 1 ) ) %>% 
  select (code,libelle_diff, evol_mens, evol_an)

### DF : ipc_nonalim - Secteur non alimentaire
### T05 : Evolutions mensuelles et annuelles pour les produits non alimentaires
### T06 : Les évolutions mensuelles décroissantes des produits non alimentaires
### T07 : Les évolutions annuelles décroissantes des produits non alimentaires
### -------------------------------------------------------------------------------

ipc_nonalim <- ipc_histo_reduit %>%
  filter ( code == "001NONALIM" |
           (nchar(code) == 2 & 
              substr(code,1,2) %in% c("03","04","05","06","07","08",
                                      "09","10","11","12")  ) ) %>%
  mutate(evol_mens = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m1]]) -1) *100
                           , 1 ) ,
         evol_an = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m12]]) -1) *100
                         , 1 ) ) %>% 
  select (code,libelle_diff, evol_mens, evol_an) 

### Graphique annuelle - ipc_evol_annuel et ipc_evol_annuel_gra
### G01 : Graphique de l'évolution annuelle en %
### T08 : Tableau de l'évolution annuelle en %
### --------------------------------------------

ipc_evol_annuel <- ipc_histo_reduit %>%
  filter(code == "000GEN") %>% 
  select(code, libelle_diff, all_of(liste_var_gra_ann))

ipc_evol_annuel <- ipc_evol_annuel %>% 
  pivot_longer(
    cols = starts_with("V"),  # Colonnes à transformer
    names_to = "x_date",           # Nom de la nouvelle colonne contenant les noms de variables
    values_to = "ipc"         # Nom de la colonne avec les valeurs
  ) %>% 
  arrange (x_date) %>%
  mutate(x_date = paste0(substr(x_date,2,5),"/",substr(x_date,7,8)) )%>% 
  mutate(evol = round_half_up( ((ipc / lag(ipc)) -1 ) *100 , 1) )

ipc_evol_annuel_gra <- ipc_evol_annuel %>%
  filter (!is.na(evol) )

### Graphique mensuelle - ipc_evol_mensuel et ipc_evol_mensuel_gra
### G02 : Graphique de l'évolution mensuelle en %
### T09 : Tableau de l'évolution mensuelle en %
### ---------------------------------------------

ipc_evol_mensuel <- ipc_histo_reduit %>%
  filter(code == "000GEN") %>% 
  select(code, libelle_diff, all_of(liste_var_gra_men))

ipc_evol_mensuel <- ipc_evol_mensuel %>% 
  pivot_longer(
    cols = starts_with("V"),  # Colonnes à transformer
    names_to = "x_date",           # Nom de la nouvelle colonne contenant les noms de variables
    values_to = "ipc"         # Nom de la colonne avec les valeurs
  ) %>% 
  arrange (x_date) %>%
  mutate(x_date = paste0(substr(x_date,2,5),"/",substr(x_date,7,8)) )%>% 
  mutate(evol = round_half_up( ((ipc / lag(ipc)) -1 ) *100 , 1) )

ipc_evol_mensuel_gra <- ipc_evol_mensuel %>%
  filter (!is.na(evol) )

### IPC_tableau
### T10 : Tableau de synthèse
### -------------------------

### IPC tableau synthese

# Recuperation des ponderations
ipc_pond <- readRDS(glue("../../Data_locales/ponderation_{code_ville}.rds")) %>%
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
                     "01","02","03","04","05","06","07","08","09","10","11","12")) %>% 
  select(code,libelle_diff,all_of(liste_var_synth)) %>% 
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
                           TRUE ~ "lxx") ,
         libelle_diff_tab_syn = if_else(code %in% c("000GEN","001ALIM","001NONALIM"),
                                        libelle_diff,
                                        paste0(code," - ", libelle_diff) ),
         evol_1mois = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m1]]) -1) *100
                            , 1 ),
         evol_3mois = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m3]]) -1) *100
                            , 1 ),
         evol_12mois = round_half_up(((.data[[var_analyse]] / .data[[var_analyse_m12]]) -1) *100
                             , 1 ) ) %>% 
  arrange(ligne) %>%
  select(all_of(liste_var_sortie_synth))          



####################
### FIN PGM      ###
####################
