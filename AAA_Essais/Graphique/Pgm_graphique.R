### créer un graphique

library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(janitor)

date_analyse <- ym("2025_07")
var_analyse <- format(date_analyse, "V%Y_%m")  # variable analyse
mois_analyse <- format(date_analyse, "%m")

# Lecture des données et mise en forme

ipc_histo <- readRDS("Data_locales/IPC_histo_2017_01_2025_07.rds")

liste_var_gra_ann <- 
  colnames(ipc_histo)[grepl(paste0(mois_analyse,"$"), colnames(ipc_histo))]  
liste_var_gra_ann <- sort(unique(liste_var_gra_ann[liste_var_gra_ann <= var_analyse]))

liste_variables <- sort(unique(c("code","libelle_diff",liste_var_gra_ann)))

ipc_histo_reduit <- ipc_histo %>% 
  filter(ville ==  "10") %>% 
  select(all_of(liste_variables))

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

#Ajouter les libellés des dates

ipc_evol_annuel_gra <- ipc_evol_annuel_gra %>% 
  mutate(
    date_x_date =  as.Date(paste0(ipc_evol_annuel_gra$x_date,'/01')),
    libelle_mois = format(date_x_date,"%B %Y"),
    max_val = evol == max(evol),
    min_val = evol == min(evol)
  )



ggplot2::ggplot(data=ipc_evol_annuel_gra, 
                aes(x=libelle_mois, 
                    y=evol,
                    group=1 )) +
  scale_y_continuous(labels = function(y) paste0(y, "%"))+
  geom_line(color = "red",linewidth = 1) +
  geom_hline(yintercept=0, col="black", linewidth=1) +
  geom_point(size=1.5)  +
  xlab("") +
  ylab("") +
  ggtitle("1-Évolution du taux de la variation annuelle (%) de
l’indice des prix à la consommation pour le mois de: ")  +
  theme(plot.title = element_text(size= 16) , 
        axis.title = element_text(size= 13) ,
        axis.text.y = element_text(size= 10,
                                   face="bold"),
        axis.text.x = element_text(size = 10, 
                                   angle = 270, 
                                   vjust = 0.5, 
                                   hjust = 0.5,
                                   face = "bold"),
        
        )
       
