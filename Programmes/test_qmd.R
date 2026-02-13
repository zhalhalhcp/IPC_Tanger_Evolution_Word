library(dplyr)
library(lubridate)
library(kableExtra) 
library(ggplot2) 
library(tidyr)
library(glue)
library(stringr)
library(janitor)

 # setwd("Programmes/Sous_programmes")
 # setwd("../..")

code_ville <- str_pad('17', width = 2, pad = "0")
periode_analyse <- '2025_09'
source("ZZ_Preparation_donnees_analyse.R")
#Ensemble

kable(ipc_ensemble,
      col.names = c("Secteur",
                    "Evolution mensuelle",
                    "Evolution annuelle"),
      format.args = list(decimal.mark = ","),
      escape = F,
      caption ="<span style='font-size:20px'>Evolutions mensuelles et annuelles<br>par grand secteur</span>") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                position = "left",
                font_size = 14) %>%
  column_spec(1, width = "6cm") %>%
  row_spec(1, bold = T)

#Alim

ipc_alim %>% 
  select (code,libelle_diff,evol_mens,evol_an) %>%
  kable(col.names = c("Code",
                      "Produits",
                      "Evolution mensuelle",
                      "Evolution annuelle" ) ,
        format.args = list(decimal.mark = ","),
        escape = F ,
        caption ="<span style='font-size:20px'>Evolutions mensuelles et annuelles<br>pour les produits alimentaires</span>") %>% 
  kable_styling(bootstrap_options = "striped",  
                full_width = F, 
                position = "left", 
                font_size = 14) %>% 
  row_spec(1, bold = T) %>% 
  column_spec(2, width = "12cm") 

# Mensuel 
ipc_alim_mens <- ipc_alim %>% 
  select (code, libelle_diff, evol_mens) %>%  
  arrange(desc(evol_mens))  

ipc_alim_mens %>%  
  kable(col.names = c("Code",
                      "Produits", 
                      "Evolution mensuelle" ),
        format.args = list(decimal.mark = ","), 
        escape = F, 
        caption ="<span style='font-size:20px'>Les évolutions mensuelles décroissantes des produits alimentaires</span>") %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                position = "left", 
                font_size = 14)  %>% 
  row_spec(which(ipc_alim_mens$code == "001ALIM"), bold = T) %>% 
  column_spec(2, width = "12cm") 

# Annuel 
ipc_alim_an <- ipc_alim %>% 
  select (code,libelle_diff,  evol_an) %>%  
  arrange(desc(evol_an))  

ipc_alim_an %>% 
  kable(col.names = c("Code",
                      "Produits", 
                      "Evolution annuelle" ), 
        format.args = list(decimal.mark = ","), 
        escape = F, 
        caption ="<span style='font-size:20px'>Les évolutions annuelles décroissantes des produits alimentaires</span>") %>%  
  kable_styling(bootstrap_options = "striped",  
                full_width = F,  
                position = "left", 
                font_size = 14)  %>% 
  row_spec(which(ipc_alim_an$code == "001ALIM"), bold = T) %>%  
  column_spec(2, width = "14cm")

##########NON ALIM

ipc_nonalim %>%
  select (code,libelle_diff,evol_mens,evol_an) %>%
  kable(col.names = c("Code",
                      "Produits",
                      "Evolution mensuelle",
                      "Evolution annuelle" ),
        format.args = list(decimal.mark = ","),
        escape = F,
        caption ="<span style='font-size:20px'>Evolutions mensuelles et annuelles<br>pour les produits non alimentaires</span>") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "left",
                font_size = 14) %>%
  row_spec(1, bold = T) %>%
  column_spec(2, width = "12cm")

# Mensuel

ipc_nonalim_mens <- ipc_nonalim %>%
  select (code,libelle_diff,  evol_mens) %>%
  arrange(desc(evol_mens))

ipc_nonalim_mens %>%
  kable(col.names = c("Code",
                      "Produits", 
                      "Evolution mensuelle" ),
        format.args = list(decimal.mark = ","),
        escape = F,
        caption ="<span style='font-size:20px'>Les évolutions mensuelles décroissantes des produits non alimentaires</span>") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "left",
                font_size = 14)  %>%
  row_spec(which(ipc_nonalim_mens$code == "001NONALIM"), bold = T) %>%
  column_spec(2, width = "12cm")

# Annuel

ipc_nonalim_an <- ipc_nonalim %>%
  select (code,libelle_diff,  evol_an) %>%
  arrange(desc(evol_an))

ipc_nonalim_an %>%
  kable(col.names = c("Code",
                      "Produits", 
                      "Evolution annuelle" ),
        format.args = list(decimal.mark = ","),
        escape = F,
        caption ="<span style='font-size:20px'>Les évolutions annuelles décroissantes des produits non alimentaires</span>") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "left",
                font_size = 14)  %>%
  row_spec(which(ipc_nonalim_an$code == "001NONALIM"), bold = T) %>%
  column_spec(2, width = "14cm")

#Graphique1

ggplot2::ggplot(data=ipc_evol_annuel_gra, 
                aes(x=x_date, 
                    y=evol,
                    group=1 )) +
  geom_line(color = "red",linewidth = 1) +
  geom_hline(yintercept=0, col="black", linewidth=1) +
  geom_point(size=1.5)  +
  xlab("Période") +
  ylab("Evolution (%)") +
  ggtitle("Graphique de l'évolution annuelle en %")  +
  theme(plot.title = element_text(size= 16) , 
        axis.title = element_text(size= 13) ,
        axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size= 10) )


ipc_evol_annuel_gra


class(ipc_evol_annuel_gra$x_date)

library(lubridate)
library(ggplot2)

# Conversion depuis le format character
#ipc_evol_annuel_gra$x_date <- ymd(ipc_evol_annuel_gra$x_date)
ipc_evol_annuel_gra$x_date <- ym(gsub("/", "-", ipc_evol_annuel_gra$x_date))




ipc_evol_annuel_gra <- ipc_evol_annuel_gra %>%
  mutate(
    x_label = format(as.Date(x_date), "%B %Y"),  # Texte d'affichage
    x_label = factor(x_label, levels = x_label)  # Facteur ordonné
  )

ggplot(
  data = ipc_evol_annuel_gra,
  aes(x = x_label, y = evol, group = 1)
) +
  geom_line(color = "#bd4a47", linewidth = 1.3) +
  geom_point(color = "#bd4a47", size = 2.5) +
  geom_text(
    aes(label = evol, y = evol + ifelse(evol == max(evol), -0.5, 0.5)),
    color = "black", size = 4, fontface = "bold"
  ) +
  geom_hline(yintercept = 0, color = "gray20", linewidth = 1) +
  labs(x = "Période", y = "Évolution (%)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_blank(),  # Supprime le titre de l'axe X
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      size = 14, family = "Times New Roman",
      color = "black", face = "bold", angle = 90, vjust = 0.5
    ),
    axis.text.y = element_text(
      size = 18, face = "bold",
      family = "Times New Roman", color = "black"
    ),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 1)
  )


ipc_evol_mensuel_gra

ipc_evol_mensuel_gra$x_date <- ym(gsub("/", "-", ipc_evol_mensuel_gra$x_date))




ipc_evol_mensuel_gra <- ipc_evol_mensuel_gra %>%
  mutate(
    x_label = format(as.Date(x_date), "%B %Y"),  # Texte d'affichage
    x_label = factor(x_label, levels = x_label)  # Facteur ordonné
  )

ggplot(
  data = ipc_evol_mensuel_gra,
  aes(x = x_label, y = evol, group = 1)
) +
  geom_line(color = "#bd4a47", linewidth = 1.3) +
  geom_point(color = "#bd4a47", size = 2.5) +
  geom_text(
    aes(label = evol), nudge_y = 0.25,
    #aes(label = evol, y = evol + ifelse(evol == max(evol), -0.5, 0.5)),
    color = "black", size = 4, fontface = "bold"
  ) +
  geom_hline(yintercept = 0, color = "gray20", linewidth = 1) +
  labs(x = "Période", y = "Évolution (%)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_blank(),  # Supprime le titre de l'axe X
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      size = 14, family = "Times New Roman",
      color = "black", face = "bold", angle = 90, vjust = 0.5
    ),
    axis.text.y = element_text(
      size = 18, face = "bold",
      family = "Times New Roman", color = "black"
    ),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 1)
  )





ggplot2::ggplot(data=ipc_evol_mensuel_gra,
                aes(x=x_date,
                    y=evol,
                    group=1 )) +
  geom_line(color = "red",linewidth = 1) +
  geom_hline(yintercept=0, col="black", linewidth=1) +
  geom_point(size=1.5)  +
  xlab("Période") +
  ylab("Evolution (%)") +
  ggtitle("Graphique de l'évolution mensuelle en %")  +
  theme(plot.title = element_text(size= 16),
        axis.title = element_text(size= 13),
        axis.text.x = element_text(size= 8),
        axis.text.y = element_text(size= 10))
