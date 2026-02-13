### créer un graphique

library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)


#lecture des dataframes des graphiques deja enregistres sous rds

ipc_evol_annuel_gra <- readRDS("AAA_Essais/Graphique/ipc_evol_annuel_gra.rds")


ipc_evol_annuel_gra$x_date <- ym(gsub("/", "-", ipc_evol_annuel_gra$x_date))




ipc_evol_annuel_gra <- ipc_evol_annuel_gra %>%
  mutate(
    x_label = str_to_sentence(format(as.Date(x_date), "%B %Y")),  # Texte d'affichage
    x_label = factor(x_label, levels = x_label)  # Facteur ordonné
  )

# 1. Calculez les valeurs min et max de la colonne 'evol'
min_evol <- min(ipc_evol_annuel_gra$evol, na.rm = TRUE)
max_evol <- max(ipc_evol_annuel_gra$evol, na.rm = TRUE)

# 2. Définissez une petite marge (ex: 10% de l'amplitude totale)
marge <- (max_evol - min_evol) * 0.10
y_min_axis <- floor((min_evol - marge) * 2) / 2
y_max_axis <- ceiling((max_evol + marge) * 2) / 2
ggplot(
  data = ipc_evol_annuel_gra,
  aes(x = x_label, y = evol, group = 1)
) +
  scale_y_continuous(
    limits = c(y_min_axis, y_max_axis), 
    breaks = scales::pretty_breaks(n = 6),
  )+
  geom_line(color = "#bd4a47", linewidth = 2) +
  geom_point(color = "black", size = 2.5) +
  # geom_text(
  #   aes(label = evol, y = evol + ifelse(evol == max(evol), -0.5, 0.5)),
  #   color = "black", size = 4, fontface = "bold"
  # ) +
  
  geom_hline(yintercept = 0, color = "gray20", linewidth = 1) +
  labs(x = "Période", y = "Évolution (%)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_blank(),  # Supprime le titre de l'axe X
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      size = 14, family = "Times New Roman",
      color = "black", face = "bold", angle = 270, vjust = 0.5, margin = margin(t = -12) 
    ),
    axis.text.y = element_text(
      size = 18, face = "bold",
      family = "Times New Roman", color = "black"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 1)
  )


ipc_evol_mensuel_gra <- readRDS("AAA_Essais/Graphique/ipc_evol_mensuel_gra.rds")


ipc_evol_mensuel_gra$x_date <- ym(gsub("/", "-", ipc_evol_mensuel_gra$x_date))


#EVolution mensuelle

ipc_evol_mensuel_gra <- ipc_evol_mensuel_gra %>%
  mutate(
    x_label = str_to_sentence(format(as.Date(x_date), "%B %Y")),  # Texte d'affichage
    x_label = factor(x_label, levels = x_label)  # Facteur ordonné
  )

# 1. Calculez les valeurs min et max de la colonne 'evol'
min_evol <- min(ipc_evol_mensuel_gra$evol, na.rm = TRUE)
max_evol <- max(ipc_evol_mensuel_gra$evol, na.rm = TRUE)

# 2. Définissez une petite marge (ex: 10% de l'amplitude totale)
marge <- (max_evol - min_evol) * 0.10
y_min_axis <- floor((min_evol - marge) * 2) / 2
y_max_axis <- ceiling((max_evol + marge) * 2) / 2

ggplot(
  data = ipc_evol_mensuel_gra,
  aes(x = x_label, y = evol, group = 1)
) +
  scale_y_continuous(
    limits = c(y_min_axis,y_max_axis), 
    breaks = scales::pretty_breaks(n = 6),
  )+
  geom_line(color = "#bd4a47", linewidth = 2) +
  geom_point(color = "black", size = 2.5) +
  # geom_text(
  #   aes(label = evol, y = evol + ifelse(evol == max(evol), -0.5, 0.5)),
  #   color = "black", size = 4, fontface = "bold"
  # ) +
  
  geom_hline(yintercept = 0, color = "gray20", linewidth = 1) +
  labs(x = "Période", y = "Évolution (%)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_blank(),  # Supprime le titre de l'axe X
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      size = 14, family = "Times New Roman",
      color = "black", face = "bold", angle = 270, vjust = 0.5, margin = margin(t = -12) , hjust=0
    ),
    axis.text.y = element_text(
      size = 18, face = "bold",
      family = "Times New Roman", color = "black"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 1)
  )
