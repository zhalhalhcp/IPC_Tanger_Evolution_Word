### programme générant les graphiques de la note au format png

print("Création des illustrations - Graphiques")

library(stringr)
library(ggplot2) 
source(file="Programmes/Sous_programmes/fonctions.R")


### Graphique annuelle

# ipc_evol_annuel_gra <- ipc_evol_annuel_gra %>% 
#   mutate(x_date = ym(gsub("/", "-", x_date) ),
#          x_label = str_to_sentence(format(as.Date(x_date), "%B %Y")),  # Texte d'affichage
#          x_label = factor(x_label, levels = x_label) )  # Facteur ordonné
for (langue in c('fr','ang','ar')) {
  # load("Programmes/Sous_programmes/illustrations_old.RData")
  load("Programmes/Sous_programmes/illustrations.RData")
  ipc_evol_annuel_gra$x_date <- ym(gsub("/", "-", ipc_evol_annuel_gra$x_date))
  ipc_evol_annuel_gra <- ipc_evol_annuel_gra %>%
    mutate(
      mois = str_to_sentence(format(as.Date(x_date), "%m")),  
      annee = str_to_sentence(format(as.Date(x_date), "%Y")), 
    ) %>% left_join(lib_mois, by = c("mois" = "code")) %>% 
    mutate(
      x_label = if (langue == "ar") {
        paste0(.data[[langue]], " ", annee)
      } else {
        paste(.data[[langue]], annee)
      }
    ) %>% 
    select(c(code,libelle_diff,x_date,ipc,evol,x_label)) %>% 
    rename(x_label=6) %>% 
    mutate(x_label = factor(x_label, levels = unique(x_label[order(x_date)])))
  
  # 1. Calculez les valeurs min et max de la colonne 'evol'
  min_evol <- min(ipc_evol_annuel_gra$evol, na.rm = TRUE)
  max_evol <- max(ipc_evol_annuel_gra$evol, na.rm = TRUE)
  
  # 2. Définissez une petite marge (ex: 10% de l'amplitude totale)
  marge <- (max_evol - min_evol) * 0.10
  y_min_axis <- floor((min_evol - marge) * 2) / 2
  y_max_axis <- ceiling((max_evol + marge) * 2) / 2
  
  # creation du graphique
  graph_variation_annuelle <- ggplot(
    data = ipc_evol_annuel_gra,
    aes(x = x_label, y = evol, group = 1)
  ) +
    scale_y_continuous(
      limits = c(y_min_axis, y_max_axis), 
      breaks = scales::pretty_breaks(n = 6),
      labels = function(x) sprintf("%.1f", x)
    )+
    geom_line(color = "#bd4a47", linewidth = 2) +
    geom_point(color = "black", size = 2.5) +
    # geom_text(
    #   aes(label = evol, y = evol + ifelse(evol == max(evol), -0.5, 0.5)),
    #   color = "black", size = 4, fontface = "bold"
    # ) +
    
    geom_hline(yintercept = 0, color = "gray20", linewidth = 1) +
    labs(x = "Période", y = "Évolution (%)") +
    theme_minimal(base_size = 13,
                  base_family = "Times New Roman") +
    theme(
      axis.title.x = element_blank(),  # Supprime le titre de l'axe X
      axis.title.y = element_blank(),
      axis.text.x = element_text(
        size = 18, family = "Times New Roman",
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
  
  print(graph_variation_annuelle)
  
  grDevices::dev.print(
    device = png,
    filename = glue(
      "Rapports_mensuels/Mois{analyse}/Graphique_variation_annuelle{analyse}_Ville{ville}_{langue}.png"
    ),
    width = 8.91,
    height = 4.26,
    units = "in",
    res = 300
  )
  
  ### Graphique mensuelle
  
  # ipc_evol_mensuel_gra <- ipc_evol_mensuel_gra %>% 
  #   mutate(x_date = ym(gsub("/", "-", x_date) ),
  #          x_label = str_to_sentence(format(as.Date(x_date), "%B %Y")),  # Texte d'affichage
  #          x_label = factor(x_label, levels = x_label) )  # Facteur ordonné
  
  ipc_evol_mensuel_gra$x_date <- ym(gsub("/", "-", ipc_evol_mensuel_gra$x_date))
  ipc_evol_mensuel_gra <- ipc_evol_mensuel_gra %>%
    mutate(
      mois = str_to_sentence(format(as.Date(x_date), "%m")),  
      annee = str_to_sentence(format(as.Date(x_date), "%Y")), 
    ) %>% left_join(lib_mois, by = c("mois" = "code")) %>% 
    mutate(
      fr = paste(fr,annee),
      ar = paste(annee,ar),
      ang = paste(ang,annee),
    ) %>% 
    select(c(code,libelle_diff,x_date,ipc,evol,langue)) %>% 
    rename(x_label=6) %>% 
    mutate(x_label = factor(x_label, levels = unique(x_label[order(x_date)])))
  
  # 1. Calculez les valeurs min et max de la colonne 'evol'
  min_evol <- min(ipc_evol_mensuel_gra$evol, na.rm = TRUE)
  max_evol <- max(ipc_evol_mensuel_gra$evol, na.rm = TRUE)
  
  # 2. Définissez une petite marge (ex: 10% de l'amplitude totale)
  marge <- (max_evol - min_evol) * 0.10
  y_min_axis <- floor((min_evol - marge) * 2) / 2
  y_max_axis <- ceiling((max_evol + marge) * 2) / 2
  
  # creation du graphique
  
  graph_variation_mensuelle <- ggplot(
    data = ipc_evol_mensuel_gra,
    aes(x = x_label, y = evol, group = 1)
  ) +
    scale_y_continuous(
      limits = c(y_min_axis,y_max_axis), 
      breaks = scales::pretty_breaks(n = 6),
      labels = function(x) sprintf("%.1f", x)
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
        size = 16, family = "Times New Roman",
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
  
  print(graph_variation_mensuelle)
  
  
  grDevices::dev.print(
    device = png,
    filename = glue(
      "Rapports_mensuels/Mois{analyse}/Graphique_variation_mensuelle{analyse}_Ville{ville}_{langue}.png"
    ),
    width = 8.91,
    height = 4.26,
    units = "in",
    res = 300
  )
  # ggsave(glue("Rapports_mensuels/Mois{analyse}/Graphique_variation_mensuelle{analyse}_Ville{ville}_{langue}.png"),
  #        plot = graph_variation_mensuelle, dpi = 300)
}

#### fin de programme #####


