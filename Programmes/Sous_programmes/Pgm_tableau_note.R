### programme générant le tableau de la note au format png

print("Création des illustrations - Tableau")

library(flextable)
library(officer)

load("Programmes/Sous_programmes/illustrations.RData")

ft <- flextable(ipc_tableau)

ft <- set_header_df(
  ft,
  mapping = data.frame(
    col_keys = names(ipc_tableau),
    line1 = c("Divisions de produits", "Pondérations",
              "Indices mensuels","Indices mensuels","Indices mensuels",
              "Indices mensuels","Indices mensuels",
              "Variation (%)","Variation (%)","Variation (%)"),
    line2 = c("Divisions de produits","Pondérations",
              lib_mois_annee_m12,lib_mois_annee_m3,lib_mois_annee_m2,
              lib_mois_annee_m1,lib_mois_annee_m,
              "1 mois","3 mois","12 mois"),
    stringsAsFactors = FALSE
  )
)
ft <- merge_h(ft, part = "header") %>%
  merge_v( part = "header")

# Police général : Times New Roman - 8 - gras
# Espacement avant et apèrs cellule
# Suppression ligne automatique en fin de tableau
# Quadrillage de toutes les cellules
# Met une virgule comme séparateur et un blanc pour les milliers

thin_border <- fp_border(color = "black", width = 0.7)
thin_border_droite <- fp_border(color = "black", width = 0.75)

ft <- ft %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 8, part = "all") %>%
  flextable::bold(bold = TRUE, part = "all") %>%
  padding(padding.top = 1.8, padding.bottom = 1.8, part = "all") %>%
  colformat_num(big.mark = " ", decimal.mark = ",") %>%
  hline( i = c(1,3,4,14,15), border = fp_border()) %>%
  hline_bottom(part="header",border = fp_border() ) %>%
  hline_top(part="header",border = fp_border() ) %>%
  hline(i=1,part="header",border = fp_border() )

  # border_remove()
  # border(border = thin_border, part = "all") %>%
  # border(i = NULL,                 # toutes les lignes
  #       j = ncol(ft$header$dataset),   # dernière colonne
  #       border.right = thin_border_droite,
  #       part = "all")

# En-têtes colonnes centrées
# Espace entre les lignes
ft <- ft %>%
  flextable::align(align = "center", part = "header") %>%
  flextable::line_spacing(space = 1.8, part = "header")

# Fond colorés de certaines lignes
ft <- ft %>% flextable::bg(i = c(1,4,15), bg = "#FBD4B4", part = "body")


# Titre et source
no_border <- fp_border(color = "transparent", width = 0)

ft <- ft %>%
  add_header_lines(values = glue("Tableau 3: Indice des prix à la consommation de la ville de Tanger : {lib_mois_annee_m} (base 2017: 100)") ) %>%
  align(i = 1,part = "header", align = "left") %>%
  font(i = 1,part = "header", fontname = "Times New Roman") %>%
  color(i = 1,part = "header", color = "#8D4355") %>%
  fontsize(i = 1,part = "header", size = 10) %>%
  bold(i = 1,part = "header", bold = TRUE) %>%
  border(i = 1 , part = "header", j = NULL, border.top = no_border) %>%
  border(i = 1 , part = "header", j = NULL, border.left = no_border) %>%
  border(i = 1 , part = "header", j = NULL, border.right = no_border)

ft <- ft %>%
  add_footer_lines(values = "Source: Haut-Commissariat au Plan, Direction de la Statistique – Enquête Nationale sur les Prix à la Consommation") %>%
  font(part = "footer", fontname = "Times New Roman") %>%
  color(part = "footer", color = "#8D4355") %>%
  fontsize(part = "footer", size = 8) %>%
  bold(part = "footer", bold = TRUE)


ft <- flextable::set_table_properties(ft, layout = "fixed") %>%
      flextable::width(j=1:10,
                       width=c(7.8,1.5,rep(1.4, 5),rep(1, 3)),
                       unit = "cm")  %>%
      flextable::align(j = 1, align = "left") %>% 
      flextable::align(j = 3:7, align = "center") 

ft


save_as_image(
  ft,
  path = glue("Rapports_mensuels/Mois{analyse}/Tableau_synthese{analyse}_Ville{ville}.png"))



