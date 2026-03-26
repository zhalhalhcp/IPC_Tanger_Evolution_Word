library(flextable)
library(officer)
library(dplyr)
library(glue)

### programme générant le tableau et le place dans le document word selon 
### les trois langues
lib_mois <- readRDS(("Input/lib_mois.rds"))
div_label <-  list(
  fr = "Divisions de produits",
  ang = "Product Divisions",
  ar = "أقسام المواد")
pond_label <-  list(
  fr = "Pondérations",
  ang = "Weightings",
  ar = "الترجيحات")
indices_label <-  list(
  fr = "Indices mensuels",
  ang = "Monthly index",
  ar = "الرقم الاستدلالي لشهر")
variation_label <-  list(
  fr = "Variation (%)",
  ang = "Variation (%)",
  ar = "التغير (%) منذ")

mois1_label <-  list(
  fr = "1 mois",
  ang = "1 month",
  ar = "شهر واحد")

mois3_label <-  list(
  fr = "3 mois",
  ang = "3 months",
  ar = "3أشهر")

mois12_label <-  list(
  fr = "12 mois",
  ang = "12 months",
  ar = "12 شهرا")

get_lib_mois_annee <- function(lib_mois_annee,langue){
  if (langue == 'fr'){
    return (lib_mois_annee)
  }
  else{
    mois <- sub(" .*", "", lib_mois_annee)
    annee <- sub(".* ", "", lib_mois_annee)
    mois_langue <- lib_mois %>% filter(fr == mois) %>% 
      pull(langue)
    return (paste(mois_langue,annee))
  }
}
get_lib_mois_annee(lib_mois_annee_m12,"ar")
load("Programmes/Sous_programmes/illustrations.RData")


dessiner_tableau_ar <- function(langue){
  lib_divisions <- readRDS("Data_communes/divisions_tableaux.rds") %>% 
    pull(langue)
  ipc_tableau[[1]] <- lib_divisions
  ipc_tableau <- ipc_tableau %>% 
    select(rev(names(.)))
  ft <- flextable(ipc_tableau)
  ft <- set_header_df(
    ft,
    mapping = data.frame(
      col_keys = names(ipc_tableau),
      line1 = c(variation_label[[langue]],variation_label[[langue]],variation_label[[langue]],
                indices_label[[langue]],indices_label[[langue]],
                indices_label[[langue]],indices_label[[langue]],indices_label[[langue]],
                pond_label[[langue]],div_label[[langue]]
                ),
      line2 = c(mois12_label[[langue]],mois3_label[[langue]],mois1_label[[langue]],
                get_lib_mois_annee(lib_mois_annee_m,langue),
                get_lib_mois_annee(lib_mois_annee_m1,langue),
                get_lib_mois_annee(lib_mois_annee_m2,langue),
                get_lib_mois_annee(lib_mois_annee_m3,langue),
                get_lib_mois_annee(lib_mois_annee_m12,langue),
                pond_label[[langue]],div_label[[langue]]
                ),
      stringsAsFactors = FALSE
    )
  )
  
  ft <- merge_h(ft, part = "header") %>%
    merge_v( part = "header")
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
  
  ft <- ft %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::line_spacing(space = 1.8, part = "header")
  
  # Fond colorés de certaines lignes
  ft <- ft %>% flextable::bg(i = c(1,4,15), bg = "#FBD4B4", part = "body")
  ft <- flextable::set_table_properties(ft, layout = "fixed") %>%
    flextable::width(j=1:10,
                     width=c(rep(1.4, 8),1.5,6),
                     unit = "cm")  %>%
    flextable::align(j = 10, align = "right") %>% 
    flextable::align(j = 3:7, align = "center") 
  
  ft
}


dessiner_tableau_fr_ang <- function(langue){
  lib_divisions <- readRDS("Data_communes/divisions_tableaux.rds") %>% 
    pull(langue)
  ipc_tableau[[1]] <- lib_divisions
  ft <- flextable(ipc_tableau)

  
  ft <- set_header_df(
    ft,
    mapping = data.frame(
      col_keys = names(ipc_tableau),
      line1 = c(div_label[[langue]], pond_label[[langue]],
                indices_label[[langue]],indices_label[[langue]],indices_label[[langue]],
                indices_label[[langue]],indices_label[[langue]],
                variation_label[[langue]],variation_label[[langue]],variation_label[[langue]]),
      line2 = c(div_label[[langue]],pond_label[[langue]],
                get_lib_mois_annee(lib_mois_annee_m12,langue),
                get_lib_mois_annee(lib_mois_annee_m3,langue),
                get_lib_mois_annee(lib_mois_annee_m2,langue),
                get_lib_mois_annee(lib_mois_annee_m1,langue),
                get_lib_mois_annee(lib_mois_annee_m,langue),
                mois1_label[[langue]],mois3_label[[langue]],mois12_label[[langue]]),
      stringsAsFactors = FALSE
    )
  )
  ft <- merge_h(ft, part = "header") %>%
    merge_v( part = "header")
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
  ft <- ft %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::line_spacing(space = 1.8, part = "header")
  
  # Fond colorés de certaines lignes
  ft <- ft %>% flextable::bg(i = c(1,4,15), bg = "#FBD4B4", part = "body")
  ft <- flextable::set_table_properties(ft, layout = "fixed") %>%
    flextable::width(j=1:10,
                     width=c(7.8,1.5,rep(1.4, 5),rep(1, 3)),
                     unit = "cm")  %>%
    flextable::align(j = 1, align = "left") %>% 
    flextable::align(j = 3:7, align = "center") 
  return(ft)
}

dessiner_tableau <- function(langue){
  if (langue=='ar'){
    dessiner_tableau_ar(langue)
  }
  else{
    dessiner_tableau_fr_ang(langue)
  }
  }

dessiner_tableau("fr")
dessiner_tableau("ang")
dessiner_tableau("ar")
