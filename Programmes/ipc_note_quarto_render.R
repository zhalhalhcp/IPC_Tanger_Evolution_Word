library(glue)

mois_courant <- "p_2025_11"
liste_villes <- c('10','08','17')
libelle_villes <- c('tanger','tetouan','alhoceima')
alias_mois_courant <- substr(mois_courant,3,9)
chemin = paste0("Rapports_mensuels/",substr(mois_courant, 3,9))

if (!dir.exists(chemin)) {
  dir.create(chemin)
  message("Dossier créé : ", chemin)
} else {
  message("Le dossier existe déjà : ", chemin)
}
for (code_ville in liste_villes){

  for (langue in c('fr','ar','ang')) {
    
    # Construire le chemin avec glue()
    input_file <- glue("Programmes/Sous_programmes/ipc_note_{langue}_quarto.qmd")
    
    # Vérifier si le fichier existe avant de lancer Quarto
    if (!file.exists(input_file)) {
      stop(glue("Fichier inexistant : {input_file}"))
    }
    
    # Rendu Quarto
    quarto::quarto_render(
      input = input_file,
      execute_params = list(
        p_mois_courant = mois_courant,
        langue = langue,
        code_ville = code_ville
      )
    )
    
    # Renommage du docx de sortie
    output_file <- glue("Programmes/Sous_programmes/ipc_note_{langue}_quarto.docx")
    target_file <- glue("{chemin}/ipc_note_{code_ville}_{langue}_{alias_mois_courant}.docx")
    file.rename(output_file, target_file)
    
    print(glue("Fin du programme {langue}"))
  }
}

