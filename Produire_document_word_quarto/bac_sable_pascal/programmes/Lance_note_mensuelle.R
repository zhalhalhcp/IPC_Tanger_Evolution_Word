
#### --------------------------------------------------------------------
#### Production du rapport mensuelle d'aide à la rédaction de la note IPC
#### --------------------------------------------------------------------

rm(list=ls())

print("Traitement de la ville de Tanger")

## Production du html pour une ville

quarto::quarto_render(input = "Produire_document_word_quarto/bac_sable_pascal/Programmes/Modele.qmd" )

