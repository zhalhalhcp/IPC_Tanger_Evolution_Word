### Chargement de la fonction round_half

round_half <- function(x, digits = 0) {
  factor <- 10^digits
  floor(x * factor + 0.5) / factor
}