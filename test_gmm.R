# Script de test pour la fonction GMM
# Ce script teste la fonction plot_gmm_threshold_detection

# Charger les packages nécessaires
library(mclust)
library(ggplot2)

# Source du fichier global.R pour charger les fonctions
source("global.R")

# Générer des données de test similaires à celles de l'utilisateur
set.seed(42)

# Générer des données à partir de deux distributions gaussiennes
data1 <- rnorm(300, mean = 0.3, sd = 0.1)  # Distribution 1 (fréquences basses)
data2 <- rnorm(300, mean = 0.8, sd = 0.1)  # Distribution 2 (fréquences hautes)

# Combiner les données
data <- c(data1, data2)

# Créer un dictionnaire de fréquences
frequencies <- setNames(as.list(data), paste0("peptide_", seq_along(data)))

# Tester la fonction plot_gmm_threshold_detection
cat("Test de la fonction plot_gmm_threshold_detection...\n")

tryCatch({
  # Tester le graphique
  gmm_plot <- plot_gmm_threshold_detection(frequencies, plot = TRUE)
  print("✅ Graphique GMM créé avec succès!")
  
  # Tester les statistiques
  gmm_stats <- get_gmm_stats(frequencies)
  cat("✅ Statistiques GMM calculées avec succès!\n")
  cat("Seuil optimal:", round(gmm_stats$threshold, 3), "\n")
  cat("Moyenne composante 1:", round(gmm_stats$means[1], 3), "\n")
  cat("Moyenne composante 2:", round(gmm_stats$means[2], 3), "\n")
  
}, error = function(e) {
  cat("❌ Erreur lors du test GMM:", e$message, "\n")
})

cat("Test terminé!\n") 