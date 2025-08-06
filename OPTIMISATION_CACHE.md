# Optimisation du Cache pour les Fréquences Bootstrap

## Problème identifié

Lorsque l'utilisateur modifiait la méthode de sélection (`selectmethod`) dans l'interface utilisateur, les fréquences bootstrap étaient recalculées à chaque fois, même si les données et les paramètres de test restaient identiques. Cela causait des temps de calcul inutiles et une mauvaise expérience utilisateur.

## Solution implémentée

### 1. Système de cache intelligent

Un système de cache a été mis en place pour stocker les résultats des calculs bootstrap basé sur :
- Tous les paramètres de sélection (`prctvalues`, `selectmethod`, `NAstructure`, etc.)
- Tous les paramètres de transformation (`rempNA`, `log`, `standardization`, etc.)
- Tous les paramètres de test bootstrap (`bootstrap_test`, `thresholdFCOOB`, etc.)
- Un hash MD5 des données d'entrée pour détecter les changements de données

**Note importante** : La méthode de seuil (`seuil_method`) n'est PAS incluse dans la clé de cache car elle n'affecte pas le calcul des fréquences, seulement leur interprétation.

### 2. Gestion automatique du cache

Le cache est automatiquement :
- **Nettoyé** quand les données changent (nouveau fichier importé)
- **Nettoyé** quand les paramètres de transformation changent
- **Nettoyé** quand les paramètres de test bootstrap changent
- **Utilisé** quand tous les paramètres restent identiques

### 3. Interface utilisateur améliorée

- **Indicateur visuel** : Message informatif sur l'optimisation du cache
- **Bouton de nettoyage manuel** : Permet à l'utilisateur de vider le cache manuellement
- **Notifications** : Informe l'utilisateur quand le cache est utilisé ou quand de nouveaux calculs sont effectués

## Avantages

1. **Performance** : Évite les recalculs inutiles
2. **Expérience utilisateur** : Réponses plus rapides lors des modifications de paramètres
3. **Transparence** : L'utilisateur sait quand le cache est utilisé
4. **Contrôle** : Possibilité de nettoyer manuellement le cache
5. **Optimisation des seuils** : Changement de méthode de seuil sans recalcul des fréquences

## Optimisation spécifique : Méthodes de seuil

### Problème initial
Changer la méthode de seuil (`seuil_method`) déclenchait un recalcul complet des fréquences bootstrap, ce qui était inutile car les fréquences ne dépendent pas de la méthode de seuil.

### Solution
- **Séparation des préoccupations** : Le calcul des fréquences est séparé de l'application du seuil
- **Application dynamique** : Quand le cache est utilisé, la méthode de seuil actuelle est appliquée dynamiquement sur les fréquences en cache
- **Pas de recalcul** : Changer de méthode de seuil ne déclenche plus de recalcul

### Méthodes de seuil supportées
- **Elbow method** : Calcul automatique du point de coude
- **GMM** : Seuil basé sur le modèle de mélange gaussien
- **Inflection point** : Point d'inflexion dans la distribution
- **Intervalle de confiance** : Seuil basé sur l'intervalle de confiance
- **Manuel** : Seuil défini par l'utilisateur

## Utilisation

### Comportement automatique
- Quand vous changez la méthode de sélection sans modifier les autres paramètres, les résultats sont récupérés depuis le cache
- Quand vous changez la méthode de seuil, les fréquences sont récupérées depuis le cache et le nouveau seuil est appliqué dynamiquement
- Quand vous modifiez des paramètres critiques (transformation, test, etc.), le cache est automatiquement nettoyé et de nouveaux calculs sont effectués

### Nettoyage manuel
- Cliquez sur le bouton "🗑️ Nettoyer le cache" pour vider manuellement le cache
- Une notification confirme le nettoyage

## Code technique

### Clé de cache
```r
cache_key <- paste(
  "bootstrap_cache_",
  input$prctvalues, "_",
  input$selectmethod, "_",
  # ... tous les paramètres ...
  data_hash
)
```

### Vérification du cache
```r
if (exists(cache_key, envir = .GlobalEnv)) {
  # Utiliser le cache
  cached_result <- get(cache_key, envir = .GlobalEnv)
} else {
  # Calculer et mettre en cache
  assign(cache_key, result, envir = .GlobalEnv)
}
```

### Nettoyage automatique
```r
observeEvent(c(input$rempNA, input$log, ...), {
  clear_bootstrap_cache()
})
```

### Application dynamique du seuil
```r
# Quand le cache est utilisé, le seuil est appliqué dynamiquement
if (input$seuil_method == "elbow") {
  seuil <- find_elbow_point(freq)[1]
  selected_vars <- names(freq)[freq >= seuil]
} else if (input$seuil_method == "manual") {
  seuil <- input$stability_threshold
  selected_vars <- names(freq)[freq >= seuil]
}
# etc.
```

## Dépendances ajoutées

- `digest` : Pour générer le hash MD5 des données

## Notes importantes

- Le cache est stocké dans l'environnement global R
- Il est automatiquement nettoyé au redémarrage de l'application
- Les clés de cache sont uniques pour chaque combinaison de paramètres
- Le système est transparent pour l'utilisateur final 