Ford_Bellman <- function(X, A, S) {
  # Calcul du plus court chemin d'un graphe
  # INPUT
  # X est l'ensemble des sommets
  # A est la matrice d'adjacence pondérée
  # S est un sommet de X
  # OUTPUT
  # pi est la liste représentant les plus courts chemins
  pi = list()
  pi[[S]] <- 0 # On initialise pi à zéro pour le sommet s
  for (i in setdiff(X, S)) {
    # On initialise pi à l'infini pour tous les autres sommets
    pi[[i]] <- Inf
  }
  
  a_changé = TRUE
  while (a_changé) {
    # Tant qu'au moins une valeur change à chaque itération
    a_changé <- FALSE
    for (i in setdiff(X, S)) {
      # Pour tous les sommets de X qui ne sont pas s
      predec <- which(A[, i] != 0)
      for (j in predec) {
        # Pour tous les prédecesseurs de i
        nv_longueur <- (A[j, i] + pi[[j]]) # On calcule le chemin entre s et i en passant par j
        if (nv_longueur < pi[[i]] && nv_longueur != 0) {
          pi[[i]] <- nv_longueur # On assigne la nouvelle longueur si elle est meilleure que l'ancienne
          a_changé <- TRUE
        }
      }
    }
  }
  return (pi)
}