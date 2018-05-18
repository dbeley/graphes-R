Prim <- function(X, A) {
  # Détermine un arbre partiel de poids minimal
  # INPUT
  # X est l'ensemble des sommets
  # A est la matrice d'adjacence pondérée
  # OUTPUT
  # Arbre partiel de poids minimal, composé de deux éléments :
  # X_prime est l'ensemble des sommets
  # U_prime est l'ensemble des arcs
  X_prime <- sample(X, 1) # On ajoute à X' un sommet au hasard
  U_prime <- list()
  indice <- 0
  while (length(setdiff(X, X_prime)) > 0) {
    # Tant que tous les sommets ne sont pas dans X'
    potentiel_j <- X_prime # les sommets de départ sont contenus dans X'
    potentiel_k <- setdiff(X, X_prime) # les sommets d'arrivée sont ceux qui ne sont pas encore dans X'
    
    poids = Inf # On initialise le poids à l'infini
    # On boucle sur les arêtes (j, k)
    for (j in potentiel_j) {
      for (k in potentiel_k) {
        if (A[j, k] != 0 &&
            A[j, k] < poids) {
          # On cherche une arête de poids minimal
          poids <- A[j, k] # On sauvegarde le nouveau poids minimal
          j_final <- j # On sauvegarde le sommet initial de l'arête
          k_final <-
            k # On sauvegarde le sommet d'arrivée de l'arête
        }
      }
    }
    X_prime <- c(X_prime, k_final) # On ajoute le sommet d'arrivée de l'arête à X'
    indice <- indice + 1
    U_prime[[indice]] <- NA # On initialise à NA l'item de la liste
    U_prime[[indice]] <- c(j_final, k_final) # On insère dans U' la nouvelle arête
  }
  return (list(X_prime, U_prime))
}