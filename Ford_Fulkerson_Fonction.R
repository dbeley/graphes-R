Ford_Fulkerson = function(X, A, s, p) {
  # Détermine un flot maximal dans un réseau avec capacités
  # INPUT
  # X est l'ensemble des sommets
  # A est la matrice d'adjacence
  # s est le sommet source
  # p est le sommet puit
  # OUTPUT
  # Phi est le flot de valeur maximale
  
  # Initialisation des variables utilisées
  # m est la matrice des marquages des sommets de A
  m <- matrix(0, nrow = length(X), ncol = 3)
  # Phi est la matrice du flot
  Phi <- matrix(0, nrow = length(X), ncol = length(X))
  V_phi <- 0
  S <- c(s)
  S_barre <- setdiff(X, S)
  
  while (1) {
    m[s,] <- c(Inf, Inf, 1)
    S <- c(s)
    S_barre <- setdiff(X, S)
    # R1 est la matrice de booléen indiquant si pour (i,j) il est possible de faire passer du flot supplémentaire
    R1 <- A - Phi > 0
    # R2 est la matrice de booléen indiquant si pour (j,i) il est possible de faire passer du flot supplémentaire
    R2 <- t(Phi) > 0
    # C est la matrice faisant l'union entre les deux conditions précédentes
    C <- R1 | R2
    
    # Tant que la longueur de C est > 0, avec i dans S, j dans S_barre
    while (length(which(matrix(C[S, S_barre] == TRUE, nrow = length(S), ncol = length(S_barre)), arr.ind = TRUE)) > 0) {
      
      pot_ij <- which(matrix(C[S, S_barre] == TRUE, nrow = length(S), ncol = length(S_barre)), arr.ind = TRUE)
      # On extrait des i et j potentiels le premier élément
      i <- S[pot_ij[1, 1]]
      j <- S_barre[pot_ij[1, 2]]
      
      # Marquage plus
      if ((A[i, j] - Phi[i, j]) > 0) {
        m[j,] <- c(i, min(m[i, 2], A[i, j] - Phi[i, j]), 1)
      }
      # Marquage moins
      else if (Phi[j, i] > 0) {
        m[j,] <- c(i, min(m[i, 2], Phi[j, i]),-1)
      }
      S = c(S, j)
      S_barre <- setdiff(X, S)
      # Puit
      if (j == p) {
        # Valeur totale du flot
        V_phi <- V_phi + m[j, 2]
        # "Aller en 14" dans le pseudo-code
        break
      }
    }
    if (is.element(p, S)) {
      while (j != s) {
        # Marquage plus
        if (m[j, 3] == 1) {
          Phi[m[j, 1], j] <- Phi[m[j, 1], j] + m[p, 2]
        }
        # Marquage moins
        else if (m[j, 3] == -1) {
          Phi[j, m[j, 1]] <- Phi[m[j, 1], j] - m[p, 2]
        }
        j <- m[j, 1]
      }
    }
    else{
      return(Phi)
    }
  }
}