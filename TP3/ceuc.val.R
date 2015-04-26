ceuc.val <- function(mu, Xtst){
  sol <- matrix(nrow = nrow(Xtst))
  dist <- matrix(ncol = nrow(mu), nrow = nrow(Xtst))
  for(i in 1:nrow(Xtst)){
    for(j in 1:nrow(mu)){
      dist[i,j] <- sqrt(sum((Xtst[i,]-t(mu[j,]))^2))
    }
  }
  # choisir la colonne de la classe
  #apply(dist, 1, min)
  #apply(Xtxt, 1, apply, 1, mu, sqrt(sum()))
}