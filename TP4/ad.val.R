ad.val <- function(pi1, pi2, mu1, mu2, sigma1, sigma2, Xtst){
  for(i in 1:nrow(Xtst)){
    #calculer g1 et g2
    #calculer f1 et f2 avec mvrnorm
    f1 <- mvrnorm(mu1, sigma1)
    f2 <- mvrnorm(mu2, sigma2)
    P1 <- (pi1*f1)/(pi1*f1+pi2)
    g1 <- -1/2*t(Xtst[i,]-mu1)*solve(sigma1)*(Xtst[i,]-mu1)-1/2
    #calculer P1 et P2
    # prendre le maximum entre P1 et P2 pour déterminer la classe
  }
}