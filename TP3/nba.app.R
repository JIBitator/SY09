nba.app <- function(Xapp, zapp) {
    tmpSigma1 <- matrix(0, nrow=ncol(Xapp), ncol=ncol(Xapp))
    tmpSigma2 <- matrix(0, nrow=ncol(Xapp), ncol=ncol(Xapp)) 
    diag(tmpSigma1) <- diag(var(Xapp[zapp==1,])) 
    diag(tmpSigma2) <- diag(var(Xapp[zapp==2,]))
    res <- NULL
    res$pi1 <- nrow(Xapp[zapp==1,])/nrow(Xapp)
    res$pi2 <- nrow(Xapp[zapp==2,])/nrow(Xapp)
    res$mu1 <- apply(Xapp[zapp==1,], 2, mean)
    res$mu2 <- apply(Xapp[zapp==2,], 2, mean)
    res$sigma1 <- tmpSigma1
    res$sigma2 <- tmpSigma2
    res
}

nba2.app <- function(Xapp,zapp){
  parametres = list()
  
  for(i in 1:2){
    Xapp_i = Xapp[zapp==i,]
    
    pi = sum(zapp == i) / length(zapp)
    mu = colMeans(Xapp_i)
    sigma = diag(diag(var(Xapp_i) * nrow(Xapp_i)/(nrow(Xapp_i)-1)))
    
    parametres[[i]] = list(pi=pi,mu=mu,sigma=sigma)
  }
  
  return(parametres)
}