adl.app <- function(Xapp, zapp){
  res <- NULL
  res$pi1 <- nrow(Xapp[zapp==1,])/nrow(Xapp)
  res$pi2 <- nrow(Xapp[zapp==2,])/nrow(Xapp)
  res$mu1 <- apply(Xapp[zapp==1,], 2, mean)
  res$mu2 <- apply(Xapp[zapp==2,], 2, mean)
  res$sigma1 <- 1/(nrow(Xapp)-2)*((res$pi1-1)*var(Xapp[zapp==1,])+(res$pi2-1)*var(Xapp[zapp==2,]))
  res$sigma2 <- res$sigma1 
  res
}

adl2.app <- function(Xapp,zapp){
  parametres = list()
  
  sigma = 0
  for(i in 1:2){
    Xapp_i = Xapp[zapp==i,]
    sigma = sigma + var(Xapp_i) * nrow(Xapp_i)
  }
  sigma = sigma / (nrow(Xapp) - 2)
  
  for(i in 1:2){
    pi = sum(zapp == i) / length(zapp)
    mu = colMeans(Xapp[zapp==i,])
    
    parametres[[i]] = list(pi=pi,mu=mu,sigma=sigma)
  }
  
  return(parametres)
}