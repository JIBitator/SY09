adq.app <- function(Xapp, zapp) { 
  res <- NULL
  res$pi1 <- nrow(Xapp[zapp==1,])/nrow(Xapp)
  res$pi2 <- nrow(Xapp[zapp==2,])/nrow(Xapp)
  res$mu1 <- apply(Xapp[zapp==1,], 2, mean)
  res$mu2 <- apply(Xapp[zapp==2,], 2, mean)
  res$sigma1 <- var(Xapp[zapp==1,])
  res$sigma2 <- var(Xapp[zapp==2,])
  res
}