log.val <- function(Xtst, beta){
  Xtst <- as.matrix(Xtst)
  #test sur les dim de beta pour savoir si ordonnée à l'origine
  if(nrow(beta)>ncol(Xtst)){
    #TODO ordonnée à l'origine
  }
  prob <- matrix(ncol=ncol(Xtst), nrow=nrow(Xtst))
  prob[,1] <- exp(Xtst %*% beta)/(1 + exp(Xtst %*% beta))
  prob[,2] <- prob[,1]
  max <- apply(prob, 1, max)
  zval <- apply(prob, 1, function(x){
    if(x[,1]==max[,1]){
      zval[,1] = 1
    } else {
      zval[,2] = 2
    }
  })
  #renvoyer 
  res <- NULL
  res$prob <- prob
  res$zval <- min
  res
}