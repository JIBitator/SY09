calculErreurs <- function(X, z){
  res <- matrix(nrow=20, ncol=2)
  for(i in 1:20){    
    #Nous séparons aléatoirement les sets de données
    donn.sep <- separ1(X, z)
    mu <- ceuc.app(donn.sep$Xapp, donn.sep$zapp)
    res[i,1] <- tauxErreur(ceuc.val(mu, donn.sep$Xapp), donn.sep$zapp)
    res[i,2] <- tauxErreur(ceuc.val(mu, donn.sep$Xtst), donn.sep$ztst)
  }
  res
}

estimationPonctuelle <- function(err){
  res <- apply(err, 2, mean)
  res
}
intervalleDeConfiance <- function(err){
  res <- matrix(nrow=2, ncol=2)
  res[,1] <- apply(err, 2, min)
  res[,2] <- apply(err, 2, max)
  res
}

tauxErreur <- function(eti1, eti2){
  e = 0.0
  for(i in 1:length(eti1)){
    if(eti1[i] != eti2[i]){
      e = e + 1
    }
  }
  e / length(eti1)
}