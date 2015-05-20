Kppv.erreur <- function(X, z){ 
  for(i in 1:20){
    donn <- separ2(X, z)
    opt <- kppv.app(donn$Xapp, donn$zapp, donn$Xval, donn$zval, c(3,5,7))
    res[i,1] <- tauxErreur(kppv.val(as.matrix(donn$Xapp), donn$zapp, opt, as.matrix(donn$Xapp)), donn$zapp)
    res[i,2] <- tauxErreur(kppv.val(as.matrix(donn$Xapp), donn$zapp, opt, as.matrix(donn$Xtst)), donn$ztst)
  }
  ret <- NULL
  ret$taux <- res
  ret$estPonc <- estimationPonctuelle(ret$taux)
  ret$IC <- intervalleDeConfiance(ret$taux)
  
  ret
}

