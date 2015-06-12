log.val = function(beta, Xtst) {
    
    #################### INIT
    Xtst = as.matrix(Xtst)
    beta = as.matrix(beta)

    if(ncol(beta)>nrow(Xtst)) {Xtst = as.matrix(cbind(rep(1,nrow(Xtst)),Xtst))} ### then add intercept if necessary
    prob = matrix(0,nrow=nrow(Xtst),ncol=2)
    colnames(prob) = c("w1","w2")
    
    #################### classement
    prob[,"w1"] = exp(Xtst%*%beta) / (1+exp(Xtst%*%beta))
    prob[,"w2"] = 1 - prob[,1]
    classement = as.integer( prob[,"w1"] < prob[,"w2"] ) + 1
    
    return(list(prob=prob, classement=classement))
}
