ad.val = function(parametres, Xtst){
    prob = matrix(0, nrow=nrow(Xtst), ncol=2)
    f_k = matrix(0, nrow=nrow(Xtst), ncol=2)
    
	########### prob
    for(i in 1:2){
        mu = parametres[[i]]$mu
        sigma = parametres[[i]]$sigma 
        f_k[,i] = mvdnorm(Xtst,mu,sigma)
    }
    
    for(i in 1:2){
        prob[,i] = (f_k[,i]*parametres[[i]]$pi) / (f_k[,1]*parametres[[1]]$pi + f_k[,2]*parametres[[2]]$pi)
    }
	
	############ classement
    classement = max.col(prob)
    
    return(list(prob=prob, classement=classement))
}
