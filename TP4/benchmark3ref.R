
benchmark3reg = function() {
  
  benchmarks = array(0,dim=c(20,3,2))
  dimnames(benchmarks)[[3]] = c("log.app","logQuad.app")
  dimnames(benchmarks)[[2]] = c("Piwa","Piwa","Piwa")
  
  ######################## benchmarks
  for(i in 1:3) {
    X = Pima[,1:7]
    z = Pima[,8]
    
    for(j in 1:20) {
      separation = separ1(X,z)
      Xapp = separation$Xapp
      Xtst = separation$Xtst
      zapp = separation$zapp
      ztst = separation$ztst
      
      for(f in c("log.app","logQuad.app")) {
        fct = get(f)
        if(f == "logQuad.app") {
          combinations = combn(1:ncol(Xtst),2)
          XtstCombinations = matrix(0,nrow(Xtst),ncol(combinations))
          for(ii in 1:ncol(combinations)){
            XtstCombinations[,ii] = Xtst[,combinations[1,ii]] * Xtst[,combinations[2,ii]]
          }
          Xtst = cbind(Xtst,XtstCombinations,Xtst^2)	
        }				
        parametres = fct(Xapp,zapp,0,1e-5)
        classement = log.val(parametres$beta,Xtst)$classement
        benchmarks[j,i,f] = sum(classement==ztst) / length(ztst)
      }
      
    }
  }
  
  ######################## means
  resume = list()
  for(f in c("log.app","logQuad.app")) {
    resume[[f]]$mean_by_file = apply(benchmarks[1:20,,f],2,mean)
    resume[[f]]$mean = sum(resume[[f]]$mean_by_file) / 3
  }
  
  
  
  return(list(details=benchmarks,resume=resume))
  
}