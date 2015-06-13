
benchmark_Pima = function() {
  
  benchmarks = array(0,dim=c(100,3,3))
  dimnames(benchmarks)[[3]] = c("adq.app","adl.app","nba.app")
  dimnames(benchmarks)[[2]] = c("Pima","Pima", "Pima")
  
  ######################## benchmarks
    X = Pima[,1:7]
    z = Pima[,8]
    
    for(j in 1:100) {
      separation = separ1(X,z)
      Xapp = separation$Xapp
      Xtst = separation$Xtst
      zapp = separation$zapp
      ztst = separation$ztst
      
      for(f in c("adq.app","adl.app","nba.app")) {
        fct = get(f)
        parametres = fct(Xapp,zapp)
        classement = ad.val(parametres,Xtst)$classement
        benchmarks[j,i,f] = sum(classement==ztst) / length(ztst)
      }
      
    }
  
  ######################## means
  resume = list()
  for(f in c("adq.app","adl.app","nba.app")) {
    resume[[f]]$mean_by_file = apply(benchmarks[1:100,,f],2,mean)
    resume[[f]]$mean = sum(resume[[f]]$mean_by_file) / 3
  }
  
  
  
  return(list(details=benchmarks,resume=resume))
  
}