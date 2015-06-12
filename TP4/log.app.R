log.app = function(Xapp, zapp, intr, epsi) {
  
  ############ INIT
  if(intr==0) {Xapp = as.matrix(Xapp)} else {Xapp = as.matrix(cbind(Xapp, rep(1,nrow(Xapp))))}
  dim = ncol(Xapp)
  w = matrix(0, nrow=dim)
  w_prec = matrix(1000, nrow=dim)
  t = as.integer(zapp == 1)
  q = 0
  print(Xapp)
  print('####################################')
  
  while( sqrt(sum((w - w_prec)^2)) > epsi ) { 
    q = q + 1
    ############ pq
    wx = Xapp %*% w
    pq = exp(wx) / (1+exp(wx))
    print(w)
    print(pq)
    ############ gradient_Lw
    gradient_Lw = t(Xapp) %*% (t-pq)
    
    ############ Wq
    Wq = diag(as.vector(pq*(rep(1,nrow(Xapp))-pq)))
    
    ############ Hq
    print(Wq)
    Hq = -t(Xapp) %*% Wq %*% Xapp
    
    ############ iteration
    w_prec = w
    print(solve(Hq))    
    w = w - solve(Hq) %*% gradient_Lw    
    logL<-sum(t*wx-log(rep(1,nrow(Xapp))+exp(wx)))
    print(q)
    print("----------------------------------------------")    
  }
  res <- NULL
  res$beta <- w
  res$niter <- q
  res$logL <- logL
  res
}