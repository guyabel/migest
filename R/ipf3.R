##
##ipf3
##
ipf3<-function(rtot=NULL,ctot=NULL,m=NULL,tol=1e-05,maxit=500,verbose=TRUE){
  if(any(round(colSums(rtot))!=round(rowSums(ctot))))
    stop("row and column totals are not equal for one or more sub-tables, ensure colSums(rtot)==rowSums(ctot)")
  n<-list(ik=rtot,
          jk=t(ctot))
  R<-dim(rtot)[1]
  
  #set up offset
  if(is.null(m)){
    m<-array(1,c(dim(rtot),dim(rtot)[1]))
    dimnames(m)<-list(orig=dimnames(rtot)[[1]],dest=dimnames(ctot)[[1]],pob=dimnames(rtot)[[2]])
  }
  
  mu<-m
  mu.marg<-n
  m.fact<-n
  it<-0; max.diff<-tol*2
  while(max.diff>tol & it<maxit){
    mu.marg$ik <- apply(mu,c(1,3),sum)
    m.fact$ik <- n$ik/mu.marg$ik
    m.fact$ik[is.nan(m.fact$ik)]<-0
    m.fact$ik[is.infinite(m.fact$ik)]<-0
    mu <- sweep(mu, c(1,3), m.fact$ik, "*")
    
    mu.marg$jk <- apply(mu, c(2,3), sum)
    m.fact$jk <- n$jk/mu.marg$jk
    m.fact$jk[is.nan(m.fact$jk)]<-0
    m.fact$jk[is.infinite(m.fact$jk)]<-0
    mu <- sweep(mu, c(2,3), m.fact$jk, "*")
    
    it<-it+1
    #max.diff<-max(abs(unlist(n)-unlist(mu.marg)))
    #speeds up a lot if get rid of unlist (new to v1.7)
    max.diff<-max(abs(c(n$ik-mu.marg$ik, n$jk-mu.marg$jk)))
    
    if(verbose==TRUE)
      cat(c(it, max.diff), "\n")
  }
  return(list(mu=mu,it=it,tol=max.diff))
}
