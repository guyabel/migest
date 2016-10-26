##
##ipf3.qi
##
# P1.adj=P1;P2.adj=P2
#rtot=t(P1.adj);ctot=P2.adj;dtot=NULL;verbose=TRUE;tol=1e-05;maxit=500;speed=TRUE;m=NULL
ipf3.qi<-function(rtot=NULL,ctot=NULL,dtot=NULL,m=NULL,speed=TRUE,tol=1e-05,maxit=500,verbose=TRUE){
  if(any(round(colSums(rtot))!=round(rowSums(ctot))))
    stop("row and column totals are not equal for one or more sub-tables, ensure colSums(rtot)==rowSums(ctot)")
  
  n<-list(ik=rtot,
          jk=t(ctot),
          ijk=dtot)
  R<-dim(rtot)[1]
  
  #set up diagonals
  df1<-expand.grid(a = 1:R, b = 1:R)
  if(is.null(dtot)){
    dtot<-array(1,c(R,R,R))
    dtot<-with(df1, replace(dtot, cbind(a, a, b),  apply(cbind(c(n$ik),c(n$jk)),1,min) ))
    n$ijk<-dtot
  }
  
  #set up offset
  if(is.null(m)){
    m<-array(1,c(dim(rtot),dim(rtot)[1]))
  }
  if(is.null(dimnames(m))){
    dimnames(m)<-list(orig=dimnames(rtot)[[1]],dest=dimnames(ctot)[[1]],pob=dimnames(rtot)[[2]])
  }
  
  #alter ss (to speed up)
  if(speed==TRUE){
    n$ik<-n$ik-(apply(n$ijk,c(1,3),sum)-(R-1))
    n$jk<-n$jk-(apply(n$ijk,c(2,3),sum)-(R-1))
    n$ijk<-with(df1, replace(n$ijk, cbind(a, a, b),  0 ))
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
    
    mu.marg$ijk <- with(df1, replace(n$ijk, cbind(a, a, b),  c(apply(mu,3,diag)) ))
    m.fact$ijk <- n$ijk/mu.marg$ijk
    m.fact$ijk[is.nan(m.fact$ijk)]<-0
    m.fact$ijk[is.infinite(m.fact$ijk)]<-0
    mu <- mu*m.fact$ijk
    
    it<-it+1
    #max.diff<-max(abs(unlist(n)-unlist(mu.marg))) 
    #speeds up a lot if get rid of unlist (new to v1.6)
    max.diff<-max(abs(c(n$ik-mu.marg$ik, n$jk-mu.marg$jk, n$ijk-mu.marg$ijk)))
    if(verbose==TRUE)
      cat(c(it, max.diff), "\n")
  }
  if(speed==TRUE){
    mu<-with(df1, replace(mu, cbind(a, a, b), c(sapply(1:R, function(i) diag(dtot[,,i]))) ))
  }
  return(list(mu=mu,it=it,tol=max.diff))
}
#rm(n,mu,mu.marg,m.fact)
#ipf3.qi(rtot=t(P1.adj),ctot=P2.adj,m=m)#
