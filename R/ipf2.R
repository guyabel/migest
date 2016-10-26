##
##ipf (rather than cm)...gives different estimates
##
ipf2<-function(rtot=NULL,ctot=NULL,m=matrix(1,length(rtot),length(ctot)),tol=1e-05,maxit=500,verbose=FALSE){
  if(!is.null(rtot) & !is.null(ctot))
    if(round(sum(rtot))!=round(sum(ctot))) 
      stop("row and column totals are not equal, ensure sum(rtot)==sum(ctot)")
  n<-list(i=rtot,
          j=ctot)
  mu<-m
  mu.marg<-n
  m.fact<-n
  if(verbose==TRUE)
    rd<-paste("%.",nchar(format(tol,scientific=FALSE))-2,"f",sep="")
  it<-0; max.diff<-tol*2
  while(it==0 | max.diff>tol & it<maxit ){
    if(!is.null(ctot)){
      mu.marg$j <- apply(mu,2,sum)
      m.fact$j <- n$j/mu.marg$j
      m.fact$j[is.nan(m.fact$j)]<-0
      m.fact$j[is.infinite(m.fact$j)]<-0
      mu <- sweep(mu, 2, m.fact$j, "*")
    }
    if(!is.null(rtot)){
      mu.marg$i <- apply(mu,1,sum)
      m.fact$i <- n$i/mu.marg$i
      m.fact$i[is.nan(m.fact$i)]<-0
      m.fact$i[is.infinite(m.fact$i)]<-0
      mu <- sweep(mu, 1, m.fact$i, "*")
    }
    it<-it+1
    #max.diff<-max(abs(unlist(n)-unlist(mu.marg)))
    #speeds up a lot if get rid of unlist (new to v1.7)
    max.diff<-max(abs(c(n$i-mu.marg$i, n$j-mu.marg$j)))
    
    if(verbose==TRUE)
      cat(sprintf(rd,unlist(m.fact)), fill = TRUE)
  }
  return(list(mu=mu,it=it,tol=max(abs(unlist(n)-unlist(mu.marg)))))
}
