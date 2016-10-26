ipf2.b<-function(rtot=NULL,ctot=NULL,btot=NULL,b = NULL, m=NULL,
                 tol=1e-05,maxit=500,verbose=TRUE, ...){
  if(any(round(sum(rtot))!=round(sum(ctot))))
    stop("row and column totals are not equal for one or more sub-tables, ensure colSums(rtot)==rowSums(ctot)")
  
  # rtot = c(30,20,30,10,20,5,0,10,5,5,5,10)
  # ctot = c(45,10,10,5,5,10,50,5,10,0,0,0)
  # btot = matrix(c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0), nrow = 4, byrow = TRUE)
  # b = c(2,3,4,3)
  b_id = block.matrix(x = 1:(nrow(btot)*nrow(btot)), b = b, ...)
  n<-list(i=rtot,
          j=t(ctot),
          b=c(btot))
  
  #set up offset
  if(is.null(m)){
    m<-matrix(1, length(rtot), length(ctot), dimnames = dimnames(b_id))
  }
  
  mu<-m
  mu.marg<-n
  m.fact<-n
  it<-0; max.diff<-tol*2
  while(max.diff>tol & it<maxit){
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
    if(!is.null(btot)){
      mu.marg$b <- sapply(1:max(b_id), block.sum, m = mu, bid = b_id)
      m.fact$b <- n$b/mu.marg$b
      m.fact$b[is.nan(m.fact$b)]<-0
      m.fact$b[is.infinite(m.fact$b)]<-0
      
      mu <- mu*block.matrix(m.fact$b, b)
    }
    
    it<-it+1
    #max.diff<-max(abs(unlist(n)-unlist(mu.marg))) 
    #speeds up a lot if get rid of unlist (new to v1.6)
    max.diff<-max(abs(c(n$i-mu.marg$i, n$j-mu.marg$j, n$b-mu.marg$b)))
    if(verbose==TRUE)
      cat(c(it, max.diff), "\n")
  }
  return(list(mu=mu,it=it,tol=max.diff))
}
#rm(n,mu,mu.marg,m.fact,it,max.diff,b_id)
#rm(rtot,ctot,btot,b)
# y <- ipf2.b(rtot= c(30,20,30,10,20,5,0,10,5,5,5,10),
#             ctot = c(45,10,10,5,5,10,50,5,10,0,0,0),
#             btot = matrix(c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0), nrow = 4, byrow = TRUE),
#             b = c(2,3,4,3), dimnames = LETTERS[1:4])
# 
# addmargins(y$mu)

