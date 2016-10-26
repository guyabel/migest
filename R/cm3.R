##
##conditional maximization in 3 dimensions
##
cm3<-function(rtot=NULL,ctot=NULL,m,tol=1e-05,maxit=500,verbose=FALSE)
{
  if(round(sum(rtot))!=round(sum(ctot)))
    stop("row and column totals are not equal, ensure sum(rtot)==sum(ctot)")
  i<-dim(m)[1];  j<-dim(m)[2]
  alpha <- rep(1,i)
  beta <- rep(1,j)
  if(verbose==TRUE){
    rd<-paste("%.",nchar(format(tol,scientific=FALSE))-2,"f",sep="")
    cat(sprintf(rd,c(alpha,beta)), fill = TRUE)
  }
  alpha.old <- alpha+1; beta.old <- beta+1
  it<-1;  max.diff<-tol*2
  while(max.diff>tol & it<maxit){
    beta.old <- beta
    for(j in 1:j) {
      beta[j] <- ctot[j]/colSums(alpha*apply(m,c(1,2),sum))[j]
    }
    alpha.old <- alpha
    for(i in 1:i) {
      alpha[i] <- rtot[i]/colSums(beta*apply(m,c(2,1),sum))[i]
    }
    it<-it+1
    max.diff<-max(abs(alpha-alpha.old), abs(beta-beta.old))
    if(verbose==TRUE)
      cat(sprintf(rd,c(alpha,beta)), fill = TRUE)
  }
  return(list(N=c(alpha%*%t(beta))*m,
              theta=c(mu=1,alpha=alpha,beta=beta)))
}
