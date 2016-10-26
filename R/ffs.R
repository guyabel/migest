##
##flows from stock
##
#P1=P1;P2=P2;d=d;b=b.adj;m=m
#P1 = s0; P2 = s1; d = df7$d; b = df7$b.adj; m = dm; method = "stocks"
#method="stocks"; d.mat=NULL;b.mat=NULL;b.deduct="native.gt0"
#m=NULL;
ffs<-function(P1,P2,d,b,m=NULL,method="stocks",b.mat=NULL,d.mat=NULL,b.deduct="native.gt0",...){
  if(!(method %in% c("outside","stocks","deaths")) | length(method)!=1)
    stop("method must be one of outside, stocks or deaths")
  if(!(b.deduct %in% c("native.only","native.gt0")) | length(b.deduct)!=1)
    stop("method must be one of outside, stocks or deaths")
  R<-nrow(P1)
  #set up offset
  if(is.null(m)){
    m<-array(1,c(dim(P1),dim(P1)[1]))
  }
  if(is.null(dimnames(m))){
    dimnames(m)<-list(orig=dimnames(P1)[[1]],dest=dimnames(P2)[[1]],pob=dimnames(P1)[[2]])
  }
  if(method=="stocks"){
    if(round(sum(P2-P1),1)!=round(sum(b-d),1)){
      message("sum(P2-P1): ", sum(P2-P1))
      message("sum(b-d):   ", sum(b-d))
      stop("difference in stock tables must be equal to sum of all births - sum of all deaths")
    }
  } 
  
  #set up migration matrix with births/deaths and others rows and columns
  y<-m
  y<-array(0,dim(m)+c(2,2,0))
  dimnames(y)<-list(o=c(dimnames(m)[[1]],"b","O"),d=c(dimnames(m)[[1]],"d","O"),b=dimnames(m)[[3]])
  y[,,]<-0
  
  #step 1-2a take off deaths
  message("Adjust for Deaths...")
  if(is.null(d.mat))
    d.mat<-ipf2(ctot=d,m=P1)$mu
  if(method=="deaths"){
    d.mat<-ipf2(ctot=d,rtot=rowSums(P1) + b - rowSums(P2),m=P1)$mu  
  }
  y[1:R,R+1,]<-t(d.mat)
  P1.adj<-P1-d.mat
  
  #step 1-2b take off births
  message("Adjust for Births...")
  if(is.null(b.mat))
    b.mat<-diag(b)
  y[R+1,1:R,]<-b.mat
  P2.adj<-P2-b.mat
  #adjust for negative nb sums after birth deduction...spread births to all population where needed (new to v1.6)
  if(b.deduct=="native.gt0"){
    ii<-diag(P2.adj<0)
    if(sum(ii)>0){
      b.mat[,ii]<-ipf2(ctot=b,m=P2)$mu[,ii]
      P2.adj<-P2-b.mat
    }
  }
  
  #step 3-4a take off moves in from external or adjust P1.adj rows
  message("Remaining Adjustments (P1)...")
  dif<-rowSums(P1.adj) - rowSums(P2.adj)
  if(method=="outside" | method=="deaths"){
    #following is in versions <1.3. is wrong. those leaving contolled for in P1, like those who die
    #this (in the #) is labelled in.mat but should be out.mat (where the dif>0). should have offset P1.adj, where they leave from, not P2.adj
    #in.mat<-t(ipf2(ctot=pmax(dif,0),m=t(P2.adj))$mu)
    #P1.adj<-P1.adj-in.mat
    #y[R+2,1:R,]<-t(in.mat)
    out.mat<-t(ipf2(ctot=pmax(dif,0),m=t(P1.adj))$mu)
    P1.adj<-P1.adj-out.mat
    y[1:R,R+2,]<-t(out.mat)
  }
  if(method=="stocks"){
    #was not reaching convergence of near zero difference in maxdiff in <v1.5, i.e. either row or col totals in estimates were not matching arguments.
    P1.adj<-ipf2(rtot=rowSums(P1.adj)-dif/2,ctot=colSums(P1.adj), m=P1.adj, maxit=100000, tol=0.1)$mu
  }
  
  #step 3-4b take off moves out from external or adjust P2.adj rows
  message("Remaining Adjustments (P2)...")
  if(method=="outside" | method=="deaths"){
    #following is in versions <1.3. is wrong. those arriving contolled for in P2, like those who are born
    #this (in the #) is labelled out.mat but should be in.mat (where the dif<0). should have offset P2.adj, where they arrive too, not P1.adj
    #out.mat<-t(ipf2(ctot=pmax(-dif,0),m=t(P1.adj))$mu)
    #P2.adj<-P2.adj-out.mat
    #y[1:R,R+2,]<-t(out.mat)
    in.mat<-t(ipf2(ctot=pmax(-dif,0),m=t(P2.adj))$mu)
    P2.adj<-P2.adj-in.mat
    y[R+2,1:R,]<-t(in.mat)
  }
  if(method=="stocks"){
    #was not reaching convergence of near zero difference in maxdiff in <v1.5, i.e. either row or col totals in estimates were not matching arguments.
    P2.adj<-ipf2(rtot=rowSums(P2.adj)+dif/2,ctot=colSums(P2.adj),m=P2.adj, maxit=100000, tol=0.1)$mu
  }
  
  #step 5 calculate
  message("Calculate Flows...")
  #ipf<-ipf3.qi(rtot=t(P1.adj),ctot=P2.adj,m=m)
  ipf<-ipf3.qi(rtot=t(P1.adj),ctot=P2.adj,m=m,...)
  y[1:R,1:R,]<-ipf$mu
  return(c(ipf,list(y=y)))
}
