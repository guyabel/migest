wd<-"C:/Users/gabel/Documents/R/mypackages/migest0/"
setwd(wd)
rm(wd)

italy<-read.csv("italy.csv")
italy<-array(italy$flow,c(5,5,20,7))
dimnames(italy)<-list(orig=c("North-West","North-East","Center","South","Islands"),
                      dest=c("North-West","North-East","Center","South","Islands"),
                      age=c(paste0(seq(0,90,5),"--",seq(4,94,5)),"95+"),
                      year=as.character(seq(1970,2000,5)))

italy["South","North-East",,]

addmargins(italy[,,,])
m=apply(italy[,,,],c(1,2,4),sum)
addmargins(m)
m["South","North-East",]
#previous function for raymer coding
trc<-function(m=data){
  dn<-names(dimnames(m))
  d<-length(dn)
  if(d<2)
    stop("m must be matrrix or array with 2 or more dimensions")
  tau<-sum(m)
  tau1<-vector("list",d)
  for(i in 1:d){
    tau1[[i]]<-apply(m,i,sum)/tau
  }
  names(tau1)<-dn
  tau2<-vector("list",choose(d,2))
  n2<-length(tau2)
  d2<-combn(1:d, 2)
  for(i in 1:n2){
    tau2[[i]]<-apply(m,d2[,i],sum)/(tau*tau1[[d2[1,i]]]%*%t(tau1[[d2[2,i]]]))
    names(tau2)[[i]]<-paste(dn[d2[,i]],collapse=".")
  }
  tau2
  list(total=tau,main=tau1,inter=tau2)
}
m=apply(italy[,,,1],c(1,2,3),sum)
trc(m)
m=apply(italy[,,,1],c(1,2),sum)
trc(m)
temp$inter

multi.comp<-function(m = data)
{
  k <- dim(m)[1]
  w <- prod(m)^(1/(k^2))
  w.r <- NULL
  w.c <- NULL
  for(i in 1:k) {
    w.r[i] <- (1/w) * (prod(m[i,  ]))^(1/k)
  }
  for(i in 1:k) {
    w.c[i] <- (1/w) * (prod(m[, i]))^(1/k)
  }
  z <- w * w.r %*% t(w.c)
  w.rc <- m/z
  comp <- rbind(cbind(w.rc, c(w.r)), c(w.c, w))
  round(comp, 3)
}

multi.comp.log<-function(m = data)
{
  k <- dim(m)[1]
  u <- sum(log(m))/(k^2)
  u.r <- colSums(log(m))/k - u
  u.c <- rowSums(log(m))/k - u
  con <- u + u.r + u.c
  u.rc <- log(m) - con
  comp <- rbind(cbind(exp(u.rc), c(exp(u.r))), c(exp(u.c), exp(u)))
  round(comp, 3)
}

Darroch.Ratcliff<-function(m.r, m.c, f)
{
  x <- (f * (m.c/colSums(f)) %*% t(m.r/rowSums(f)))/(sum(m.c)/(sum(
    f)))
  comp <- rbind(cbind(x, colSums(x)), c(rowSums(x), sum(x)))
  comp
}
Darroch.Ratcliff(m.r=c(18,20),m.c=c(16,22),f=matrix(c(5,2,1,7),ncol=2))

