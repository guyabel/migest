##
##flow matrix
##
fm<-function(y){
  R<-dim(y)[3]
  dg<-diag(apply(y,c(1,2),sum))
  od<-apply(y,c(1,2),sum)
  if(R==dim(y)[1])  fl<-od-diag(dg,R,R)
  if(R!=dim(y)[1]){
    y.adj<-y
    for(i in 1:R){
      y.adj[i,,i]<-y[i,,i]+y[R+1,,i]
    }
    od<-apply(y.adj,c(1,2),sum)
    fl<-od[1:R,1:R]-diag(dg[1:R],R,R)
  }
  diag(fl)<-0
  fl
}
