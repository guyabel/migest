#' Extract a classic origin-destination migration flow matrix.
#'
#' Extract a classic origin-destination migration flow matrix from a more detailed dis-aggregation of flows stored in an (\code{array}) object.
#' @param y Array of origin-destination matrices, where the first and second dimensions correspond to origin and destination respectively. Higher dimension(s) refer to additional migrant characteristic(s).
#'
#' @return
#' Matrix from summing over the first and second dimension. Set diagonals to zero.
#' 
#' Returns a \code{matrix} object of origin-destination flows
#' @export
#'
#' @examples
sum_od <- function(y){
  R <- dim(y)[3]
  dg <- diag(apply(y,c(1,2),sum))
  od <- apply(y,c(1,2),sum)
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
