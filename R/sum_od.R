#' Extract a classic origin-destination migration flow matrix.
#'
#' Extract a classic origin-destination migration flow matrix from a more detailed dis-aggregation of flows stored in an (\code{array}) object.
#' @param y Array of origin-destination matrices, where the first and second dimensions correspond to origin and destination respectively. Higher dimension(s) refer to additional migrant characteristic(s).
#'
#' @return
#' Matrix from summing over the first and second dimension. Set diagonals to zero.
#' 
#' Returns a \code{matrix} object of origin-destination flows
#' @author Guy J. Abel
#'
#' @examples
#' dn <- LETTERS[1:2]
#' y <- cm3(row_tot = c(18, 20) * 2, col_tot = c(16, 22) * 2, 
#'          m = array(c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2), 
#'                    dimnames = list(orig = dn, dest = dn, type = c("ILL", "HEALTHY"))))
#' round(addmargins(y$n))
#' round(addmargins(sum_od(y$n))) 
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
