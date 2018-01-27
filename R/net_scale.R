#' Scale Migration Flows in Origin-Destination
#'
#' This function is predominantly intended to be used within the \code{\link{ipf_net}} routine.
#' 
#' @param m Matrix of origin-destination flows, where the first and second dimensions correspond to origin and destination respectively. 
#' @param region Integer corresponding to row (column) in a square matrix for the region where scaling is to be applied
#' @param alpha Numeric value of the scaling factor
#'
#' @return Matrix scaled in region(s) by value of alpha, where \code{alpha} applied to destination flows and inverse of \code{alpha} applied to origin flows
#' @author Guy J. Abel
#' @export
#'
#' @examples
#' dn <- LETTERS[1:4]
#' P1 <- matrix(c(0, 100, 10, 0, 55, 0, 50, 5, 80, 40, 0, 40, 20, 25, 20, 0), 4, 4, 
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' P2 <- matrix(c(0, 100, 60, 0, 80, 0, 75, 5, 90, 30, 0, 40, 40, 45, 0, 0), 4, 4, 
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' m <- ffs_econ(P1, P2, decrease = "return")
#' p <- net_param(region = 1, m = m, ntot = 30)
#' net_scale(m, region = 1, alpha = p[p>0])
net_scale <- function(m, region = NULL, alpha){
  x <- m
  x[,] <- 1
  x[region,] <- x[region,]*1/alpha
  x[,region] <- x[,region]*alpha
  return(x * m)
}
