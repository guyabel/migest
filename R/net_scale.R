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
net_scale <- function(m, region = NULL, alpha){
  x <- m
  x[] <- 1
  if(length(dim(m)) == 2){
    x[region,] <- x[region,]*1/alpha
    x[,region] <- x[,region]*alpha
  }
  if(length(dim(m)) == 3){
    x[region,,] <- x[region,,]*1/alpha
    x[,region,] <- x[,region,]*alpha
  }
  return(x * m)
}
