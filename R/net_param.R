#' Estimate Parameters for Net Migration Scaling.
#'
#' This function is predominantly intended to be used within the \code{\link{ipf_net}} routine.
#' @param m Matrix of origin-destination flows, where the first and second dimensions correspond to origin and destination respectively. 
#' @param region Integer value corresponding to the region that the net migration sum is desired. Will return sums for all regions by default.
#' @param ntot Vector of net migration totals to constrain the sum of the imputed cell columns. Elements must sum to zero.
#'
#' @return Vector of two values corresponding to the roots for the quadratic equation.
#' @author Guy J. Abel
#' @export
net_param <- function(m, region, ntot){
  if(length(dim(m)) == 2){
    emi <- m[region,]
    imm <- m[,region]
  }
  if(length(dim(m)) == 3){
    emi <- m[region,,]
    imm <- m[,region,]
  }
  etot <- sum(emi)
  itot <- sum(imm)
  p <- NULL
  if(etot != 0 & itot != 0)
    p <- quadratic_eqn(a = itot, b = -ntot, c = -etot)
  return(p)
}

