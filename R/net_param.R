#' Estimate Parameters for Net Migration Scaling.
#'
#' This function is predominantly intended to be used within the \code{\link{ipf_net}} routine.
#' @param m Matrix of origin-destination flows, where the first and second dimensions correspond to origin and destination respectively. 
#' @param region Integer value corresponding to the region that the net migration sum is desired. Will return sums for all regions by default.
#' @param ntot Vector of net migraiton totals to constrain the sum of the imputed cell columns. Elements must sum to zero.
#'
#' @return Vector of two values corresonding to the roots fo the quatratic equation.
#' @author Guy J. Abel
#' @export
net_param <- function(m, region, ntot){
  emi <- m[region,]
  imm <- m[,region]
  itot <- sum(imm)
  etot <- sum(emi)
  quadratic_eqn(a = itot, b = -ntot, c = -etot)
}