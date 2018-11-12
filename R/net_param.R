#' Estimate Parameters for Net Migration Scaling.
#'
#' This function is predominantly intended to be used within the \code{\link{ipf_net}} routine.
#' @param m Matrix of origin-destination flows, where the first and second dimensions correspond to origin and destination respectively. 
#' @param region Integer value corresponding to the region that the net migration sum is desired. Will return sums for all regions by default.
#' @param net_tot Vector of net migration totals to constrain the sum of the imputed cell columns. Elements must sum to zero.
#'
#' @return Vector of two values corresponding to the roots for the quadratic equation.
#' @author Guy J. Abel
#' @export
net_param <- function(m, region, net_tot){
  if(length(dim(m)) == 2){
    emi <- m[region,]
    imm <- m[,region]
  }
  if(length(dim(m)) == 3){
    emi <- m[region,,]
    imm <- m[,region,]
  }
  emi_tot <- sum(emi)
  imm_tot <- sum(imm)
  p <- NULL
  if(emi_tot != 0 & imm_tot != 0)
    p <- quadratic_eqn(a = imm_tot, b = -net_tot, c = -emi_tot)
  return(p)
}

