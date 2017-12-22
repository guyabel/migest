#' Title
#'
#' @param m 
#' @param ntot 
#'
#' @return
#' @export
#'
#' @examples
net_param <- function(m, ntot){
  emi <- m[region,]
  imm <- m[,region]
  itot <- sum(imm)
  etot <- sum(emi)
  quadratic.eqn(a = itot, b = -ntot, c = -etot)
}