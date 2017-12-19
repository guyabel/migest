#' Title
#'
#' @param m 
#' @param region 
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
net_scale <- function(m, region = NULL, alpha){
  x <- m
  x[,] <- 1
  x[region,] <- x[region,]*1/alpha
  x[,region] <- x[,region]*alpha
  x * m
}
p <- net.param(region = 1, m = m, ntot = 30)
m
mm <- net.scale(m, region = 1, alpha = p[p>0])