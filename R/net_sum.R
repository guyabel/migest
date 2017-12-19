#' Title
#'
#' @param m 
#' @param region 
#' @param orig 
#'
#' @return
#' @export
#'
#' @examples
#' m <- ffs_econ(P1, P2, decrease = "return")
net.sum(m = m)
net_sum <- function(m, region = 1:dim(m)[1], orig = "row"){
  rtot <- apply(X = m, MARGIN = 1, FUN = sum, na.rm = TRUE)
  ctot <- apply(X = m, MARGIN = 2, FUN = sum, na.rm = TRUE)
  if(orig == "row")
    net <- ctot - rtot
  if(orig == "col")
    net <- rtot - ctot
  if(is.null(region))
    region <- 1:min(length(rtot),length(ctot))
  n <- net[region]
  return(n)
}