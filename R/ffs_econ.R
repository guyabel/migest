#' Estimation of Bilateral Migrant Flows from Bilteral Migrant Stocks Using Stock Differencing
#'
#' @param P1 
#' @param P2 
#' @param decrease 
#'
#' @return
#' @export
#'
#' @examples
#' P1 <- matrix(c(NA, 100, 10, 0, 55, NA, 50, 5, 80, 40, NA, 40, 20, 25, 20, NA), 4, 4, 
# dimnames = list(pob = dn, por = dn), byrow = TRUE)
# P2 <- matrix(c(NA, 100, 60, 0, 80, NA, 75, 5, 90, 30, NA, 40, 40, 45, 0, NA), 4, 4, 
             # dimnames = list(pob = dn, por = dn), byrow = TRUE)

# display with row and col totals
# addmargins(P1)
# addmargins(P2)
ffs_econ(P1, P2, decrease = "zero")
ffs_econ(P1, P2, decrease = "return")

ffs_econ <- function(P1, P2, decrease = "return"){
  if(decrease == "zero"){
    y0 <- P2 - P1
    y0[y0<0] <- 0
    y <- y0
  }
  if(decrease == "return"){
    y0 <- P2 - P1
    y1 <- t(y0) * -1
    y1[y1<0] <- 0
    y0[y0<0] <- 0
    y <- y0 + y1
  }
  if(!is.null(dimnames(P1))){
    dimnames(y) <- list(orig = dimnames(P1)[[1]], dest = dimnames(P1)[[2]])
  }
  return(y)
}