#' Estimation of Bilateral Migrant Flows from Bilteral Migrant Stocks Using Stock Differencing
#' 
#' Estimates migrant transitions flows between two sequential migrant stock tables using differencing approaches communly used by economists.
#' @param P1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param P2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param decrease How to treat decreases in bilateral stocks over the \emph{t} to \emph{t}+1 period (so as to avoid a negative bilateral flow estiamtes). See details for possible options.
#'
#' @return Estimates migrant transitions flows between two sequential migrant stock tables. 
#' 
#' When \code{decrease = "zero"} all decreases in migrant stocks over there period are set to zero, following the approach of Bertoli and Fernandez-Huertas Moraga (2015) 
#' 
#' When \code{decrease = "return"} all decreases in migrant stocks are assumed to correspond to return flows back to their place of birth, following the approach of Beine and Parsons (2015)
#' @author Guy J. Abel
#' @seealso \code{\link{ffs}}
#' @export
#'
#' @examples
#' dn <- LETTERS[1:4]
#' P1 <- matrix(c(0, 100, 10, 0, 55, 0, 50, 5, 80, 40, 0, 40, 20, 25, 20, 0), 4, 4, 
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' P2 <- matrix(c(0, 100, 60, 0, 80, 0, 75, 5, 90, 30, 0, 40, 40, 45, 0, 0), 4, 4, 
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' P1; P2
#' 
#' ffs_diff(P1, P2, decrease = "zero")
#' ffs_diff(P1, P2, decrease = "return")
ffs_diff <- function(P1, P2, decrease = "return"){
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
