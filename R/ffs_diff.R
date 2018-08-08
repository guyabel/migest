#' Estimation of Bilateral Migrant Flows from Bilateral Migrant Stocks Using Stock Differencing
#' 
#' Estimates migrant transitions flows between two sequential migrant stock tables using differencing approaches commonly used by economists.
#' @param m1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param decrease How to treat decreases in bilateral stocks over the \emph{t} to \emph{t}+1 period (so as to avoid a negative bilateral flow estimates). See details for possible options.
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
#' P1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' P2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' reg <- LETTERS[1:4]
#' dimnames(P1) <- dimnames(P2) <- list(pob = reg, por = reg)
#' P1; P2
#' 
#' ffs_diff(m1 = P1, m2 = P2, decrease = "zero")
#' ffs_diff(m1 = P1, m2 = P2, decrease = "return")
ffs_diff <- function(m1, m2, decrease = "return"){
  if(decrease == "zero"){
    y0 <- m2 - m1
    y0[y0<0] <- 0
    y <- y0
  }
  if(decrease == "return"){
    y0 <- m2 - m1
    y1 <- t(y0) * -1
    y1[y1<0] <- 0
    y0[y0<0] <- 0
    y <- y0 + y1
  }
  if(!is.null(dimnames(m1))){
    dimnames(y) <- list(orig = dimnames(m1)[[1]], dest = dimnames(m1)[[2]])
  }
  return(y)
}