#' Estimation of bilateral migrant flows from bilateral migrant stocks using stock differencing approaches
#' 
#' Estimates migrant transitions flows between two sequential migrant stock tables using differencing approaches commonly used by economists.
#' @param stock_start Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param stock_end Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param decrease How to treat decreases in bilateral stocks over the \emph{t} to \emph{t}+1 period (so as to avoid a negative bilateral flow estimates). See details for possible options. Default is \code{return}
#' @param include_native_born Logical value to indicate whether to include diagonal elements of \code{stock_start} and \code{stock_end}. Default of \code{FALSE} - not include.
#'
#' @return Estimates migrant transitions flows between two sequential migrant stock tables. 
#' 
#' When \code{decrease = "zero"} all decreases in migrant stocks over there period are set to zero, following the approach of Bertoli and Fernandez-Huertas Moraga (2015) 
#' 
#' When \code{decrease = "return"} all decreases in migrant stocks are assumed to correspond to return flows back to their place of birth, following the approach of Beine and Parsons (2015)
#' 
#' @references 
#' Beine, Michel, Simone Bertoli, and Jesús Fernández-Huertas Moraga. (2016). A Practitioners’ Guide to Gravity Models of International Migration. \emph{The World Economy} 39(4):496–512.
#' 
#' @author Guy J. Abel
#' @seealso \code{\link{ffs_demo}}, \code{\link{ffs_rates}}
#' @export
#'
#' @examples
#' s1 <- matrix(data = c(100, 10, 10, 0, 20, 55, 25, 10, 10, 40, 140, 65, 20, 25, 20, 200),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' s2 <- matrix(data = c(75, 25, 5, 15, 20, 45, 30, 15, 30, 40, 150, 35, 10, 50, 5, 200),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' r <- LETTERS[1:4]
#' dimnames(s1) <- dimnames(s2) <- list(pob = r, por = r)
#' s1; s2
#' 
#' ffs_diff(stock_start = s1, stock_end = s2, decrease = "zero")
#' ffs_diff(stock_start = s1, stock_end = s2, decrease = "return")
ffs_diff <- function(stock_start, stock_end, decrease = "return", include_native_born = FALSE){
  if(include_native_born == FALSE){
    diag(stock_start) <- 0
    diag(stock_end) <- 0
  }
  y0 <- stock_end - stock_start
  if(decrease == "zero"){
    y0[y0<0] <- 0
    y <- y0
  }
  if(decrease == "return"){
    y1 <- t(y0) * -1
    y1[y1<0] <- 0
    y0[y0<0] <- 0
    y <- y0 + y1
  }
  if(!is.null(dimnames(stock_start))){
    dimnames(y) <- list(orig = dimnames(stock_start)[[1]], dest = dimnames(stock_start)[[2]])
  }
  return(y)
}