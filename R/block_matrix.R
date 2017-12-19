#' Create a Block Matrix with Non-Uniform Block Sizes.
#' 
#' Creates a \code{matrix} with differing size blocks
#' @param x Vector of numbers to identify each block.
#' @param b Numeric value for the size of the blocks within the matrix ordered depending on \code{byrow}
#' @param byrow Logical value. If \code{FALSE} (the default) the blocks are filled by columns, otherwise the blocks in the matrix are filled by rows.
#' @param dimnames Character string of name attribute for the basis of the blcok matrix. If \code{NULL} a vector of the same length of \code{b} provides the basis of row and column names.#'
#' 
#' @return Returns a \code{matrix} with block sizes determined by the \code{b} argument. Each block is filled with the same value taken from \code{x}. 
#' @author Guy J. Abel
#' @seealso \code{\link{stripe_matrix}}, \code{\link{block_sum}}, \code{\link{ipf2_block}}
#' @export
#'
#' @examples
#' block_matrix(x = 1:16, b = c(2,3,4,2))
#' block_matrix(x = 1:25, b = c(2,3,4,2,1))
block_matrix <- function(x = NULL, b = NULL, byrow = FALSE, dimnames = NULL){
  n <- length(b)
  bb <- rep(1:n, times = b)
  dn <- NULL
  if(is.null(dimnames)){
    dn <- rep(LETTERS[1:n], times = b)
    dd <- unlist(sapply(b, seq, from = 1))
    dn <- paste0(dn, dd)
    dn <- list(dn, dn)
  }
  if(!is.null(dimnames)){
    dn <- dimnames
  }
  xx <- matrix(NA, nrow = sum(b), ncol = sum(b), dimnames = dn)
  k <- 1
  if(byrow == TRUE){
    for(i in 1:n){
      for(j in 1:n){
        xx[i==bb, j==bb] <- x[k]
        k <- k+1
      }
    }
  }
  if(byrow == FALSE){
    for(j in 1:n){
      for(i in 1:n){
        xx[i==bb, j==bb] <- x[k]
        k <- k+1
      }
    }
  }
  return(xx)
}
