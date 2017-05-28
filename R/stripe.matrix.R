#' Create a Stripped Matrix with Non-Uniform Block Sizes.
#'
#' @param x Vector of numbers to identify each stripe.
#' @param s Vector of values for the size of the stripes, order depending on \code{byrow}
#' @param byrow Logical value. If \code{FALSE} (the default) the stripes are filled by columns, otherwise the stripes in the matrix are filled by rows.
#' @param dimnames Character string of name attribute for the basis of the stripped matrix. If \code{NULL} a vector of the same length of \code{s} provides the basis of row and column names.
#'
#' @return Returns a \code{matrix} with stripe sizes determined by the \code{s} argument. Each stripe is filled with the same value taken from \code{x}. 
#' @author Guy J. Abel
#' @seealso \code{\link{block.matrix}}, \code{\link{block.sum}}, \code{\link{ipf2.s}}
#' @export
#'
#' @examples
#' stripe.matrix(x = 1:44, s = c(2,3,4,2), dimnames = LETTERS[1:4], byrow = TRUE)
stripe.matrix <- function(x = NULL, s = NULL, byrow = FALSE, dimnames = NULL){
  n <- length(s)
  ss <- rep(1:n, times = s)
  dn <- NULL
  if(is.null(dimnames)){
    dn <- LETTERS[1:n]
    dn <- rep(LETTERS[1:n], times = s)
    dd <- unlist(sapply(s, seq, from = 1))
    dn <- paste0(dn, dd)
    dn <- list(dn, dn)
  }
  if(!is.null(dimnames)){
    dn <- dimnames
  }
  xx <- matrix(NA, nrow = sum(s), ncol = sum(s), dimnames = dn)
  k <- 1
  if(byrow == TRUE){
    for(i in 1:sum(s)){
      for(j in 1:n){
        xx[i, j==ss] <- x[k]
        k <- k+1
      }
    }
  }
  if(byrow == FALSE){
    for(j in 1:sum(s)){
      for(i in 1:n){
        xx[i==ss, j] <- x[k]
        k <- k+1
      }
    }
  }
  return(xx)
}
