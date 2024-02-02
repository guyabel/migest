#' Handle negative native born populations 
#'
#' This function is predominantly intended to be used within the ffs routines in the migest package. Adjustment to ensure positive population counts in all elements of stock matrix. On rare occasions when working with international stock data the foreign born population can exceed the total population due to conflicting data sources.
#' @param m Matrix of migrant stock totals. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' A matrix which scales the elements in columns (places of residence) with a negative population to match the overall population (column total). Negative values will be replaced with zero. Positive values will be scaled down to ensure the column total matches the original \code{m}.
#' @author Guy J. Abel
#' @seealso \code{\link{ffs_demo}}
#' 
#' @examples
#' \donttest{
#' ## cant have examples if function not in namespace - i.e. without export 
#' ## so comment all out for own use
#' # dn <- LETTERS[1:4]
#' # P <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#' #             nrow = 4, ncol = 4, dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' # # display with row and col totals
#' # addmargins(A = P)
#' # 
#' # # no change
#' # y <- nb_non_zero(m = P)
#' # addmargins(A = y)
#' # 
#' # # adjust a native born population to negative
#' # P[4, 4] <- -20
#' # # display with row and col totals
#' # addmargins(A = P)
#' # 
#' # y <- nb_non_zero(m = P)
#' # addmargins(A = y)
#' }
nb_non_zero <- function(m, verbose = FALSE){
  i <- diag(m) < 0
  if (any(i)){
    message(
      paste0("There are more foreign born than total population in ", 
             names(i)[i], ". \n
             ... Reduced foreign born populations to allow for zero (rather than negative) native born for this region. Might want to check the input data.\n"))
    m[, i] <- mipfp::Ipfp(
      seed = pmax(m, 0), 
      target.list = list(2),
      target.data = list(colSums(m)),
      tol = 1e-03, iter = 1e05, tol.margins = 1e-03
    )$x.hat[, i]
  }
  return(m)
}

  