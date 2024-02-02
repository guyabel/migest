#' Rescale integer vector to a set sum
#'
#' For when you want to rescale a set of numbers to sum to a given value and do not want all rescaled values to be integers.
#'
#' @param x Vector of numeric values
#' @param tot Numeric integer value to rescale sum to.
#'
#' @return Vector or integer values that sum to to \code{tot}
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3_qi}}, \code{\link{ffs_diff}}
#' @export
#'
#' @examples
#' x <- rnorm(n = 10, mean = 5, sd = 20)
#' y <- rescale_integer_sum(x, tot = 10)
#' y
#' sum(y)
#' 
#' for(i in 1:10){
#'   y <- rescale_integer_sum(x = rpois(n = 10, lambda = 10), tot = 1000)
#'   print(sum(y))
#' }
#x <- rpois(n = 10, lambda = 10)
#tot <- 200
rescale_integer_sum <- function(x, tot){
  xx <- x * (tot/sum(x))
  xx <- round(xx)
  d <- tot - sum(xx)
  if(d != 0){
    # edit the most extreme value
    m <- which(x == max(abs(x)))
    # choose an index randomly if more than one of the same extreme value
    m_this <- sample(x = m, size = 1)
    xx[m_this] <- xx[m_this] + d
  }
  return(xx)
}
