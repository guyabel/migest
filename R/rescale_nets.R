#' Rescale Net Migration Total to a Global Zero Sum
#'
#' @param x Vector of net migration values
#' @param method Method used to adjust net migration values of \code{x} to obtain a global zero sum. By default \code{method="no-switches"}. Can also take values \code{method="switches"}. See details for explanation on each method.
#' @param w Weights used in rescaling method
#'
#' @return Rescales net migration for a number of regions in vector \code{x} to sum to zero.  When \code{method="no-switches"} rescaling of values are done for the positive and negative values separately, to ensure the final global sum is zero. When \code{method="switches"} the mean of the unscaled net migration is subtracted from each value. 
#' @export
#'
#' @examples
#' # net migration in regions countries (does not add up to zero)
#' x <- c(-200, -30, -5, 0, 10, 20, 60, 80)
#' x
#' sum(x)
#' # rescale 
#' y1 <- rescale_nets(x)
#' y1
#' sum(y1)
#' # rescale allowing switching of signs (small negative value becomes positive)
#' y2 <- rescale_nets(x, method = "switches")
#' y2
#' sum(y2)
rescale_nets <- function(x, method = "no-switches", w = rep(1, length(x))){
  if(method == "no-switches"){
    x1 <- x[x>0 & !is.na(x)]
    x2 <- x[x<0 & !is.na(x)]
    w1 <- w[x>0 & !is.na(x)]
    w2 <- w[x<0 & !is.na(x)]
    d <- (sum(x1*w1) + sum(x2*w2)) / 2
    s1 <- (sum(x1*w1) - d) / sum(x1*w1)
    s2 <- (sum(x2*w2) - d) / sum(x2*w2)
    y <- x
    y[x>0 & !is.na(x)] <- x1*s1
    y[x<0 & !is.na(x)] <- x2*s2
  }
  if(method == "switches"){
    y <- x*w - mean(x*w)
  }
  return(y)
}

