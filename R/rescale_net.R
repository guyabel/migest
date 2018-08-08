#' Rescale Net Migration Total to a Global Zero Sum
#'
#' Modify a set of net migration (or any numbers) so that they sum to zero.
#' @param x Vector of net migration values
#' @param method Method used to adjust net migration values of \code{x} to obtain a global zero sum. By default \code{method="no-switches"}. Can also take values \code{method="switches"}. See details for explanation on each method.
#' @param w Weights used in rescaling method
#' @param integer_result Logical operator to indicate if output shoud be integers, default is \code{TRUE}.
#'
#' @return Rescales net migration for a number of regions in vector \code{x} to sum to zero.  When \code{method="no-switches"} rescaling of values are done for the positive and negative values separately, to ensure the final global sum is zero. When \code{method="switches"} the mean of the unscaled net migration is subtracted from each value. 
#' @references 
#' Abel, G. J. (2018). Non-zero trajectories for long-run net migration assumptions in global population projection models. \emph{Demographic Research} 38, (54) 1635–1662
#' @author Guy J. Abel
#' @export
#'
#' @examples
#' # net migration in regions countries (does not add up to zero)
#' x <- c(-200, -30, -5, 0, 10, 20, 60, 80)
#' x
#' sum(x)
#' # rescale 
#' y1 <- rescale_net(x)
#' y1
#' sum(y1)
#' # rescale without integer restriction
#' y2 <- rescale_net(x, integer_result = FALSE)
#' y2
#' sum(y2)
#' # rescale allowing switching of signs (small negative value becomes positive)
#' y3 <- rescale_net(x, method = "switches")
#' y3
#' sum(y3)
rescale_net <- function(x, method = "no-switches", w = rep(1, length(x)), integer_result = TRUE){
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
  if(method == "switches")
    y <- x*w - mean(x*w)
  if(integer_result){
    y <- round(y)
    d <- sum(y)
    if(d != 0){
      # edit the most extreme value
      m <- which(abs(y) == max(abs(y)))
      # choose an index randomly if more than one of the same extreme value
      m_this <- sample(x = m, size = 1)
      y[m_this] <- y[m_this] - d
    }
  }
  return(y)
}
