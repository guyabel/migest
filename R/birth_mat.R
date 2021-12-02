#' Calculate births for each element of place of birth - place of residence stock matrix
#'
#' This function is predominantly intended to be used within the ffs routines in the migest package.
#' @param b_por Vector of numeric values for births in each place of residence
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param method Character string of either \code{"native"} or \code{"proportion"} to choose method to distribute births. The \code{"proportion"} method assumes the rate of non-migration increase in each place of birth sub-group (native born and all foreign born stocks) is the same. The \code{"native"} method ensures that all births (non-migration increases) in stocks belong to the native born population (they do not move straight after birth).
#' @param non_negative Adjust birth matrix calculation to ensure all deductions from \code{m2} will result in positive population counts. On rare occasions when working with international stock data the number of births can exceed the increase in the number of native born population.
#'
#' @return Matrix of place of birth by place of residence for new-born’s
birth_mat <- function(b_por = NULL, m2 = NULL, method = "native", non_negative = TRUE){
  # m2 = m2_b
  bb <- m2
  if(method == "native"){
    bb[,] <- diag(b_por)
    if(non_negative){
      # check that deduction of bb will not lead to negative populations
      xx <- diag(m2) - b_por < 0
      if (sum(xx) > 0)
        bb[, xx] <- ipf2(col_tot = b_por, m = m2)$mu[, xx]
      # bb[, xx] <- mipfp::Ipfp(seed = m2,
      #                         target.list = list(2),
      #                         target.data = b_por)$x.hat[, xx]
    }
  }
  if(method == "proportion"){
    bb <- ipf2(col_tot = b_por, m = m2)$mu
  }
  return(bb)
}
