#' Calculate Births for Each Element of Place of Birth - Place of Residence Stock Matrix 
#'
#' This function is predominantly intended to be used within the ffs routines in the migest package. 
#' @param b_por Vector of numberic values for births in each place of residence
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param non_negative Adjust birth matrix calculation to ensure all deductions from \code{m2} will result in positive population counts. On rare occasions when working with international stock data the number of births can exceed the increase in the number of native born population.
#'
#' @return Matrix of place of birth by place of residence for new-born’s
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3_qi}}, \code{\link{ffs_diff}}
#' @export

birth_mat <- function(b_por = NULL, m2 = NULL, non_negative = TRUE){
  # m2 = m2_b
  bb <- m2
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
  return(bb)
}
