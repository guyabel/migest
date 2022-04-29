#' Rescale native born populations to match global differences in births and deaths over period
#'
#' This function is predominantly intended to be used within the ffs routines in the migest package. Adjustment to ensure that global differences in stocks match the global demographic changes from births and deaths.
#' @param m1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param d Vector of the number of deaths between time \emph{t} and \emph{t}+1 in each region.
#' @param b Vector of the number of births between time \emph{t} and \emph{t}+1 in each region.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' List with adjusted \code{m1} and \code{m2}.
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3_qi}}, \code{\link{ffs_diff}}
#' 
#' @examples
#' \donttest{
#' dn <- LETTERS[1:4]
#' P1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#'              nrow = 4, ncol = 4, dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' P2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
#'              nrow = 4, ncol = 4, dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' # display with row and col totals
#' addmargins(A = P1)
#' addmargins(A = P2)
#' 
#' # births and deaths
#' b <- rep(x = 10, 4)
#' d <- rep(x = 5, 4)
#' # no change in stocks, but 20 more births than deaths...
#' sum(P2 - P1) + sum(b - d)
#' # rescale
#' # y <- rescale_nb(m1 = P1, m2 = P2, b = b, d = d)
#' # y
#' # sum(y$m1_adj - y$m2_adj) + sum(b - d)
#' 
#' # check for when extra is positive and odd
#' d[1] <- 31
#' d
#' sum(P2 - P1) - sum(b - d)
#' # rescale
#' # y <- rescale_nb(m1 = P1, m2 = P2, b = b, d = d)
#' # sum(y$m1_adj - y$m2_adj) - sum(b - d)
#' }
rescale_nb <- function(m1, m2, b, d, verbose = FALSE){
  # m1 = m1_c; m2 = m2_c; b = 0; d = 0
  # m1 = m1_a; m2 = m2_a; b = 0; d = 0
  pop_grow <- sum(m2 - m1)
  nat_grow <- sum(b - d)
  dd <- nat_grow - pop_grow
  tot1 <- sum(diag(m1))
  tot2 <- sum(diag(m2))
  # if dd is an odd number
  if(round(dd %% 2) == 0 & dd %% 2 != 0){
    diag(m2) <- rescale_integer_sum(x = diag(m2), tot = tot2 + 1)
    pop_grow <- sum(m2 - m1)
    dd <- nat_grow - pop_grow
    tot2 <- sum(diag(m2))
  }
  # alter diagonal of m1 and m2 so that they have same difference as b - d
  if(dd != 0){
    if(verbose){
      message("Difference in stocks (sum m2 - sum m1) is not the same as difference in natural population change (sum b - sum d). Adjusting native born populations so that stock difference and population change are equal.")
    }
    diag(m1) <- rescale_integer_sum(x = diag(m1), tot = tot1 - dd/2)
    diag(m2) <- rescale_integer_sum(x = diag(m2), tot = tot2 + dd/2)
  }
  return(list(m1_adj = m1, m2_adj = m2))
}
