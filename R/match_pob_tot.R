#' Adjust Migrant Stock Tables to Have Matching Place of Birth Totals
#'
#' This function is predominantly intended to be used within the ffs routines in the migest package. 
#' @param m1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param method Character string matching either \code{rescale}, \code{open}, \code{open-dr}. The \code{rescale} method ensure flow estimates closely match the net migration totals implied by the changes in population totals, births and deaths - as introduced in the Science paper. The \code{open-dr} method allows for moves in and out of the global system - as introduced in the Demographic Research paper. The \code{open} method is a slight improvement over \code{open-dr} - the calculation of the moves and in and out use more sensible weights.
#'
#' @return Returns a \code{list} object with:
#' \item{m1_adj }{Matrix of adjusted \code{m1} where rows (place of births) match \code{m2_adj}.}
#' \item{m2_adj }{Matrix of adjusted \code{m2} where rows (place of births) match \code{m1_adj}.}
#' \item{in_mat }{Matrix of estimated inflows into the system.}
#' \item{out_mat }{Matrix of estimated outflows from the system.}
#' @references 
#' Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by Gender between 1960 and 2015. \emph{International Migration Review} Forthcoming.
#' 
#' Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3_qi}}, \code{\link{ffs_diff}}
#' @export
match_pob_tot <- function(m1, m2, method = "rescale"){
  if (!(method %in% c("open", "open-dr", "rescale")) | length(method) != 1)
    stop("method must be open, open-dr or rescale")
  # m1 = m1_c; m2 = m2_c
  dd <- rowSums(m1) - rowSums(m2)
  if(method == "rescale"){
    m1_adj <- ipf2(rtot = rowSums(m1) - dd / 2, ctot = colSums(m1), m = m1, maxit = 1e05, tol = 0.1)$mu
    m2_adj <- ipf2(rtot = rowSums(m2) + dd / 2, ctot = colSums(m2), m = m2, maxit = 1e05, tol = 0.1)$mu
    # zeros for in and out matrices
    in_mat <- ipf2(rtot = 0, m = m1)$mu
    out_mat <- ipf2(rtot = 0, m = m2)$mu
  }
  if(method == "open"){
    in_mat <- ipf2(rtot = pmax(-dd, 0), m = m1)$mu
    out_mat <- ipf2(rtot = pmax(dd, 0), m = m2)$mu
    m1_adj <- m1 - out_mat
    m2_adj <- m2 - in_mat
  }
  if(method == "open-dr"){
    in_mat <- ipf2(rtot = pmax(-dd, 0), m = m2)$mu
    out_mat <- ipf2(rtot = pmax(dd, 0), m = m1)$mu
    m1_adj <- m1 - out_mat
    m2_adj <- m2 - in_mat
  }
  # open-dr not preferred.. using m2 as offset for leavers.. i.e. distribution at end of period, not start of period (as in open)
  return(list(m1_adj = m1_adj, m2_adj = m2_adj, in_mat = in_mat, out_mat = out_mat))
}
