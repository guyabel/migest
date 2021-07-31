#' Calculate deaths for each element of place of birth - place of residence stock matrix
#'
#' This function is predominantly intended to be used within the \code{ffs} routines in the migest package.
#' @param d_por Vector of numeric values for deaths in each place of residence.
#' @param m1 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1. Used to distribute deaths proportionally to each migrant stock population. For use when \code{method = "accounting"}
#' @param method Character string of either \code{"proportion"} or \code{"accounting"} to choose method to distrubte deaths. The \code{"proportion"} method assumes the mortality rate in each place of birth sub-group (native born and all foreign born stocks) is the same. The \code{"accounting"} method ensures that the the deaths by place of birth matches that implied by demographic accounting. Still needs to be explored fully.
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1. Used to distribute deaths proportionally to each migrant stock population.
#' @param b_por Vector of numeric values for births in each place of residence. For use when \code{method = "accounting"}.
#'
#' @return
#' Matrix of place of death by place of residence
#' @keyword internal
#' @noRd
death_mat <- function(d_por = NULL, m1 = NULL, method = "proportion",
                      m2 = NULL, b_por = NULL){
  if (method == "proportion")
    dd <- ipf2(col_tot = d_por, m = m1)$mu
    # dd <- mipfp::Ipfp(seed = m1,
    #                   target.list = list(2),
    #                   target.data = d_por)$x.hat

  if (method == "accounting"){
    d_pob <- rowSums(m1) + b_por - rowSums(m2)
    # dd <- mipfp::Ipfp(seed = m1,
    #                   target.list = list(1, 2),
    #                   target.data = list(d_pob, d_por))$x.hat
    dd <- ipf2(col_tot = d_por, row_tot = d_pob, m = m1)$mu
  }
  return(dd)
}

