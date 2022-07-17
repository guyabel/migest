#' Adjust migrant stock tables to have matching place of birth totals
#'
#' This function is predominantly intended to be used within the ffs routines in the migest package.
#' @param m1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param method Character string matching either \code{rescale}, \code{rescale-adjust-zero-fb}, \code{open} or \code{open-dr}. See details.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration of the rescale, as used in \code{ipf2}. By default \code{FALSE}.
#'
#' @details
#' The \code{rescale} and \code{rescale-adjust-zero-fb} method ensure flow estimates closely match the net migration totals implied by the changes in population totals, births and deaths - as introduced in the Science paper. The \code{rescale-adjust-zero-fb} can adjust for rare cases when row total margins that are smaller than native born totals in countries where there are no foreign born populations (e.g. South Sudan 1990-1995).
#' The \code{open-dr} method allows for moves in and out of the global system - as introduced in the Demographic Research paper. The \code{open} method is a slight improvement over \code{open-dr} - the calculation of the moves and in and out using more sensible weights.
#'
#' @return Returns a \code{list} object with:
#' \item{m1_adj }{Matrix of adjusted \code{m1} where rows (place of births) match \code{m2_adj}.}
#' \item{m2_adj }{Matrix of adjusted \code{m2} where rows (place of births) match \code{m1_adj}.}
#' \item{in_mat }{Matrix of estimated inflows into the system.}
#' \item{out_mat }{Matrix of estimated outflows from the system.}
#' @references
#' Abel and Cohen (2019) Bilateral international migration flow estimates for 200 countries \emph{Scientific Data} 6 (1), 1-13
#'
#' Azose & Raftery (2019) Estimation of emigration, return migration, and transit migration between all pairs of countries \emph{Proceedings of the National Academy of Sciences} 116 (1) 116-122
#'
#' Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by Gender between 1960 and 2015. \emph{International Migration Review} 52 (3), 809–852.
#'
#' Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522
#'
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3_qi}}, \code{\link{ffs_diff}}
match_pob_tot <- function(m1, m2, method = "rescale", verbose = FALSE){
  if (!(method %in% c("open", "open-dr", "rescale-adjust-zero-fb", "rescale")) | length(method) != 1)
    stop("method must be open, open-dr, rescale-adjust-zero-fb or rescale")
  # m1 = m1_c; m2 = m2_c
  # method = "rescale"
  dd <- rowSums(m1) - rowSums(m2)
  # round(sum(dd))
  pob1 <- nb1 <- pob2 <- nb2 <- value <- mid_target <- target <- fb1 <- fb2 <- NULL
  if(method == "rescale-adjust-zero-fb"){
    z <-
      dd %>%
      tibble::enframe() %>%
      dplyr::mutate(nb1 = diag(m1),
                    nb2 = diag(m2),
                    pob1 = rowSums(m1),
                    pob2 = rowSums(m2),
                    expat1 = pob1 - nb1,
                    expat2 = pob2 - nb2,
                    mid_target = rowSums(m1) - value/2,
                    fb1 = colSums(m1) - nb1,
                    fb2 = colSums(m2) - nb2,
                    target = ifelse(nb1 > mid_target, nb1, mid_target),
                    target = ifelse(nb2 > mid_target, nb2, target)) %>%
      # arrange(-value) %>%
      dplyr::filter(fb1 == 0 | fb2 == 0) %>%
      dplyr::filter(mid_target != target)

    dd_wz <- dd
    zid <- which(names(dd) %in% z$name)
    x <- rescale_nb(m1 = m1[-zid, -zid], m2 = m2[-zid, -zid], b = 0, d = 0)
    m1_wz <- x$m1_adj
    m2_wz <- x$m2_adj
    dd_wz <- rowSums(m1_wz) - rowSums(m2_wz)
    # sum(dd_wz)

    # m1_wz_adj <- ipf2(row_tot = rowSums(m1_wz) - dd_wz, col_tot = colSums(m1_wz), m = m1_wz, maxit = 1e05, tol = 0.1, verbose = verbose)
    # m2_wz_adj <- ipf2(row_tot = rowSums(m2_wz) + dd_wz, col_tot = colSums(m2_wz), m = m2_wz, maxit = 1e05, tol = 0.1, verbose = verbose)
    
    m1_wz_adj <- mipfp::Ipfp(seed = m1_wz, 
                      target.list = list(1, 2), 
                      target.data = list(rowSums(m1_wz) - dd_wz, 
                                         colSums(m1_wz)),
                      tol = 1e-05)
    m2_wz_adj <- mipfp::Ipfp(seed = m2_wz, 
                      target.list = list(1, 2), 
                      target.data = list(rowSums(m2_wz) + dd_wz, 
                                         colSums(m2_wz)),
                      tol = 1e-05)
    m1_z <- m1[zid,]
    m2_z <- m2[zid,]
    if(length(zid) == 1){
      m1_z <- matrix(m1_z, nrow = 1, ncol = length(m1_z), dimnames = list(orig = z$name, dest = names(m1_z)))
      m2_z <- matrix(m2_z, nrow = 1, ncol = length(m2_z), dimnames = list(orig = z$name, dest = names(m2_z)))
    }
    m1_z_adj <- sweep(x = m1_z, MARGIN = 1, STATS = z$target/rowSums(m1_z), FUN = "*")
    m2_z_adj <- sweep(x = m2_z, MARGIN = 1, STATS = z$target/rowSums(m2_z), FUN = "*")

    m1_adj <- m1
    m2_adj <- m2
    m1_adj[,] <- 0
    m2_adj[,] <- 0
    m1_adj[-zid,-zid] <- m1_wz_adj$mu
    m2_adj[-zid,-zid] <- m2_wz_adj$mu
    m1_adj[zid,] <- m1_z_adj
    m2_adj[zid,] <- m2_z_adj
    # colSums(m2_adj)[zid] - colSums(m1_adj)[zid]
  }


  if(method == "rescale"){
    m1_adj <- mipfp::Ipfp(seed = m1, tol = 1e-03, iter = 1e05,
                          # print = TRUE,
                          target.list = list(1, 2),
                          target.data = list(rowSums(m1) - dd / 2, colSums(m1)),
                          tol = 1e-05)
    m2_adj <- mipfp::Ipfp(seed = m2, tol = 1e-03, iter = 1e05,
                          # print = TRUE,
                          target.list = list(1, 2),
                          target.data = list(rowSums(m2) + dd / 2, colSums(m2)),
                          tol = 1e-05)
    if(min(m1_adj$x.hat) < 0 | min(m2_adj$x.hat) < 0)
      message("negative flows from rescale")
    if(m1_adj$conv == FALSE | m2_adj$conv == FALSE)
      message("no convergence in rescaling")
    
    m1_adj <- m1_adj$x.hat
    m2_adj <- m2_adj$x.hat
  }
  # zeros for in and out matrices for return object
  zeros <- mipfp::Ipfp(seed = m1, target.list = list(1, 2), target.data = list(0, 0), tol = 1e-05)
  in_mat <- out_mat <- zeros
  if(method == "open"){
    in_mat <- mipfp::Ipfp(seed = m1, tol = 1e-03, iter = 1e05,
                          target.list = list(1),
                          target.data = list(pmax(-dd, 0)))
    out_mat <- mipfp::Ipfp(seed = m2, tol = 1e-03, iter = 1e05,
                          target.list = list(1),
                          target.data = list(pmax(dd, 0)))
    m1_adj <- m1 - out_mat$x.hat
    m2_adj <- m2 - in_mat$x.hat
  }
  if(method == "open-dr"){
    in_mat <- mipfp::Ipfp(seed = m2, tol = 1e-03, iter = 1e05,
                          target.list = list(1),
                          target.data = list(pmax(-dd, 0)))
    out_mat <- mipfp::Ipfp(seed = m1, tol = 1e-03, iter = 1e05,
                           target.list = list(1),
                           target.data = list(pmax(dd, 0)))
    m1_adj <- m1 - out_mat$x.hat
    m2_adj <- m2 - in_mat$x.hat
  }

  # if(method == "rescale"){
  #   m1_adj <- ipf2(row_tot = rowSums(m1) - dd/2, col_tot = colSums(m1), m = m1, maxit = 1e05, tol = 0.1, verbose = verbose)
  #   m2_adj <- ipf2(row_tot = rowSums(m2) + dd/2, col_tot = colSums(m2), m = m2, maxit = 1e05, tol = 0.1, verbose = verbose)
  # 
  #   # round(rowSums(m1) - dd/2)
  #   # round(rowSums(m2) + dd/2)
  # 
  #   # zeros for in and out matrices
  #   in_mat <- ipf2(row_tot = 0, m = m1)$mu
  #   out_mat <- ipf2(row_tot = 0, m = m2)$mu
  # 
  #   if(min(m1_adj$mu) < 0 | min(m2_adj$mu) < 0)
  #     message("negative flows from rescale")
  #   if(m1_adj$tol > 1 | m2_adj$tol > 1)
  #     message("no convergence in rescaling")
  #   m1_adj <- m1_adj$mu
  #   m2_adj <- m2_adj$mu
  # }
  # if(method == "open"){
  #   in_mat <- ipf2(row_tot = pmax(-dd, 0), m = m1, verbose = verbose)$mu
  #   out_mat <- ipf2(row_tot = pmax(dd, 0), m = m2, verbose = verbose)$mu
  #   m1_adj <- m1 - out_mat
  #   m2_adj <- m2 - in_mat
  # }
  # if(method == "open-dr"){
  #   in_mat <- ipf2(row_tot = pmax(-dd, 0), m = m2, verbose = verbose)$mu
  #   out_mat <- ipf2(row_tot = pmax(dd, 0), m = m1, verbose = verbose)$mu
  #   m1_adj <- m1 - out_mat
  #   m2_adj <- m2 - in_mat
  # }
  # open-dr not preferred.. using m2 as offset for leavers.. i.e. distribution at end of period, not start of period (as in open)
  return(list(m1_adj = m1_adj, m2_adj = m2_adj, in_mat = in_mat$x.hat, out_mat = out_mat$x.hat))
}
