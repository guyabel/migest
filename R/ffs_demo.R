#' Estimation of bilateral migrant flows from bilateral migrant stocks using demographic accounting approaches
#'
#' Estimates migrant transitions flows between two sequential migrant stock tables. Replaces old \code{ffs}.
#' @param m1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param b_por Vector of the number of births between time \emph{t} and \emph{t}+1 in each region.
#' @param d_por Vector of the number of deaths between time \emph{t} and \emph{t}+1 in each region.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations.
#' @param stayer_assumption Logical value to indicate whether to use \code{\link{ipf3}} or \code{ipf3_qi} to estimate flows. By default uses \code{ipf3_qi}, i.e. is set to \code{TRUE}. The \code{ipf} function is useful for replicating method of Azoze and Raferty.
#' @param match_global Character string used to indicate whether to balance the change in stocks totals with the changes in births and deaths. Only applied when \code{match_pob_tot_method} is either \code{rescale} or \code{rescale-adjust-zero-fb}. By default uses \code{after-demo-adjust} rather than \code{before-demo-adjust} which I think minimises risk of negative values.
#' @param match_pob_tot_method Character string passed to \code{method} argument in \code{match_pob_tot} to ensure place of birth margins in stock tables match.
#' @param birth_non_negative Logical value passed to \code{non_negative} argument in \code{birth_mat}.
#' @param death_method Character string passed to \code{method} argument in \code{death_mat}.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration of the various IPF routines. By default \code{FALSE}.
#' @param ... Additional arguments passes to \code{\link{ipf3_qi}} or \code{\link{ipf3}}.
#'
#' @return
#' Estimates migrant transitions flows between two sequential migrant stock tables using various methods. See the example section for possible variations on estimation methods.
#' 
#' Returns a \code{list} object with:
#' \item{mu }{Array of indirect estimates of origin-destination matrices by place of birth.}
#' \item{it }{Iteration count.}
#' \item{tol }{Tolerance level at final iteration.}
#' \item{y }{Array of indirect estimates of origin-destination matrices by place of birth with additional rows and columns for births, deaths and moves to other regions.}
#' \item{...}{Slots to record which estimation method was used (as set by arguments above)}
#' \item{od_flow }{Matrix of estimated origin-destination flows}
#' @references 
#' Abel and Cohen (2019) Bilateral international migration flow estimates for 200 countries \emph{Scientific Data} 6 (1), 1-13
#' 
#' Azose & Raftery (2019) Estimation of emigration, return migration, and transit migration between all pairs of countries \emph{Proceedings of the National Academy of Sciences} 116 (1) 116-122
#' 
#' Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by Gender between 1960 and 2015. \emph{International Migration Review} 52 (3), 809–852.
#' 
#' Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522
#' 
#' Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
#' @author Guy J. Abel
#' @seealso \code{\link{ffs_diff}}, \code{\link{ffs_rates}}
#' @aliases ffs
#' @export
#'
#' @examples
#' ##
#' ## without births and deaths over period
#' ##
#' # data as in papers
#' s1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' s2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' b <- d <- rep(0, 4)
#' r <- LETTERS[1:4]
#' dimnames(s1) <- dimnames(s2) <- list(pob = r, por = r)
#' names(b) <- names(d) <- r
#' s1
#' 
#' s2
#' 
#' b
#' 
#' d
#' 
#' # demographic research and science paper example
#' e0 <- ffs_demo(m1 = s1, m2 = s2, b_por = b, d_por = d)
#' e0$od_flow
#' 
#' # international migration review paper example
#' s1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
#' s2[,] <- c(70, 25, 10, 40, 30, 60, 55, 45, 10, 10, 140, 0, 10, 15, 50, 180)
#' e1 <- ffs_demo(m1 = s1, m2 = s2, b_por = b, d_por = d)
#' e1$od_flow
#' 
#' # international migration review supp. material example
#' # distance matrix
#' dd <- matrix(data = c(0, 5, 50, 500, 5, 0, 45, 495, 50, 45, 0, 450, 500, 495, 450, 0), 
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' dimnames(dd) <- list(orig = r, dest = r)
#' dd
#' e3 <- ffs_demo(m1 = s1, m2 = s2, b_por = b, d_por = d, m = dd)
#' e3$od_flow
#' 
#' ##
#' ## with births and deaths over period
#' ##
#' # demographic research paper example (with births and deaths)
#' s1[,] <- c(1000, 55, 80, 20, 100, 555, 40, 25, 10, 50, 800, 20, 0, 5, 40, 200)
#' s2[,] <- c(1060, 45, 70, 30, 60, 540, 75, 30, 10, 40, 770, 20, 10, 0, 70, 230)
#' b[] <- c(80, 20, 40, 60)
#' d[] <- c(70, 30, 50, 10)
#' e4 <- ffs_demo(m1 = s1, m2 = s2, b_por = b, d_por = d, match_pob_tot_method = "open-dr")
#' # makes more sense to use this method
#' e5 <- ffs_demo(m1 = s1, m2 = s2, b_por = b, d_por = d, match_pob_tot_method = "open")
#' e5$od_flow
#' 
#' # science paper  supp. material example
#' b[] <- c(80, 20, 60, 60)
#' e6 <- ffs_demo(m1 = s1, m2 = s2, b_por = b, d_por = d)
#' e6$od_flow
#' 
#' # international migration review supp. material example (with births and deaths)
#' s1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
#' s2[,] <- c(75, 20, 30, 30, 25, 45, 40, 30, 5, 30, 150, 20, 0, 15, 60, 230)
#' b[] <- c(10, 50, 25, 60)
#' d[] <- c(30, 10, 40, 10)
#' e7 <- ffs_demo(m1 = s1, m2 = s2, b_por = b, d_por = d)
#' e7$od_flow
# m1 = s1; m2 = s2; b_por = b; d_por = d; m = NULL
# m1 = s1; m2 = s2; b_por = births; d_por = deaths; m = NULL
# stayer_assumption = TRUE; match_pob_tot_method = "rescale"; birth_non_negative = TRUE; death_method = "proportion"; match_global = "after-demo-adjust"; verbose = FALSE
# match_pob_tot_method = "open";
ffs_demo <- function(m1 = NULL,
                     m2 = NULL, 
                     b_por = NULL, 
                     d_por = NULL, 
                     m = NULL,
                     stayer_assumption = TRUE,
                     match_global = "before-demo-adjust",
                     match_pob_tot_method = "rescale",
                     birth_non_negative = TRUE,
                     death_method = "proportion",
                     verbose = FALSE,
                     ...) {
  # make sure dimensions match
  R <- unique(c(dim(m1), dim(m2), length(b_por), length(d_por)))
  if (length(R) != 1)
    stop("m1 and m2 matrices must be square and with the same dimension as length of b_por and d_por.")
  
  # make sure data in same order
  if (is.null(names(b_por)) | is.null(names(b_por)) | is.null(dimnames(m1)) | is.null(dimnames(m2)))
    stop("m1 and m2 must be named matrices, b_por and d_por must be named vectors, all names must be common")
  
  # get dimension names and everything in same order
  dn <- union(names(b_por), names(d_por))
  dn <- union(dn, dimnames(m1)[[1]])
  dn <- union(dn, dimnames(m1)[[2]])
  dn <- union(dn, dimnames(m2)[[1]])
  dn <- union(dn, dimnames(m2)[[2]])
  m1_a <- m1[dn, dn]
  m2_a <- m2[dn, dn]
  b <- b_por[dn]
  d <- d_por[dn]
  
  # set up m and y to store results
  if(is.null(m) | length(dim(m) == 2)){
    m <- ipf_seed(m = m, R = R, n_dim = 3, dn = dn)
  }
  y <- array(0, dim(m) + c(2, 2, 0))
  dimnames(y) <- list(orig = c(dn, "birth", "outside"),
                      dest = c(dn, "death", "outside"),
                      pob = dn)
  
  if(match_global == "before-demo-adjust"  & match_pob_tot_method %in% c("rescale", "rescale-adjust-zero-fb")){
    if(verbose)
      message("Rescale native born cells for global zero net migration...")
    x <- rescale_nb(m1 = m1_a, m2 = m2_a, b = b, d = d)
    m1_a <- x$m1_adj
    m2_a <- x$m2_adj
  }
  
  # adjust for births and deaths
  if(verbose)
    message("Adjust stock tables for changes in births and deaths...")
  
  b_mat <- birth_mat(b_por = b, m2 = m2_a, non_negative = birth_non_negative)
  d_mat <- death_mat(d_por = d, m1 = m1_a, method = death_method, m2 = m2_a, b_por = b)
  m1_b <- m1_a - d_mat
  m2_b <- m2_a - b_mat
  
  # clean stocks so that population growth (m2 - m1) matches natural growth (b - d)
  # in scientific data paper used rescale_nb before births and deaths - not sure why, 
  # leads more risk of negative values when adjust pob row totals to match (next)
  m1_c <- m1_b
  m2_c <- m2_b
  if(match_global == "after-demo-adjust" & match_pob_tot_method %in% c("rescale", "rescale-adjust-zero-fb")){
    if(verbose)
      message("Rescale native born cells for global zero net migration...")
    dd <- sum(m2_c) - sum(m1_c)
    diag(m1_c) <- diag(m1_c) + dd * diag(m1_c)/sum(diag(m1_c)) * 0.5
    diag(m2_c) <- diag(m2_c) - dd * diag(m2_c)/sum(diag(m2_c)) * 0.5
  }
  
  # adjust for pob rows to match
  if(verbose)
    message("Rescale stock tables for equal place of birth totals...")
  x <- match_pob_tot(m1 = m1_c, m2 = m2_c, method = match_pob_tot_method, verbose = verbose)
  m1_d <- x$m1_adj
  m2_d <- x$m2_adj
  # round(rowSums(m1_d)); round(rowSums(m2_d));
  
  # ipf
  if(verbose)
    message("Estimate flows to match changes in adjusted stocks")
  if(stayer_assumption)
    fl <- ipf3_qi(row_tot = t(m1_d), col_tot = m2_d, m = m, verbose = verbose, ...)$mu
  if(!stayer_assumption)
    fl <- ipf3(row_tot = t(m1_d), col_tot = m2_d, m = m, verbose = verbose, ...)$mu
    # fl <- mipfp::Ipfp(seed = m, tol = 1e-03, iter = 1e05, 
    #                   # print = TRUE, 
    #                   target.list = list(c(1, 3), c(2,3)),
    #                   target.data = list(t(m1_d), m2_d))$x.hat

  # fill in y
  y[1:R, 1:R, ] <- fl
  y[R + 1, 1:R, ] <- b_mat
  y[R + 2, 1:R, ] <- t(x$out_mat)
  y[1:R, R + 1, ] <- t(d_mat)
  y[1:R, R + 2, ] <- t(x$in_mat)
  
  return(list(flow = fl, 
              y = y, 
              stayer_assumption = stayer_assumption,
              match_pob_tot_method = match_pob_tot_method,
              birth_non_negative = birth_non_negative,
              death_method = death_method, 
              od_flow = stats::addmargins(sum_od(fl))
  ))
}
