#' Estimation of Bilateral Migrant Flows from Bilateral Migrant Stocks Using Demographic Accounting Approaches
#'
#' Estimates migrant transitions flows between two sequential migrant stock tables. Replaces old \code{ffs}.
#' @param m1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param b_por Vector of the number of births between time \emph{t} and \emph{t}+1 in each region.
#' @param d_por Vector of the number of deaths between time \emph{t} and \emph{t}+1 in each region.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations.
#' @param stayer_assumption Logical value to indicate wheather to use \code{\link{ipf3}} or \code{\link{ipf3_qi}} to estimate flows. By default uses \code{ipf3_qi}, i.e. is set to \code{TRUE}. The \code{ipf} function is useful for replicating method of Azoze and Raferty.
#' @param match_pob_tot_method Character string passed to \code{method} argument in \code{\link{match_pob_tot}} to ensure place of birth margins in stock tables match.
#' @param birth_non_negative Logical value passed to \code{non_negative} argument in \code{\link{birth_mat}}.
#' @param death_method Character string passed to \code{method} argument in \code{\link{death_mat}}.
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
#' Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by Gender between 1960 and 2015. \emph{International Migration Review} Forthcoming.
#' 
#' Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522
#' 
#' Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3_qi}}, \code{\link{ffs_diff}}, \code{\link{ffs_rates}}
#' @aliases ffs
#' @export
#'
#' @examples
#' ##
#' ## without births and deaths over period
#' ##
#' # data as in papers
#' P1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' P2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' b <- d <- rep(0, 4)
#' reg <- LETTERS[1:4]
#' dimnames(P1) <- dimnames(P2) <- list(pob = reg, por = reg)
#' names(b) <- names(d) <- reg
#' P1; P2; b; d
#' 
#' # demographic research and science paper example
#' ffs_demo(m1 = P1, m2 = P2, b_por = b, d_por = d)
#' 
#' # international migration review paper example
#' P1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
#' P2[,] <- c(70, 25, 10, 40, 30, 60, 55, 45, 10, 10, 140, 0, 10, 15, 50, 180)
#' ffs_demo(m1 = P1, m2 = P2, b_por = b, d_por = d)
#' 
#' # international migration review supp. material example
#' dm <- matrix(data = c(0, 5, 50, 500, 5, 0, 45, 495, 50, 45, 0, 450, 500, 495, 450, 0), 
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' dimnames(dm) <- list(orig = reg, dest = reg)
#' ffs_demo(m1 = P1, m2 = P2, b_por = b, d_por = d, m = dm)
#' 
#' ##
#' ## with births and deaths over period
#' ##
#' # demographic research paper example
#' P1[,] <- c(1000, 55, 80, 20, 100, 555, 40, 25, 10, 50, 800, 20, 0, 5, 40, 200)
#' P2[,] <- c(1060, 45, 70, 30, 60, 540, 75, 30, 10, 40, 770, 20, 10, 0, 70, 230)
#' b[] <- c(80, 20, 40, 60)
#' d[] <- c(70, 30, 50, 10)
#' ffs_demo(m1 = P1, m2 = P2, b_por = b, d_por = d, match_pob_tot_method = "open-dr")
#' # makes more sense to use this method
#' ffs_demo(m1 = P1, m2 = P2, b_por = b, d_por = d, match_pob_tot_method = "open")
#' 
#' # science paper  supp. material example
#' b[] <- c(80, 20, 60, 60)
#' ffs_demo(m1 = P1, m2 = P2, b_por = b, d_por = d)
#' 
#' # international migration review supp. material example
#' P1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
#' P2[,] <- c(75, 20, 30, 30, 25, 45, 40, 30, 5, 30, 150, 20, 0, 15, 60, 230)
#' b[] <- c(10, 50, 25, 60)
#' d[] <- c(30, 10, 40, 10)
#' ffs_demo(m1 = P1, m2 = P2, b_por = b, d_por = d)
# m1 = P1; m2 = P2; b_por = b; d_por = d; m = NULL
# stayer_assumption = TRUE; match_pob_tot_method = "rescale"; birth_non_negative = TRUE; death_method = "proportion"
# match_pob_tot_method = "open";
ffs_demo <- function(m1 = NULL,
                     m2 = NULL, 
                     b_por = NULL, 
                     d_por = NULL, 
                     m = NULL,
                     stayer_assumption = TRUE,
                     match_pob_tot_method = "rescale",
                     birth_non_negative = TRUE,
                     death_method = "proportion",
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
  
  # clean stocks so that population growth (m2 - m1) matches natural growth (b - d)
  m1_b <- m1_a
  m2_b <- m2_a
  if(match_pob_tot_method == "rescale"){
    x <- rescale_nb(m1 = m1, m2 = m2, b = b, d = d)
    m1_b <- x$m1_adj
    m2_b <- x$m2_adj
  }
  
  # adjust for births and deaths
  b_mat <- birth_mat(b_por = b_por, m2 = m2_b, non_negative = birth_non_negative)
  d_mat <- death_mat(d_por = d_por, m1 = m1_b, method = death_method, m2 = m2_b, b_por = b)
  m1_c <- m1_b - d_mat
  m2_c <- m2_b - b_mat
  
  # adjust for pob rows to match
  x <- match_pob_tot(m1 = m1_c, m2 = m2_c, method = match_pob_tot_method)
  m1_d <- x$m1_adj
  m2_d <- x$m2_adj
  
  # ipf
  if(stayer_assumption)
    fl <- ipf3_qi(rtot = t(m1_d), ctot = m2_d, m = m,...)
  if(!stayer_assumption)
    fl <- ipf3(rtot = t(m1_d), ctot = m2_d, m = m,...)
  
  # fill in y
  y[1:R, 1:R, ] <- fl$mu
  y[R + 1, 1:R, ] <- b_mat
  y[R + 2, 1:R, ] <- t(x$out_mat)
  y[1:R, R + 1, ] <- t(d_mat)
  y[1:R, R + 2, ] <- t(x$in_mat)
  
  return(c(fl, 
           list(y = y, 
                stayer_assumption = stayer_assumption,
                match_pob_tot_method = match_pob_tot_method,
                birth_non_negative = birth_non_negative,
                death_method = death_method, 
                od_flow = stats::addmargins(sum_od(fl$mu)))
  ))
}
