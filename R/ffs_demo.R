#' Estimation of bilateral migrant flows from bilateral migrant stocks using demographic accounting approaches
#'
#' Estimates migrant transitions flows between two sequential migrant stock tables. Replaces old \code{ffs}.
#' @param stock_start Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}. Previously had argument name \code{m1}.
#' @param stock_end Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1. Previously had argument name \code{m2}.
#' @param births Vector of the number of births between time \emph{t} and \emph{t}+1 in each region. Previously had argument name \code{b_por}.
#' @param deaths Vector of the number of deaths between time \emph{t} and \emph{t}+1 in each region. Previously had argument name \code{d_por}.
#' @param seed Matrix of auxiliary data. By default set to 1 for all origin-destination combinations. Previously had argument name \code{m}.
#' @param stayer_assumption Logical value to indicate whether to use a quasi-independent or independent IPFP to estimate flows. By default uses quasi-independent, i.e. is set to \code{TRUE} and estimates the minimum migration. When set to \code{FALSE} estimates flows under the independent model as used as part of Azose and Raftery (2019).
#' @param match_global Character string used to indicate whether to balance the change in stocks totals with the changes in births and deaths. Only applied when \code{match_birthplace_tot_method} is either \code{rescale} or \code{rescale-adjust-zero-fb}. By default uses \code{after-demo-adjust} rather than \code{before-demo-adjust} which I think minimises risk of negative values.
#' @param match_birthplace_tot_method Character string passed to \code{method} argument in \code{match_birthplace_tot} to ensure place of birth margins in stock tables match.
#' @param birth_method Character string passed to \code{method} argument in \code{birth_mat}.
#' @param birth_non_negative Logical value passed to \code{non_negative} argument in \code{birth_mat}.
#' @param death_method Character string passed to \code{method} argument in \code{death_mat}.
#' @param verbose Logical value to show progress of the estimation procedure. By default \code{FALSE}.
#' @param return Character string used to indicate whether to return the array of estimated flows when set to \code{flow} (default), array of demographic accounts when set to \code{account} or the demographic account, list of input settings and the origin-destination matrix when set to \code{classic}
#'
#' @return
#' Estimates migrant transitions flows between two sequential migrant stock tables using various methods. See the example section for possible variations on estimation methods.
#' 
#' Detail of returned object varies depending on the setting used in the \code{return} argument.
#' 
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
#' # data as in demographic research and science paper papers
#' s1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' s2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' b <- d <- rep(0, 4)
#' r <- LETTERS[1:4]
#' dimnames(s1) <- dimnames(s2) <- list(birth =  r, dest = r)
#' names(b) <- names(d) <- r
#' addmargins(s1)
#' addmargins(s2)
#' b
#' d
#' 
#' # demographic research and science paper example
#' e0 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
#' e0
#' sum_od(e0)
#' 
#' # international migration review paper example
#' s1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
#' s2[,] <- c(70, 25, 10, 40, 30, 60, 55, 45, 10, 10, 140, 0, 10, 15, 50, 180)
#' addmargins(s1)
#' addmargins(s2)
#' 
#' e1 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
#' sum_od(e1)
#' 
#' # international migration review supp. material example
#' # distance matrix
#' dd <- matrix(data = c(0, 5, 50, 500, 5, 0, 45, 495, 50, 45, 0, 450, 500, 495, 450, 0),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' dimnames(dd) <- list(orig = r, dest = r)
#' dd
#' e2 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d, seed = dd)
#' sum_od(e2)
#' 
#' ##
#' ## with births and deaths over period
#' ##
#' # demographic research paper example (with births and deaths)
#' s1[,] <- c(1000, 55, 80, 20, 100, 555, 40, 25, 10, 50, 800, 20, 0, 5, 40, 200)
#' s2[,] <- c(1060, 45, 70, 30, 60, 540, 75, 30, 10, 40, 770, 20, 10, 0, 70, 230)
#' b[] <- c(80, 20, 40, 60)
#' d[] <- c(70, 30, 50, 10)
#' e3 <- ffs_demo(stock_start = s1, stock_end = s2, 
#'                births = b, deaths = d, 
#'                match_birthplace_tot_method = "open-dr")
#' sum_od(e3)
#' # makes more sense to use this method
#' e4 <- ffs_demo(stock_start = s1, stock_end = s2, 
#'                births = b, deaths = d, 
#'                match_birthplace_tot_method = "open")
#' sum_od(e4)
#' 
#' # science paper  supp. material example
#' b[] <- c(80, 20, 60, 60)
#' e5 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
#' sum_od(e5)
#' 
#' # international migration review supp. material example (with births and deaths)
#' s1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
#' s2[,] <- c(75, 20, 30, 30, 25, 45, 40, 30, 5, 30, 150, 20, 0, 15, 60, 230)
#' b[] <- c(10, 50, 25, 60)
#' d[] <- c(30, 10, 40, 10)
#' e6 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
#' sum_od(e6)
#' 
#' # scientific data 2019 paper
#' s1[] <- c(100, 80, 30, 60, 10, 180, 10, 70, 10, 10, 140, 10, 0, 90, 40, 160)
#' s2[] <- c(95, 75, 55, 35, 5, 225, 0, 25, 15, 5, 115, 25, 5, 55, 50, 215)
#' b[] <- c(0, 0, 0, 0)
#' d[] <- c(0, 0, 0, 0)
#' e7 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
#' sum_od(e7)
# source("C:/Users/Guy/Documents/GitHub/migest/R/ipf_seed.R")
# source("C:/Users/Guy/Documents/GitHub/migest/R/birth_mat.R");
# source("C:/Users/Guy/Documents/GitHub/migest/R/death_mat.R")
# source("C:/Users/Guy/Documents/GitHub/migest/R/match_birthplace_tot.R"); 
# source("C:/Users/Guy/Documents/GitHub/migest/R/nb_scale_global.R")
# source("C:/Users/Guy/Documents/GitHub/migest/R/nb_non_zero.R")

# stock_start = s1; stock_end = s2; births = b; deaths = d; seed = NULL
# stock_start = x$stock_start; stock_end = x$stock_end; births = x$births; deaths = x$deaths; seed = NULL
# #defaults
# seed = NULL
# stayer_assumption = TRUE
# match_global = "before-demo-adjust"
# match_birthplace_tot_method = "rescale"
# birth_method = "native"
# birth_non_negative = TRUE
# death_method = "proportion"
# verbose = FALSE
ffs_demo <- function(stock_start = NULL,
                     stock_end = NULL, 
                     births = NULL, 
                     deaths = NULL, 
                     seed = NULL,
                     stayer_assumption = TRUE,
                     match_global = "before-demo-adjust",
                     match_birthplace_tot_method = "rescale",
                     birth_method = "native",
                     birth_non_negative = TRUE,
                     death_method = "proportion",
                     verbose = FALSE,
                     return = "flow") {
  # make sure dimensions match
  n <- unique(c(dim(stock_start), dim(stock_end), length(births), length(deaths)))
  if (length(n) != 1)
    stop("stock_start and stock_end matrices must be square and with the same dimension as length of births and deaths.")
  
  # make sure data in same order
  if (is.null(names(births)) | is.null(names(births)) | is.null(dimnames(stock_start)) | is.null(dimnames(stock_end)))
    stop("stock_start and stock_end must be named matrices, births and deaths must be named vectors, all names must be common")
  
  # get dimension names and everything in same order
  r <- union(names(births), names(deaths))
  r <- union(r, dimnames(stock_start)[[1]])
  r <- union(r, dimnames(stock_start)[[2]])
  r <- union(r, dimnames(stock_end)[[1]])
  r <- union(r, dimnames(stock_end)[[2]])
  # r <- r[r != "KWT"]
  stock_start1 <- stock_start[r, r]
  stock_end1 <- stock_end[r, r]
  births1 <- births[r]
  deaths1 <- deaths[r]
  
  # set up m and y to store results
  if(is.null(seed) | length(dim(seed) == 2)){
    seed <- ipf_seed(m = seed, R = n, n_dim = 3, dn = r)
  }
  y <- array(0, dim(seed) + c(2, 2, 0))
  dimnames(y) <- list(orig = c(r, "birth", "outside"),
                      dest = c(r, "death", "outside"),
                      birth = r)
  
  # check for negative stocks
  stock_start1 <- nb_non_zero(m = stock_start1)
  stock_end1 <- nb_non_zero(m = stock_end1) 
  
  stock_start2 <- stock_end2 <- NULL
  if(match_global == "before-demo-adjust"  & match_birthplace_tot_method %in% c("rescale", "rescale-adjust-zero-fb")){
    if(verbose)
      message("Rescale native born cells for global zero net migration...")
    x1 <- nb_scale_global(m1 = stock_start1, m2 = stock_end1, b = births1, d = deaths1)
    stock_start2 <- x1$m1_adj
    stock_end2 <- x1$m2_adj
    # sum(stock_end1) - sum(b) + sum(d); sum(stock_start1)
  }
  if(is.null(stock_start2))
    stock_start2 <- stock_start1
  if(is.null(stock_end2))
    stock_end2 <- stock_end1

  # adjust for births and deaths
  if(verbose)
    message("Adjust stock tables for changes in births and deaths...")
  b_mat <- birth_mat(b_por = births1, m2 = stock_end2, method = birth_method, non_negative = birth_non_negative)
  d_mat <- death_mat(d_por = deaths1, m1 = stock_start2, method = death_method, m2 = stock_end2, b_por = births1)
  stock_start3 <- stock_start2 - d_mat
  stock_end3 <- stock_end2 - b_mat
  
  # clean stocks so that population growth (m2 - m1) matches natural growth (b - d)
  # in scientific data paper used nb_scale_global before births and deaths - not sure why, 
  # leads more risk of negative values when adjust orig row totals to match (next)
  stock_start4 <- stock_start3
  stock_end4 <- stock_end3
  if(match_global == "after-demo-adjust" & match_birthplace_tot_method %in% c("rescale", "rescale-adjust-zero-fb")){
    if(verbose)
      message("Rescale native born cells for global zero net migration...")
    dd <- sum(stock_end4) - sum(stock_start4)
    diag(stock_start4) <- diag(stock_start4) + dd * diag(stock_start4)/sum(diag(stock_start4)) * 0.5
    diag(stock_end4) <- diag(stock_end4) - dd * diag(stock_end4)/sum(diag(stock_end4)) * 0.5
  }
  
  # adjust for birthplace rows to match
  if(verbose)
    message("Rescale stock tables for equal place of birth totals...")
  x2 <- match_birthplace_tot(m1 = stock_start4, m2 = stock_end4, method = match_birthplace_tot_method, verbose = verbose)
  stock_start5 <- x2$m1_adj
  stock_end5 <- x2$m2_adj
  
  # ipf
  if(verbose)
    message("Estimate flows to match changes in adjusted stocks")
  if(stayer_assumption){
    xx <- expand.grid(a = 1:n, b = 1:n)
    diag_count <- array(0, c(n, n, n))
    diag_count <- with(
      data = xx, 
      expr = replace(x = diag_count,
                     list = cbind(a, a, b), 
                     values = apply(X = cbind(c(t(stock_start5)), c(t(stock_end5))), 
                                    MARGIN = 1, 
                                    FUN = min)))
    dimnames(diag_count) <- dimnames(seed)
    
    diag_zero <- diag_count + 1
    diag_zero <- with(data = xx, expr = replace(x = diag_zero, 
                                                list = cbind(a, a, b), 
                                                values = 0))
    
    x0 <- t(stock_start5) - apply(X = diag_count, MARGIN = c(1, 3), FUN = sum)
    x1 <- t(stock_end5) - apply(X = diag_count, MARGIN = c(1, 3), FUN = sum)
    # tiny differences in column sums. rescale to get matching
    x1 <- mipfp::Ipfp(
      seed = x1, 
      target.list = list(2), 
      target.data = list(colSums(x0)),
      tol = 1e-03, iter = 1e05, tol.margins = 1e-03
    )$x.hat
    
    # # checking 
    # g <- colSums(x0) %>%
    #   enframe(value = "x0") %>%
    #   mutate(x1 = colSums(x1))
    # g %>%
    #   filter(x0 != x1) %>%
    #   mutate(d = x0 - x1, 
    #          abs_d = abs(d)) %>%
    #   arrange(-abs_d)

    a0 <- mipfp::Ipfp(
      seed = diag_zero,
      target.list = list(c(1, 3), c(2, 3)),
      target.data = list(x0, x1),
      tol = 1e-03, iter = 1e05, tol.margins = 1e-03
    )
    f0 <- a0$x.hat + diag_count
  }
  
  if(!stayer_assumption){
    f0 <- mipfp::Ipfp(
      seed = seed,
      target.list = list(c(1, 3), c(2,3)),
      target.data = list(t(stock_start5), t(stock_end5)),
      tol = 1e-03, iter = 1e05, tol.margins = 1e-03
      )$x.hat
  }
  
  if(return == "flow")
    rr <- f0
  
  if(return %in% c("account", "classic")){
    y[1:n, 1:n, ] <- f0
    y[n + 1, 1:n, ] <- b_mat
    y[n + 2, 1:n, ] <- t(x2$out_mat)
    y[1:n, n + 1, ] <- t(d_mat)
    y[1:n, n + 2, ] <- t(x2$in_mat)
    rr <- y
  }
    
  if(return == "classic")
    rr <- list(flow = f0, 
               y = y, 
               stayer_assumption = stayer_assumption,
               match_birthplace_tot_method = match_birthplace_tot_method,
               birth_non_negative = birth_non_negative,
               death_method = death_method, 
               od_flow = f0 %>%
                 sum_od() %>%
                 stats::addmargins()
    )
  return(rr)
}
