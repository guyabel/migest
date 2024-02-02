#' Iterative proportional fitting routine for the indirect estimation of origin-destination-migrant type migration flow tables with known origin and destination margins.
#'
#' The \code{ipf3} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ \log y_{ijk} = \log \alpha_{i} + \log \beta_{j} + \log \lambda_{k} + \log \gamma_{ik} + \log \kappa_{jk} + \log m_{ijk} }
#' where \eqn{m_{ijk}} is a set of prior estimates for \eqn{y_{ijk}} and is no more complex than the matrices being fitted.
#' @param row_tot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param col_tot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant typologies combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Iterative Proportional Fitting routine set up in a similar manner to Agresti (2002, p.343). The arguments \code{row_tot} and \code{col_tot} take the row-table and column-table specific known margins.
#' 
#' The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) to equal those provided in the row and column totals.
#' 
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @references 
#' Abel and Cohen (2019) Bilateral international migration flow estimates for 200 countries \emph{Scientific Data} 6 (1), 1-13
#' 
#' Azose & Raftery (2019) Estimation of emigration, return migration, and transit migration between all pairs of countries \emph{Proceedings of the National Academy of Sciences} 116 (1) 116-122
#' 
#' Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
#' 
#' Agresti, A. (2002). \emph{Categorical Data Analysis} 2nd edition. Wiley. 
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3_qi}}, \code{\link{ipf2}}
#' @export
#'
#' @examples
#' ## create row-table and column-table specific known margins.
#' dn <- LETTERS[1:4]
#' P1 <- matrix(c(1000, 100,  10,   0, 
#'                55,   555,  50,   5, 
#'                80,    40, 800 , 40, 
#'                20,    25,  20, 200), 
#'              nrow = 4, ncol = 4, byrow = TRUE, 
#'              dimnames = list(pob = dn, por = dn))
#' P2 <- matrix(c(950, 100,  60,   0, 
#'                 80, 505,  75,   5, 
#'                 90,  30, 800,  40, 
#'                 40,  45,   0, 180), 
#'              nrow = 4, ncol = 4, byrow = TRUE, 
#'              dimnames = list(pob = dn, por = dn))
#' # display with row and col totals
#' addmargins(P1)
#' addmargins(P2)
#' 
#' # run ipf
#' y <- ipf3(row_tot = t(P1), col_tot = P2)
#' # display with row, col and table totals
#' round(addmargins(y$mu), 1)
#' # origin-destination flow table
#' round(sum_od(y$mu), 1)
#' 
#' ## with alternative offset term
#' dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 4))
#' y <- ipf3(row_tot = t(P1), col_tot = P2, m = dis)
#' # display with row, col and table totals
#' round(addmargins(y$mu), 1)
#' # origin-destination flow table
#' round(sum_od(y$mu), 1)
ipf3 <-
  function(row_tot = NULL,
           col_tot = NULL,
           m = NULL,
           tol = 1e-05,
           maxit = 500,
           verbose = TRUE) {
    if (any(round(colSums(row_tot)) != round(rowSums(col_tot))))
      stop(
        "row and column totals are not equal for one or more sub-tables, ensure colSums(row_tot)==rowSums(col_tot)"
      )
    
    R <- unique(c(dim(row_tot), dim(col_tot)))
    if (length(R) != 1)
      stop("Row totals and column totals matrices must be square and with the same dimensions.")
    dn <- dimnames(row_tot)[[1]]
    
    n <- list(ik = row_tot,
              jk = t(col_tot))
    
    #set up offset
    if (length(dim(m)) == 2) {
      m <- array(data = c(m), dim = c(R, R, R))
    }
    if (is.null(m)) {
      m <- array(data = 1, dim = c(dim(row_tot), dim(row_tot)[1]))
    }
    if (is.null(dimnames(m))) {
      dimnames(m) <- list(orig = dn,
                          dest = dn,
                          pob = dn)
    }
    
    mu <- m
    mu_margin <- n
    mu_scaler <- n
    it <- 0
    d_max <- tol * 2
    while (d_max > tol & it < maxit) {
      mu_margin$ik <- apply(X = mu, MARGIN = c(1, 3), FUN = sum)
      mu_scaler$ik <- n$ik / mu_margin$ik
      mu_scaler$ik[is.nan(mu_scaler$ik) | is.infinite(mu_scaler$ik)] <- 0
      mu <- sweep(x = mu, MARGIN = c(1, 3), STATS = mu_scaler$ik, FUN = "*")
      
      mu_margin$jk <- apply(X = mu, MARGIN = c(2, 3), FUN = sum)
      mu_scaler$jk <- n$jk / mu_margin$jk
      mu_scaler$jk[is.nan(mu_scaler$jk) | is.infinite(mu_scaler$jk)] <- 0
      mu <- sweep(x = mu, MARGIN = c(2, 3), STATS = mu_scaler$jk, FUN = "*")
      
      it <- it + 1
      #d_max<-max(abs(unlist(n)-unlist(mu_margin)))
      #speeds up a lot if get rid of unlist (new to v1.7)
      d <- c(n$ik - mu_margin$ik, n$jk - mu_margin$jk)
      d_max <- max(abs(d))
      
      if (verbose == TRUE)
        cat(c(it, d_max), "\n")
    }
    return(list(mu = mu, it = it, tol = d_max))
  }