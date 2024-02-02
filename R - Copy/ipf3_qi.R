#' Iterative proportional fitting routine for the indirect estimation of origin-destination-migrant type migration flow tables with known origin and destination margins and diagonal elements.
#'
#' This function is predominantly intended to be used within the \code{\link{ffs}} routine.
#' 
#' The \code{ipf3} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ \log y_{ijk} = \log \alpha_{i} + \log \beta_{j} + \log \lambda_{k} + \log \gamma_{ik} + \log \kappa_{jk} + \log \delta_{ijk}I(i=j) + \log m_{ijk} }
#' where \eqn{m_{ijk}} is a set of prior estimates for \eqn{y_{ijk}} and is no more complex than the matrices being fitted. The \eqn{\delta_{ijk}I(i=j)} term ensures a saturated fit on the diagonal elements of each \eqn{(i,j)} matrix.
#' @param row_tot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param col_tot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param diag_count Array with counts on diagonal to constrain diagonal elements of the indirect estimates too. By default these are taken as their maximum possible values given the relevant margins totals in each table. If user specifies their own array of diagonal totals, values on the non-diagonals in the array can take any positive number (they are ultimately ignored).
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant typologies combinations. 
#' @param speed Speeds up the IPF algorithm by minimizing sufficient statistics.
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Iterative Proportional Fitting routine set up using the partial likelihood derivatives illustrated in Abel (2013). The arguments \code{row_tot} and \code{col_tot} take the row-table and column-table specific known margins. By default the diagonal values are taken as their maximum possible values given the relevant margins totals in each table. Diagonal values can be added by the user, but care must be taken to ensure resulting diagonals are feasible given the set of margins. 
#' 
#' The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) equal those provided in the row and column totals.
#' 
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @references 
#' Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3}}, \code{\link{ffs_demo}}
#' 
#' 
#' @examples
#' \donttest{
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
#' # # run ipf
#' # y <- ipf3_qi(row_tot = t(P1), col_tot = P2)
#' # # display with row, col and table totals
#' # round(addmargins(y$mu), 1)
#' # # origin-destination flow table
#' # round(sum_od(y$mu), 1)
#' 
#' ## with alternative offset term
#' # dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 4))
#' # y <- ipf3_qi(row_tot = t(P1), col_tot = P2, m = dis)
#' # # display with row, col and table totals
#' # round(addmargins(y$mu), 1)
#' # # origin-destination flow table
#' # round(sum_od(y$mu), 1)
#' } 
ipf3_qi <-
  function(row_tot = NULL,
           col_tot = NULL,
           diag_count = NULL,
           m = NULL,
           speed = TRUE,
           tol = 1e-05,
           maxit = 500,
           verbose = TRUE) {
    # P1.adj=P1;P2.adj=P2
    #row_tot=t(P1.adj);col_tot=P2.adj;diag_count=NULL;verbose=TRUE;tol=1e-05;maxit=500;speed=TRUE;m=NULL
    
    if (any(round(colSums(row_tot)) != round(rowSums(col_tot))))
      stop(
        "row and column totals are not equal for one or more sub-tables, ensure colSums(row_tot)==rowSums(col_tot)"
      )
    
    R <- unique(c(dim(row_tot), dim(col_tot)))
    if (length(R) != 1)
      stop("Row totals and column totals matrices must be square and with the same dimensions.")
    dn <- dimnames(row_tot)[[1]]
    
    n <- list(ik = row_tot,
              jk = t(col_tot),
              ijk = diag_count)
    #set up diagonals
    df1 <- expand.grid(a = 1:R, b = 1:R)
    if (is.null(diag_count)) {
      diag_count <- array(1, c(R, R, R))
      diag_count <-
        with(data = df1, 
             expr = replace(x = diag_count, 
                            list = cbind(a, a, b), 
                            values = apply(X = cbind(c(n$ik), c(n$jk)), MARGIN = 1, FUN = min)))
      n$ijk <- diag_count
    }
    
    #set up offset
    if (length(dim(m)) == 2) {
      m <- array(c(m), c(R, R, R))
    }
    if (is.null(m)) {
      m <- array(1, c(R, R, R))
    }
    if (is.null(dimnames(m))) {
      dimnames(m) <- list(orig = dn,
                          dest = dn,
                          pob = dn)
    }
    
    #alter ss (to speed up)
    if (speed == TRUE) {
      n$ik <- n$ik - (apply(X = n$ijk, MARGIN = c(1, 3), FUN = sum) - (R - 1))
      n$jk <- n$jk - (apply(X = n$ijk, MARGIN = c(2, 3), FUN = sum) - (R - 1))
      n$ijk <- with(data = df1, expr = replace(x = n$ijk, list = cbind(a, a, b),  values = 0))
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
      
      mu_margin$ijk <- with(data = df1, 
                          expr = replace(x = n$ijk, 
                                         list = cbind(a, a, b), 
                                         values = c(apply(X = mu, MARGIN = 3, FUN = diag))))
      mu_scaler$ijk <- n$ijk / mu_margin$ijk
      mu_scaler$ijk[is.nan(mu_scaler$ijk) | is.infinite(mu_scaler$ijk)] <- 0
      mu <- mu * mu_scaler$ijk
      
      it <- it + 1
      #d_max<-max(abs(unlist(n)-unlist(mu_margin)))
      #speeds up a lot if get rid of unlist (new to v1.6)
      d <- c(n$ik - mu_margin$ik, n$jk - mu_margin$jk, n$ijk - mu_margin$ijk)
      d_max <- max(abs(d))
      if (verbose == TRUE)
        cat(c(it, d_max), "\n")
    }
    if (speed == TRUE) {
      mu <-
        with(data = df1, 
             expr = replace(x = mu, 
                            list = cbind(a, a, b), 
                            values = c(sapply(X = 1:R, FUN = function(i) diag(diag_count[, , i])))))
    }
    return(list(mu = mu, it = it, tol = d_max))
  }
#rm(n,mu,mu_margin,mu_scaler)
#ipf3_qi(row_tot=t(P1.adj),col_tot=P2.adj,m=m)#
