#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Migrant Type Migration Flow Tables with Known Origin and Destination Margins.
#'
#' The \code{ipf3} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ \log y_{ijk} = \log \alpha_{i} + \log \beta_{j} + \log \lambda_{k} + \log \gamma_{ik} + \log \kappa_{jk} + \log m_{ijk} }
#' where \eqn{m_{ijk}} is a set of prior estimates for \eqn{y_{ijk}} and is no more complex than the matrices being fitted.
#' @param rtot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param ctot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant typologies combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Iterative Proportional Fitting routine set up in a similar manner to Agresti (2002, p.343). The arguments \code{rtot} and \code{ctot} take the row-table and column-table specific known margins.
#' 
#' The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) to equal those provided in the row and column totals.
#' 
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @references 
#' Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
#' 
#' Agresti, A. (2002). \emph{Categorical Data Analysis} 2nd edition. Wiley. 
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3.qi}}, \code{\link{ipf2}}
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
#' y <- ipf3(rtot = t(P1), ctot = P2)
#' # display with row, col and table totals
#' round(addmargins(y$mu), 1)
#' # origin-destination flow table
#' round(fm(y$mu), 1)
#' 
#' ## with alternative offset term
#' dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 4))
#' y <- ipf3(rtot = t(P1), ctot = P2, m = dis)
#' # display with row, col and table totals
#' round(addmargins(y$mu), 1)
#' # origin-destination flow table
#' round(fm(y$mu), 1) 
ipf3 <-
  function(rtot = NULL,
           ctot = NULL,
           m = NULL,
           tol = 1e-05,
           maxit = 500,
           verbose = TRUE) {
    if (any(round(colSums(rtot)) != round(rowSums(ctot))))
      stop(
        "row and column totals are not equal for one or more sub-tables, ensure colSums(rtot)==rowSums(ctot)"
      )
    
    R <- unique(c(dim(rtot), dim(ctot)))
    if (length(R) != 1)
      stop("Row totals and column totals matrices must be square and with the same dimensions.")
    dn <- dimnames(rtot)[[1]]
    
    n <- list(ik = rtot,
              jk = t(ctot))
    
    #set up offset
    if (length(dim(m)) == 2) {
      m <- array(c(m), c(R, R, R))
    }
    if (is.null(m)) {
      m <- array(1, c(dim(rtot), dim(rtot)[1]))
    }
    if (is.null(dimnames(m))) {
      dimnames(m) <- list(orig = dn,
                          dest = dn,
                          pob = dn)
    }
    
    mu <- m
    mu.marg <- n
    m.fact <- n
    it <- 0
    max.diff <- tol * 2
    while (max.diff > tol & it < maxit) {
      mu.marg$ik <- apply(mu, c(1, 3), sum)
      m.fact$ik <- n$ik / mu.marg$ik
      m.fact$ik[is.nan(m.fact$ik)] <- 0
      m.fact$ik[is.infinite(m.fact$ik)] <- 0
      mu <- sweep(mu, c(1, 3), m.fact$ik, "*")
      
      mu.marg$jk <- apply(mu, c(2, 3), sum)
      m.fact$jk <- n$jk / mu.marg$jk
      m.fact$jk[is.nan(m.fact$jk)] <- 0
      m.fact$jk[is.infinite(m.fact$jk)] <- 0
      mu <- sweep(mu, c(2, 3), m.fact$jk, "*")
      
      it <- it + 1
      #max.diff<-max(abs(unlist(n)-unlist(mu.marg)))
      #speeds up a lot if get rid of unlist (new to v1.7)
      max.diff <- max(abs(c(n$ik - mu.marg$ik, n$jk - mu.marg$jk)))
      
      if (verbose == TRUE)
        cat(c(it, max.diff), "\n")
    }
    return(list(mu = mu, it = it, tol = max.diff))
  }
