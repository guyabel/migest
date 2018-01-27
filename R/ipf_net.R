#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Net Migration Totals.
#'
#' The \code{ipf_net} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ ??? }
#' 
#' @param ntot Vector of net migraiton totals to constrain the sum of the imputed cell columns. Elements must sum to zero.
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant typologies combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Iterative Proportional Fitting routine set up using the partial likelihood derivatives. The argument \code{ntot} takes the known net migration totals.
#' The user must ensure that the net migraiton totals sum to globally to zero.
#' 
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @author Guy J. Abel
#' @export
#'
#' @examples
#' dn <- LETTERS[1:4]
#' P1 <- matrix(c(0, 100, 10, 0, 55, 0, 50, 5, 80, 40, 0, 40, 20, 25, 20, 0), 4, 4, 
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' P2 <- matrix(c(0, 100, 60, 0, 80, 0, 75, 5, 90, 30, 0, 40, 40, 45, 0, 0), 4, 4, 
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' m <- ffs_econ(P1, P2, decrease = "return")
#' 
#' y <- ipf_net(ntot = c(30, 40, -15, -55), m = m)
#' addmargins(m)
#' net_sum(y$mu)
ipf_net <-
  function(ntot = NULL,
           m = NULL,
           tol = 1e-05,
           maxit = 500,
           verbose = TRUE) {
    R <- unique(c(dim(m), length(ntot)))
    if (length(R) != 1)
      stop("The m matrix must be square and with the same dimensions as the length of net total vector (ntot).")
    if (sum(ntot))
      message("Convergence will not be obtained as ntot does not sum to zero.")
    dn <- dimnames(m)[[1]]
    
    #set up offset
    if (is.null(m)) {
      m <- matrix(1, nrow = R, ncol = R)
    }
    if (is.null(dimnames(m))) {
      dimnames(m) <- list(orig = dn, dest = dn)
    }
    
    mu <- m
    it <- 0
    d_max <- tol * 2
    while (d_max > tol & it < maxit) {
      for(i in 1:R){
        p <- net_param(m = mu, region = i, ntot = ntot[i])
        mu <- net_scale(m = mu, region = i, alpha = p[p>0])
        mu
      }
      it <- it + 1
      d <- c(net_sum(mu) - ntot)
      d_max <- max(abs(d))
      if (verbose == TRUE)
        cat(c(it, d_max), "\n")
    }
    return(list(mu = mu, it = it, tol = d_max))
  }
