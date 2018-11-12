#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Net Migration Totals.
#'
#' The \code{ipf_net} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{\log y_{ij} = \log \alpha_{i} + \log \alpha_{i}^{-1} + \log m_{ij} }
#' 
#' @param net_tot Vector of net migration totals to constrain the sum of the imputed cell columns. Elements must sum to zero.
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant typologies combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Iterative Proportional Fitting routine set up using the partial likelihood derivatives. The argument \code{net_tot} takes the known net migration totals.
#' The user must ensure that the net migration totals sum to globally to zero.
#' 
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @author Guy J. Abel
#' @export
#'
#' @examples
#' m <- matrix(data = 1:16, nrow = 4)
#' y <- ipf_net(net_tot = c(30, 40, -15, -55), m = m)
#' addmargins(m)
#' sum_net(y$mu)
#' 
#' dn <- LETTERS[1:4]
#' m <- matrix(c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0), 4, 4,
#'             dimnames = list(orig = dn, dest = dn), byrow = TRUE)
#' addmargins(m)
#' sum_net(m)
#' 
#' y <- ipf_net(net_tot = c(-100, 125, -75, 50), m = m)
#' addmargins(y$mu)
#' sum_net(y$mu)
ipf_net <-
  function(net_tot = NULL,
           m = NULL,
           tol = 1e-05,
           maxit = 500,
           verbose = TRUE) {
    R <- unique(c(dim(m), length(net_tot)))
    if (length(R) != 1)
      stop("The m matrix must be square and with the same dimensions as the length of net total vector (net_tot).")
    if (round(sum(net_tot), 5) != 0)
      message("Convergence will not be obtained as net_tot does not sum to zero.")
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
    pp <- matrix(NA, ncol = R, nrow = 1)
    while (d_max > tol & it < maxit) {
      for(i in 1:R){
        p <- net_param(m = mu, region = i, net_tot = net_tot[i])
        p <- p[p>0]
        if(is.infinite(p) | is.na(p) | is.nan(p))
          p <- 1
        mu <- net_scale(m = mu, region = i, alpha = p)
        pp[i] <- p
      }
      it <- it + 1
      d <- c(sum_net(mu) - net_tot)
      d_max <- max(abs(d))
      if (verbose == TRUE & (it <20 | it %% 10 ==0)){
        cat("iteration:", it, "\n")
        cat("parameters:", pp, "\n")
        cat("max difference:", d_max, "\n")
        cat("\n")
      }
    }
    return(list(mu = mu, it = it, tol = d_max))
  }
