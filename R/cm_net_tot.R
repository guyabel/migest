#' Conditional maximization routine for the indirect estimation of origin-destination-type migration flow tables with known net migration and grand totals.
#'
#' The \code{cm_net} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{\log y_{ij} = \log \alpha_{i} + \log \alpha_{i}^{-1} + \log m_{ij} }
#' 
#' @param net_tot Vector of net migration totals to constrain the sum of the imputed cell row and columns. Elements must sum to zero.
#' @param tot Numeric value of grand total to constrain sum of all imputed cells.
#' @param m Array of auxiliary data. By default, set to 1 for all origin-destination-migrant typologies combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#' @param alpha0 Vector of initial estimates for alpha
#' @param lambda0 Numeric value of initial estimates for lambda
#' @param alpha_constrained Logical value to indicate if the first alpha should be constrain to unity. By default \code{TRUE}
#'
#' @return
#' Conditional maximisation routine set up using the partial likelihood derivatives. The argument \code{net_tot} takes the known net migration totals.
#' The user must ensure that the net migration totals sum globally to zero.
#' 
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @author Guy J. Abel, Peter W. F. Smith
#' @export
#'
#' @examples
#' m <- matrix(data = 1:16, nrow = 4)
#' # m[lower.tri(m)] <- t(m)[lower.tri(m)]
#' addmargins(m)
#' sum_net(m)
#' 
#' y <- cm_net_tot(net_tot = c(30, 40, -15, -55), tot = 200, m = m)
#' addmargins(y$n)
#' sum_net(y$n)
#' 
#' m <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
#'             nrow = 4, ncol = 4, byrow = TRUE,
#'             dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
#' addmargins(m)
#' sum_net(m)
#' 
#' y <- cm_net_tot(net_tot = c(-100, 125, -75, 50), tot = 600, m = m)
#' addmargins(y$n)
#' sum_net(y$n)
cm_net_tot <- function(net_tot = NULL, tot = NULL, m = NULL, tol = 1e-06, maxit = 500, verbose = TRUE,
                   alpha0 = rep(1, length(net_tot)), lambda0 = 1,
                   alpha_constrained = TRUE) {
  # net_tot = c(-100, 125, -75, 50); m = NULL; tol = 1e-06;  maxit = 500; verbose = TRUE
  # m = matrix(data = 1:16, nrow = 4); net_tot = c(30, 40, -15, -55); tol = 1e-06;  maxit = 500; verbose = TRUE; alpha0 = rep(1, length(net_tot)); lambda0 = 1
  R <- unique(c(dim(m), length(net_tot)))
  if (length(R) != 1)
    stop("The m matrix must be square and with the same dimensions as the length of net total vector (net_tot).")
  if (sum(net_tot) %% 1 > tol)
    message("Convergence will not be obtained as net_tot does not sum to zero.")
  
  #set up offset
  if (is.null(m))
    m <- matrix(1, nrow = R, ncol = R)
  if (is.null(dimnames(m)))
    dimnames(m) <- list(orig = LETTERS[1:R], dest = LETTERS[1:R])
  
  alpha <- alpha0
  lambda <- lambda0
  theta <- c(alpha, lambda)
  it <- 0;  
  d_max <- tol * 2
  
  ii <- 1
  if(alpha_constrained)
    ii <- 2
  
  while (d_max > tol & it < maxit) {
    if (verbose == TRUE & (it < 20 | it %% 10 == 0)){
      cat("iteration:", it, "\n")
      cat("alpha parameters:", alpha, "\n")
      cat("lambda parameter:", lambda, "\n")
      cat("\n")
    }
    alpha_old <- alpha
    lambda_old <- lambda
    theta_old <- c(alpha_old, lambda_old)
    
    for(i in ii:R){
      # i 
      # j = 3
      # (-net_tot[i] + 
      #    sqrt (net_tot[i]^2 + 4 * sum(1/aa[, (j-1)] * ll[(j-1)] * m[i,]) * sum(aa[,(j-1)] * ll[(j-1)] * m[,i]))) / (2 * sum(1/aa[, (j-1)] * ll[(j-1)] * m[i,])) 
      p <- quadratic_eqn(a = sum(1/alpha_old * m[i,] * lambda_old), 
                         b = net_tot[i], 
                         c = -sum(alpha_old * m[,i] * lambda_old))
      p <- p[p>0]
      if(is.infinite(p) | is.na(p) | is.nan(p))
        p <- 1
      alpha[i] <- p
    }
    f0 <- diag(alpha_old) %*% m %*% diag(1/alpha_old)
    lambda <- tot / sum(f0)
    theta <- c(alpha, lambda)
    d_max <- max(abs(theta_old - theta))
    it <- it + 1
  }
  f <- lambda * diag(alpha) %*% m %*% diag(1/alpha)
  dimnames(f) <- dimnames(m)
  return(list(n = f, theta = c(alpha = alpha, lambda = lambda)))
}
