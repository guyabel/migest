#' Conditional Maximisation Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Net Migration Totals.
#'
#' The \code{cm_net} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{\log y_{ij} = \log \alpha_{i} + \log \alpha_{i}^{-1} + \log m_{ij} }
#' 
#' @param net_tot Vector of net migration totals to constrain the sum of the imputed cell columns. Elements must sum to zero.
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant typologies combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Conditional maximisation routine set up using the partial likelihood derivatives. The argument \code{net_tot} takes the known net migration totals.
#' The user must ensure that the net migration totals sum globally to zero.
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
#' # m[lower.tri(m)] <- t(m)[lower.tri(m)]
#' addmargins(m)
#' sum_net(m)
#' 
#' y <- cm_net(net_tot = c(30, 40, -15, -55), m = m)
#' addmargins(y$n)
#' sum_net(y$n)
#' 
#' m <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
#'             nrow = 4, ncol = 4, byrow = TRUE,
#'             dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
#' addmargins(m)
#' sum_net(m)
#' 
#' y <- cm_net(net_tot = c(-100, 125, -75, 50), m = m)
#' addmargins(y$n)
#' sum_net(y$n)
cm_net <- function(net_tot = NULL, m = NULL, tol = 1e-06, maxit = 500, verbose = TRUE,
                   alpha0 = rep(1, length(net_tot))) {
   # net_tot = c(-100, 125, -75, 50); m = NULL; tol = 1e-06;  maxit = 500; verbose = TRUE
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
   it <- 0;  
   d_max <- tol * 2
   mm <- m
   
   while (d_max > tol & it < maxit) {
      if (verbose == TRUE & (it < 20 | it %% 10 == 0)){
         cat("iteration:", it, "\n")
         cat("alpha parameters:", alpha, "\n")
         cat("\n")
      }
      alpha_old <- alpha
      for(i in 1:R){
         # mm <- (1/alpha) %*% t(alpha) * m
         mm[,i] <- 1/alpha * m[,i]
         mm[i,] <- alpha * m[i,]
         
         # print(round(addmargins(mm),1))
         # print(migest::sum_net(mm))
         # print(net_tot)
         emi_tot <- apply(X = mm, MARGIN = 1, FUN = "sum")
         imm_tot <- apply(X = mm, MARGIN = 2, FUN = "sum")
         
         p <- quadratic_eqn(a = imm_tot[i], b = -net_tot[i], c = -emi_tot[i])
         p <- p[p>0]
         if(is.infinite(p) | is.na(p) | is.nan(p))
            p <- 1
         alpha[i] <- p
      }
      d_max <- max(abs(alpha_old - alpha))
      it <- it + 1
   }
   return(list(n = (1/alpha) %*% t(alpha) * m, 
               theta = c(alpha = alpha)))
}
