#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Origin and Destination Margins and Stripe Elements.
#'
#' The \code{ipf2.b} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ \log y_{pq} = \log \alpha_{p} + \log \beta_{q} + \log \lambda_{ij}I(p \in i, q \in j) + \log m_{pq} }
#' where \eqn{m_{pq}} is a prior estimate for \eqn{y_{pq}} and is no more complex than the matrices being fitted. The \eqn{\lambda_{ij}I(p \in i, q \in j)} term ensures a saturated fit on the block the \eqn{(i,j)} block. 
#'
#' @param row_tot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param col_tot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param stripe_tot Matrix of stripe totals to constrain the sum of the imputed cell blocks. 
#' @param stripe Matrix of stripe stucture corresponding to \code{stripe_tot}.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations.
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#' @param ... Additional arguments passes to \code{\link{stripe_matrix}}.
#'
#' @return
#' Iterative Proportional Fitting routine set up using the partial likelihood derivatives. The arguments \code{row_tot} and \code{col_tot} take the row-table and column-table specific known margins. The \code{stripe_tot} take the totals over the stripes in the matrix defined with \code{b}. Diagonal values can be added by the user, but care must be taken to ensure resulting diagonals are feasible given the set of margins. 
#' The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) equal those provided in the row and column totals.
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @author Guy J. Abel
#' @seealso \code{\link{stripe_matrix}}, \code{\link{block_matrix}}, \code{\link{block_sum}}
#' 
#' @export
#' @examples
#' y <- ipf2_stripe(row_tot = c(85, 70, 35, 30, 60, 55, 65),
#'  stripe_tot = matrix(c(15,20,50,
#'                 35,10,25,
#'                 5 ,0 ,30,
#'                 10,10,10,
#'                 30,30,0,
#'                 15,30,10,
#'                 35,25,5 ), ncol = 3, byrow = TRUE),
#'  stripe = stripe_matrix(x = 1:21, s = c(2,2,3), byrow = TRUE))
#'  addmargins(y$mu)
ipf2_stripe <- function(row_tot = NULL,
                   col_tot = NULL,
                   stripe_tot = NULL,
                   stripe = NULL,
                   m = NULL,
                   tol = 1e-05,
                   maxit = 500,
                   verbose = TRUE,
                   ...) {
  if (sum(!is.null(row_tot),!is.null(col_tot)) == 2)
    if (any(round(sum(row_tot)) != round(sum(col_tot))))
      stop(
        "row and column totals are not equal for one or more sub-tables, ensure colSums(row_tot)==rowSums(col_tot)"
      )
  s_id <- stripe
  byrow <- length(unique(s_id[, 1])) == length(s_id[, 1])
  if (byrow == TRUE)
    s <- table(stripe[1, ])
  if (byrow == FALSE)
    s <- table(stripe[, 1])
  if (byrow == TRUE)
    stripe_tot <- t(stripe_tot)
  
  # n <- list(i = row_tot,
  #           j = ifelse(is.null(col_tot), 0, t(col_tot)),
  #           s = c(stripe_tot))
  n <- list(i = row_tot,
            j = col_tot,
            s = c(stripe_tot))
  
  #set up offset
  if (is.null(m)) {
    m <- s_id
    m[, ] <- 1
  }
  
  mu <- m
  mu_margin <- n
  mu_scaler <- n
  it <- 0
  d_max <- tol * 2
  while (d_max > tol & it < maxit) {
    if (!is.null(col_tot)) {
      mu_margin$j <- apply(X = mu, MARGIN = 2, FUN = sum)
      mu_scaler$j <- n$j / mu_margin$j
      mu_scaler$j[is.nan(mu_scaler$j) | is.infinite(mu_scaler$j)] <- 0
      mu <- sweep(x = mu, MARGIN = 2, STATS = mu_scaler$j, FUN = "*")
    }
    if (!is.null(row_tot)) {
      mu_margin$i <- apply(X = mu, MARGIN = 1, FUN = sum)
      mu_scaler$i <- n$i / mu_margin$i
      mu_scaler$i[is.nan(mu_scaler$i) | is.infinite(mu_scaler$i)] <- 0
      mu <- sweep(x = mu, MARGIN = 1, STATS = mu_scaler$i, FUN = "*")
    }
    if (!is.null(stripe_tot)) {
      mu_margin$s <- sapply(X = 1:max(s_id), FUN = block_sum, m = mu, block_id = s_id)
      mu_scaler$s <- n$s / mu_margin$s
      mu_scaler$s[is.nan(mu_scaler$s) | is.infinite(mu_scaler$s)] <- 0
      mu <- mu * stripe_matrix(x = mu_scaler$s, s = s, byrow = byrow)
    }
    
    it <- it + 1
    d <- c(n$i - mu_margin$i, n$j - mu_margin$j, n$s - mu_margin$s)
    d_max <- max(abs(d))

    if (verbose == TRUE)
      cat(c(it, d_max), "\n")
  }
  return(list(mu = mu, it = it, tol = d_max))
}
# rm(n,mu,mu_margin,mu_scaler,it,d_max,s_id)
# rm(row_tot,col_tot,stripe_tot,s, byrow, m, maxit,tol,verbose)
