#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Origin and Destination Margins and Block Diagonal Elements.
#'
#' The \code{ipf2.b} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ \log y_{pq} = \log \alpha_{p} + \log \beta_{q} + \log \lambda_{ij}I(p \in i, q \in j) + \log m_{pq} }
#' where \eqn{m_{pq}} is a prior estimate for \eqn{y_{pq}} and is no more complex than the matrices being fitted. The \eqn{\lambda_{ij}I(p \in i, q \in j)} term ensures a saturated fit on the block the \eqn{(i,j)} block. 
#'
#' @param rtot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param ctot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param btot Matrix of block totals to constrain the sum of the imputed cell blocks. 
#' @param block Matrix of block structure corresponding to \code{btot}.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations.
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#' @param ... Additional arguments passes to \code{\link{block_matrix}}.
#'
#' @return
#' Iterative Proportional Fitting routine set up using the partial likelihood derivatives. The arguments \code{rtot} and \code{ctot} take the row-table and column-table specific known margins. The \code{btot} take the totals over the blocks in the matrix defined with \code{b}. Diagonal values can be added by the user, but care must be taken to ensure resulting diagonals are feasible given the set of margins. 
#' 
#' The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) equal those provided in the row and column totals.
#' 
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @author Guy J. Abel
#' @seealso \code{\link{block_matrix}}, \code{\link{stripe_matrix}}, \code{\link{block_sum}}
#' 
#' @export
#' @examples
#' y <- ipf2_block(rtot= c(30,20,30,10,20,5,0,10,5,5,5,10),
#'                 ctot = c(45,10,10,5,5,10,50,5,10,0,0,0),
#'                 btot = matrix(data = c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0),
#'                               nrow = 4, byrow = TRUE),
#'                 block = block_matrix(x = 1:16, b = c(2,3,4,3)))
#' addmargins(y$mu)
# rtot = c(30,20,30,10,20,5,0,10,5,5,5,10)
# ctot = c(45,10,10,5,5,10,50,5,10,0,0,0)
# btot = matrix(c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0), nrow = 4, byrow = TRUE)
ipf2_block <- function(rtot = NULL,
                   ctot = NULL,
                   btot = NULL,
                   block = NULL,
                   m = NULL,
                   tol = 1e-05,
                   maxit = 500,
                   verbose = TRUE,
                   ...) {
  if (sum(!is.null(rtot),!is.null(ctot)) == 2)
    if (any(round(sum(rtot)) != round(sum(ctot))))
      stop(
        "row and column totals are not equal for one or more sub-tables, ensure colSums(rtot)==rowSums(ctot)"
      )
  b_id <- block
  b <- table(block[1, ])
  n <- list(i = rtot,
            j = t(ctot),
            b = c(btot))
  
  #set up offset
  if (is.null(m)) {
    m <- b_id
    m[, ] <- 1
  }
  
  mu <- m
  mu_margin <- n
  mu_scaler <- n
  it <- 0
  d_max <- tol * 2
  while (d_max > tol & it < maxit) {
    if (!is.null(ctot)) {
      mu_margin$j <- apply(X = mu, MARGIN = 2, FUN = sum)
      mu_scaler$j <- n$j / mu_margin$j
      mu_scaler$j[is.nan(mu_scaler$j) | is.infinite(mu_scaler$j)] <- 0
      mu <- sweep(x = mu, MARGIN = 2, STATS = mu_scaler$j, FUN = "*")
    }
    if (!is.null(rtot)) {
      mu_margin$i <- apply(X = mu, MARGIN = 1, FUN = sum)
      mu_scaler$i <- n$i / mu_margin$i
      mu_scaler$i[is.nan(mu_scaler$i) | is.infinite(mu_scaler$i)] <- 0
      mu <- sweep(x = mu, MARGIN = 1, STATS = mu_scaler$i, FUN = "*")
    }
    if (!is.null(btot)) {
      mu_margin$b <- sapply(X = 1:max(b_id), FUN = block_sum, m = mu, block_id = b_id)
      mu_scaler$b <- n$b / mu_margin$b
      mu_scaler$b[is.nan(mu_scaler$b) | is.infinite(mu_scaler$b)] <- 0
      mu <- mu * block_matrix(x = mu_scaler$b, b = b)
    }
    
    it <- it + 1
    d <- c(n$i - mu_margin$i, n$j - mu_margin$j, n$b - mu_margin$b)
    d_max <- max(abs(d))

    if (verbose == TRUE)
      cat(c(it, d_max), "\n")
  }
  return(list(mu = mu, it = it, tol = d_max))
}
#rm(n,mu,mu_margin,mu_scaler,it,d_max,b_id)
#rm(rtot,ctot,btot,b)
