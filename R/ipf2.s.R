#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Origin and Destination Margins and Stripe Elements.
#'
#' The \code{ipf2.b} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ \log y_{pq} = \log \alpha_{p} + \log \beta_{q} + \log \lambda_{ij}I(p \in i, q \in j) + \log m_{pq} }
#' where \eqn{m_{pq}} is a prior estimate for \eqn{y_{pq}} and is no more complex than the matrices being fitted. The \eqn{\lambda_{ij}I(p \in i, q \in j)} term ensures a saturated fit on the block the \eqn{(i,j)} block. 
#'
#' @param rtot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param ctot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param stot Matrix of stripe totals to constrain the sum of the imputed cell blocks. 
#' @param stripe Matrix of stripe stucture corresponding to \code{stot}.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations.
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#' @param ... Additional arguments passes to \code{\link{stripe.matrix}}.
#'
#' @return
#' Iterative Proportional Fitting routine set up using the partial likelihood derivatives. The arguments \code{rtot} and \code{ctot} take the row-table and column-table specific known margins. The \code{stot} take the totals over the stripes in the matrix defined with \code{b}. Diagonal values can be added by the user, but care must be taken to ensure resulting diagonals are feasible given the set of margins. 
#' The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) equal those provided in the row and column totals.
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @author Guy J. Abel
#' @seealso \code{\link{stripe.matrix}}, \code{\link{stripe.matrix}}, \code{\link{block.sum}}
#' 
#' @export
#' @examples
#' y <- ipf2.s(rtot = c(85, 70, 35, 30, 60, 55, 65),
#'  stot = matrix(c(15,20,50,
#'                 35,10,25,
#'                 5 ,0 ,30,
#'                 10,10,10,
#'                 30,30,0,
#'                 15,30,10,
#'                 35,25,5 ), ncol = 3, byrow = TRUE),
#'  stripe = stripe.matrix(x = 1:21, s = c(2,2,3), byrow = TRUE))
#'  addmargins(y$mu)
ipf2.s <- function(rtot = NULL,
                   ctot = NULL,
                   stot = NULL,
                   stripe = NULL,
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
  s_id <- stripe
  byrow <- length(unique(s_id[, 1])) == length(s_id[, 1])
  if (byrow == TRUE)
    s <- table(stripe[1, ])
  if (byrow == FALSE)
    s <- table(stripe[, 1])
  if (byrow == TRUE)
    stot <- t(stot)
  
  n <- list(i = rtot,
            j = ifelse(is.null(ctot), 0, t(ctot)),
            s = c(stot))
  
  #set up offset
  if (is.null(m)) {
    m <- s_id
    m[, ] <- 1
  }
  
  mu <- m
  mu.marg <- n
  m.fact <- n
  it <- 0
  max.diff <- tol * 2
  while (max.diff > tol & it < maxit) {
    if (!is.null(ctot)) {
      mu.marg$j <- apply(mu, 2, sum)
      m.fact$j <- n$j / mu.marg$j
      m.fact$j[is.nan(m.fact$j)] <- 0
      m.fact$j[is.infinite(m.fact$j)] <- 0
      mu <- sweep(mu, 2, m.fact$j, "*")
    }
    if (!is.null(rtot)) {
      mu.marg$i <- apply(mu, 1, sum)
      m.fact$i <- n$i / mu.marg$i
      m.fact$i[is.nan(m.fact$i)] <- 0
      m.fact$i[is.infinite(m.fact$i)] <- 0
      mu <- sweep(mu, 1, m.fact$i, "*")
    }
    if (!is.null(stot)) {
      mu.marg$s <- sapply(1:max(s_id), block.sum, m = mu, bid = s_id)
      m.fact$s <- n$s / mu.marg$s
      m.fact$s[is.nan(m.fact$s)] <- 0
      m.fact$s[is.infinite(m.fact$s)] <- 0
      mu <- mu * stripe.matrix(m.fact$s, s, byrow = byrow)
    }
    
    it <- it + 1
    max.diff <-
      max(abs(c(
        n$i - mu.marg$i, n$j - mu.marg$j, n$s - mu.marg$s
      )))
    if (verbose == TRUE)
      cat(c(it, max.diff), "\n")
  }
  return(list(mu = mu, it = it, tol = max.diff))
}
# rm(n,mu,mu.marg,m.fact,it,max.diff,s_id)
# rm(rtot,ctot,stot,s, byrow, m, maxit,tol,verbose)
