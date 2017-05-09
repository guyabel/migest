#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Origin and Destination Margins and Block Diagonal Elements.
#'
#' The \code{ipf2.b} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{ \log y_{pq} = \log \alpha_{p} + \log \beta_{q} + \log \lambda_{ij}I(p \in i, q \in j) + \log m_{pq} }
#' where \eqn{m_{pq}} is a prior estimate for \eqn{y_{pq}} and is no more complex than the matrices being fitted. The \eqn{\lambda_{ij}I(p \in i, q \in j)} term ensures a saturated fit on the block the \eqn{(i,j)} block. 
#'
#' @param rtot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param ctot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param btot Matrix of block totals to constrain the sum of the imputed cell blocks. 
#' @param blocks Matrix of block structure corresponding to \code{btot}.
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant type combinations.
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#' @param ... Additional arguments passes to \code{\link{block.matrix}}.
#'
#' @return
#' Iterative Proportional Fitting routine set up using the partial likelihood derivatives. The arguments \code{rtot} and \code{ctot} take the row-table and column-table specific known margins. The \code{btot} take the totals over the blocks in the matrix defined with \code{b}. Diagonal values can be added by the user, but care must be taken to ensure resulting diagonals are feasible given the set of margins. 
#' The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) equal those provided in the row and column totals.
#' Returns a \code{list} object with
#' \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @author Guy J. Abel
#' @seealso \code{\link{block.matrix}}, \code{\link{stripe.matrix}}, \code{\link{block.sum}}
#' 
#' @export
y <- ipf2.b(rtot= c(30,20,30,10,20,5,0,10,5,5,5,10),
            ctot = c(45,10,10,5,5,10,50,5,10,0,0,0),
            btot = matrix(c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0), nrow = 4, byrow = TRUE),
            b = c(2,3,4,3))
addmargins(y$mu)
#' @examples
# rtot = c(30,20,30,10,20,5,0,10,5,5,5,10)
# ctot = c(45,10,10,5,5,10,50,5,10,0,0,0)
# btot = matrix(c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0), nrow = 4, byrow = TRUE)
# b = c(2,3,4,3)
ipf2.b<-function(rtot = NULL, ctot = NULL, btot = NULL,
                 b = NULL, m = NULL,
                 tol = 1e-05, maxit = 500, verbose=TRUE, ...){
  if(sum(!is.null(rtot), !is.null(ctot)) == 2)
    if(any(round(sum(rtot))!=round(sum(ctot))))
      stop("row and column totals are not equal for one or more sub-tables, ensure colSums(rtot)==rowSums(ctot)")
  b_id <- block.matrix(x = 1:(nrow(btot)*ncol(btot)), b = b, ...)
  n<-list(i = rtot,
          j = t(ctot),
          b = c(btot))
  
  #set up offset
  if(is.null(m)){
    m <- b_id
    m[,] <- 1
  }

  mu <- m
  mu.marg <- n
  m.fact <- n
  it <- 0; max.diff <- tol*2
  while(max.diff>tol & it<maxit){
    if(!is.null(ctot)){
      mu.marg$j <- apply(mu,2,sum)
      m.fact$j <- n$j/mu.marg$j
      m.fact$j[is.nan(m.fact$j)]<-0
      m.fact$j[is.infinite(m.fact$j)]<-0
      mu <- sweep(mu, 2, m.fact$j, "*")
    }
    if(!is.null(rtot)){
      mu.marg$i <- apply(mu,1,sum)
      m.fact$i <- n$i/mu.marg$i
      m.fact$i[is.nan(m.fact$i)]<-0
      m.fact$i[is.infinite(m.fact$i)]<-0
      mu <- sweep(mu, 1, m.fact$i, "*")
    }
    if(!is.null(btot)){
      mu.marg$b <- sapply(1:max(b_id), block.sum, m = mu, bid = b_id)
      m.fact$b <- n$b/mu.marg$b
      m.fact$b[is.nan(m.fact$b)]<-0
      m.fact$b[is.infinite(m.fact$b)]<-0
      
      mu <- mu*block.matrix(m.fact$b, b)
    }
    
    it<-it+1
    max.diff<-max(abs(c(n$i-mu.marg$i, n$j-mu.marg$j, n$b-mu.marg$b)))
    if(verbose==TRUE)
      cat(c(it, max.diff), "\n")
  }
  return(list(mu=mu,it=it,tol=max.diff))
}
#rm(n,mu,mu.marg,m.fact,it,max.diff,b_id)
#rm(rtot,ctot,btot,b)


