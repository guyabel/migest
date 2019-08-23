#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination Migration Flow Table with Known Margins.
#'
#' The \code{ipf2} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{\log y_{ij} = \log \alpha_{i} + \log \beta_{j} + \log m_{ij} }
#' where \eqn{m_{ij}} is a set of prior estimates for \eqn{y_{ij}} and itself is no more complex than the one being fitted. 
#' @param row_tot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param col_tot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Iterative Proportional Fitting routine set up in a similar manner to Agresti (2002, p.343). This is equivalent to a conditional maximization of the likelihood, as discussed by Willekens (1999), and hence provides identical indirect estimates to those obtained from the \code{\link{cm2}} routine. 
#' 
#' The user must ensure that the row and column totals are equal in sum. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) to equal those provided in the row and column totals.
#' 
#' If only one of the margins is known, the function can still be run. The indirect estimates will correspond to the log-linear model without the \eqn{\alpha_{i}} term if (\code{row_tot = NULL}) or without the \eqn{\beta_{j}} term if (\code{col_tot = NULL})
#' 
#' Returns a \code{list} object with
#' \item{mu }{Origin-Destination matrix of indirect estimates}
#' \item{it }{Iteration count}
#' \item{tol }{Tolerance level at final iteration}
#' @references 
#' Agresti, A. (2002). \emph{Categorical Data Analysis} 2nd edition. Wiley. 
#' 
#' Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of Migration Flows: From Entropy to EM. \emph{Mathematical Population Studies} 7 (3), 239--78.
#' @author Guy J. Abel
#' @seealso \code{\link{cm2}}, \code{\link{ipf3}}
#' @export
#'
#' @examples
#' ## with Willekens (1999) data
#' dn <- LETTERS[1:2]
#' y <- ipf2(row_tot = c(18, 20), col_tot = c(16, 22), 
#'           m = matrix(c(5, 1, 2, 7), ncol = 2, 
#'                      dimnames = list(orig = dn, dest = dn)))
#' round(addmargins(y$mu),2)
#' 
#' ## with all elements of offset equal
#' y <- ipf2(row_tot = c(18, 20), col_tot = c(16, 22))
#' round(addmargins(y$mu),2)
#' 
#' ## with bigger matrix
#' dn <- LETTERS[1:3]
#' y <- ipf2(row_tot = c(170, 120, 410), col_tot = c(500, 140, 60), 
#'           m = matrix(c(50, 10, 220, 120, 120, 30, 545, 0, 10), ncol = 3, 
#'                      dimnames = list(orig = dn, dest = dn)))
#' # display with row and col totals
#' round(addmargins(y$mu))
#' 
#' ## only one margin known
#' dn <- LETTERS[1:2]
#' y <- ipf2(row_tot = c(18, 20), col_tot = NULL, 
#'           m = matrix(c(5, 1, 2, 7), ncol = 2, 
#'                      dimnames = list(orig = dn, dest = dn)))
#' round(addmargins(y$mu))
#ipf (rather than cm)...gives different estimates
ipf2 <- function(row_tot = NULL, col_tot = NULL, 
                 m = matrix(1,length(row_tot),length(col_tot)),
                 tol = 1e-05, maxit=500, verbose=FALSE){
  if(!is.null(row_tot) & !is.null(col_tot))
    if(round(sum(row_tot))!=round(sum(col_tot))) 
      stop("row and column totals are not equal, ensure sum(row_tot)==sum(col_tot)")
  n <- list(i = row_tot, j = col_tot)
  mu <- m
  mu_margin <- n
  mu_scaler <- n
  if(verbose==TRUE)
    rd <- paste("%.",nchar(format(tol,scientific=FALSE))-2,"f",sep="")
  it <- 0; d_max <- tol*2
  while(it==0 | d_max>tol & it<maxit ){
    if(!is.null(col_tot)){
      mu_margin$j <- apply(X = mu, MARGIN = 2, FUN = sum)
      mu_scaler$j <- n$j/mu_margin$j
      mu_scaler$j[is.nan(mu_scaler$j) | is.infinite(mu_scaler$j)]<-0
      mu <- sweep(x = mu, MARGIN = 2, STATS = mu_scaler$j, FUN = "*")
    }
    if(!is.null(row_tot)){
      mu_margin$i <- apply(X = mu, MARGIN = 1, FUN = sum)
      mu_scaler$i <- n$i/mu_margin$i
      mu_scaler$i[is.nan(mu_scaler$i) | is.infinite(mu_scaler$i)]<-0
      mu <- sweep(x = mu, MARGIN = 1, STATS = mu_scaler$i, FUN = "*")
    }
    it <- it+1
    #speeds up a lot if get rid of unlist (new to v1.7)
    #d_max<-max(abs(unlist(n)-unlist(mu_marg)))
    d <- c(n$i-mu_margin$i, n$j-mu_margin$j)
    d_max <- max(abs(d))
    
    if(verbose==TRUE)
      cat(sprintf(rd,unlist(mu_scaler)), fill = TRUE)
  }
  r <- list(mu=mu, it=it, tol=d_max)
  return(r)
}
