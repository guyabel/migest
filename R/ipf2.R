#' Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination Migration Flow Table with Known Margins.
#'
#' The \code{ipf2} function finds the maximum likelihood estimates for fitted values in the log-linear model:
#' \deqn{\log y_{ij} = \log \alpha_{i} + \log \beta_{j} + \log m_{ij} }
#' where \eqn{m_{ij}} is a set of prior estimates for \eqn{y_{ij}} and itself is no more complex than the one being fitted. 
#' @param rtot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param ctot Vector of destination totals to constrain the sum of the imputed cell columns.
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
#' If only one of the margins is known, the function can still be run. The indirect estimates will correspond to the log-linear model without the \eqn{\alpha_{i}} term if (\code{rtot = NULL}) or without the \eqn{\beta_{j}} term if (\code{ctot = NULL})
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
#' y <- ipf2(rtot = c(18, 20), ctot = c(16, 22), 
#'           m = matrix(c(5, 1, 2, 7), ncol = 2, 
#'                      dimnames = list(orig = dn, dest = dn)))
#' round(addmargins(y$mu),2)
#' 
#' ## with all elements of offset equal
#' y <- ipf2(rtot = c(18, 20), ctot = c(16, 22))
#' round(addmargins(y$mu),2)
#' 
#' ## with bigger matrix
#' dn <- LETTERS[1:3]
#' y <- ipf2(rtot = c(170, 120, 410), ctot = c(500, 140, 60), 
#'           m = matrix(c(50, 10, 220, 120, 120, 30, 545, 0, 10), ncol = 3, 
#'                      dimnames = list(orig = dn, dest = dn)))
#' # display with row and col totals
#' round(addmargins(y$mu))
#' 
#' ## only one margin known
#' dn <- LETTERS[1:2]
#' y <- ipf2(rtot = c(18, 20), ctot = NULL, 
#'           m = matrix(c(5, 1, 2, 7), ncol = 2, 
#'                      dimnames = list(orig = dn, dest = dn)))
#' round(addmargins(y$mu))
#ipf (rather than cm)...gives different estimates
ipf2 <- function(rtot=NULL,ctot=NULL,m=matrix(1,length(rtot),length(ctot)),tol=1e-05,maxit=500,verbose=FALSE){
  if(!is.null(rtot) & !is.null(ctot))
    if(round(sum(rtot))!=round(sum(ctot))) 
      stop("row and column totals are not equal, ensure sum(rtot)==sum(ctot)")
  n<-list(i=rtot,
          j=ctot)
  mu<-m
  mu.marg<-n
  m.fact<-n
  if(verbose==TRUE)
    rd<-paste("%.",nchar(format(tol,scientific=FALSE))-2,"f",sep="")
  it<-0; max.diff<-tol*2
  while(it==0 | max.diff>tol & it<maxit ){
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
    it<-it+1
    #max.diff<-max(abs(unlist(n)-unlist(mu.marg)))
    #speeds up a lot if get rid of unlist (new to v1.7)
    max.diff<-max(abs(c(n$i-mu.marg$i, n$j-mu.marg$j)))
    
    if(verbose==TRUE)
      cat(sprintf(rd,unlist(m.fact)), fill = TRUE)
  }
  return(list(mu=mu,it=it,tol=max(abs(unlist(n)-unlist(mu.marg)))))
}
