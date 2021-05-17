#' Conditional maximization routine for the indirect estimation of origin-destination migration flow table with known margins
#'
#' The \code{cm2} function finds the maximum likelihood estimates for parameters in the log-linear model:
#' \deqn{ \log y_{ij} = \log \alpha_i + \log \beta_j + \log m_{ij} }
#' as introduced by Willekens (1999). The \eqn{\alpha_i} and  \eqn{\beta_j} represent background information related to  the characteristics of the origin and destinations respectively. The \eqn{m_{ij}} factor represents auxiliary information on migration flows, which imposes its interaction structure onto the estimated flow matrix.
#' @param row_tot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param col_tot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#' @param rtot Depreciated. Use \code{row_tot}
#' @param ctot Depreciated. Use \code{col_tot}
#'
#' @return
#' Parameter estimates are obtained using the EM algorithm outlined in Willekens (1999). This is equivalent to a conditional maximization of the likelihood, as discussed by Raymer et. al. (2007). It also provides identical indirect estimates to those obtained from the \code{\link{ipf2}} routine. 
#' 
#' The user must ensure that the row and column totals are equal in sum. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) to equal those provided in the row (\code{row_tot}) and column (\code{col_tot}) arguments.
#' 
#' Returns a \code{list} object with
#' \item{N }{Origin-Destination matrix of indirect estimates}
#' \item{theta }{Collection of parameter estimates}
#' @references 
#' Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and registration data to estimate detailed elderly migration flows in England and Wales. \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)} 170 (4), 891--908.
#' 
#' Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of Migration Flows: From Entropy to EM. \emph{Mathematical Population Studies} 7 (3), 239--78.
#' @author Guy J. Abel
#' @seealso \code{\link{ipf2}}
#' @export
#'
#' @examples
#' ## with Willekens (1999) data
#' dn <- LETTERS[1:2]
#' y <- cm2(row_tot = c(18, 20), col_tot = c(16, 22), 
#'          m = matrix(c(5, 1, 2, 7), ncol = 2, dimnames = list(orig = dn, dest = dn)))
#' y
#' 
#' ## with all elements of offset equal (independence fit)
#' y <- cm2(row_tot = c(18, 20), col_tot = c(16, 22))
#' y
#' 
#' ## with bigger matrix
#' dn <- LETTERS[1:4]
#' y <- cm2(row_tot = c(250, 100, 140, 110), col_tot = c(150, 150, 180, 120),
#'          m = matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
#'                     nrow = 4, ncol = 4, dimnames = list(orig = dn, dest = dn), byrow = TRUE))
#'                     
#' # display with row and col totals
#' round(addmargins(y$n)) 
cm2 <- function(row_tot = NULL, col_tot = NULL, 
                m = matrix(data = 1, nrow = length(row_tot), ncol = length(col_tot)),
                tol = 1e-06, maxit = 500, verbose = TRUE, 
                rtot = row_tot, ctot = col_tot){
  if(round(sum(row_tot)) != round(sum(col_tot))) 
    stop("row and column totals are not equal, ensure sum(row_tot)==sum(col_tot)")
  
  i <- dim(m)[1];  
  j <- dim(m)[2]
  alpha <- rep(x = 1, times = i)
  beta <- rep(x = 1, times = j)
  if (verbose == TRUE){
    cat("iteration:", 0, "\n")
    cat("alpha parameters:", "\n")
    cat("beta parameters:", beta, "\n")
    cat("\n")
  }
  
  it <- 1;  
  d_max <- tol * 2

  while(d_max > tol & it < maxit){
    alpha_old <- alpha
    for(i in 1:i) {
      alpha[i] <- row_tot[i]/sum(beta * m[i,  ])
    }
    beta_old <- beta
    for(j in 1:j) {
      beta[j] <- col_tot[j]/sum(alpha * m[, j])
    }
    d_max <- max(abs(c(alpha_old - alpha, beta_old - beta)))
    if (verbose == TRUE & (it < 20 | it %% 10 ==0)){
      cat("iteration:", it, "\n")
      cat("alpha parameters:", alpha, "\n")
      cat("beta parameters:", beta, "\n")
      cat("max difference:", d_max, "\n")
      cat("\n")
    }
    it <- it+1
  }
  return(
    list(n = alpha %*% t(beta)*m, 
         theta = c(alpha = alpha, beta = beta))
  )
}
