#' Conditional Maximization Routine for the Indirect Estimation of Origin-Destination-Migrant Type Migration Flow Tables with Known Origin and Destination Margins.
#'
#' The \code{cm3} function finds the maximum likelihood estimates for parameters in the log-linear model:
#' \deqn{ \log y_{ijk} = \log \alpha_{i} + \log \beta_{j} + \log m_{ijk} }
#' as introduced by Abel (2005). The \eqn{\alpha_{i}} and  \eqn{\beta_{j}} represent background information related to  the characteristics of the origin and destinations respectively. The \eqn{m_{ijk}} factor represents auxiliary information on origin-destination migration flows by a migrant characteristic (such as age, sex, disability, household type, economic status, etc.). This method is useful for combining data from detailed data collection processes (such as a Census) with more up-to-date information on migration inflows and outflows (where details on movements by migrant characteristics are not known).
#' @param row_tot Vector of origin totals to constrain the sum of the imputed cell rows.
#' @param col_tot Vector of destination totals to constrain the sum of the imputed cell columns.
#' @param m Array of auxiliary data. By default set to 1 for all origin-destination-migrant typology combinations. 
#' @param tol Numeric value for the tolerance level used in the parameter estimation.
#' @param maxit Numeric value for the maximum number of iterations used in the parameter estimation.
#' @param verbose Logical value to indicate the print the parameter estimates at each iteration. By default \code{FALSE}.
#'
#' @return
#' Parameter estimates were obtained using the conditional maximization of the likelihood, as discussed by Abel (2005) and Raymer et. al. (2007). 
#' 
#' The user must ensure that the row and column totals are equal in sum. Care must also be taken to allow the row and column dimension of the auxiliary matrix (\code{m}) to equal those provided in the row and column totals.
#' 
#' Returns a \code{list} object with
#' \item{N }{Origin-Destination matrix of indirect estimates}
#' \item{theta }{Collection of parameter estimates}
#' @references 
#' Abel, G. J. (2005) \emph{The Indirect Estimation of Elderly Migrant Flows in England and Wales} (MS.c. Thesis). University of Southampton
#' 
#' Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and registration data to estimate detailed elderly migration flows in England and Wales. \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)} 170 (4), 891--908.
#' @author Guy J. Abel
#' @seealso \code{\link{cm2}}, \code{\link{ipf3}}
#' @export
#'
#' @examples
#' ## over two tables
#' dn <- LETTERS[1:2]
#' y <- cm3(row_tot = c(18, 20) * 2, col_tot = c(16, 22) * 2, 
#'          m = array(c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2), 
#'                    dimnames = list(orig = dn, dest = dn, type = c("ILL", "HEALTHY"))))
#' # display with row, col and table totals
#' y
#' 
#' ## over three tables
#' y <- cm3(row_tot = c(170, 120, 410), col_tot = c(500, 140, 60), 
#'          m = array(c(5, 1, 2, 7, 4, 2, 5, 9, 5, 4, 3, 1), dim = c(2, 2, 3), 
#'                    dimnames = list(orig = dn, dest = dn, type = c("0--15", "15-60", ">60"))),
#'          verbose = FALSE)
#' # display with row, col and table totals
#' y
cm3 <- function(row_tot=NULL,col_tot=NULL,m,tol=1e-05,maxit=500,verbose=FALSE)
{
  if(round(sum(row_tot))!=round(sum(col_tot)))
    stop("row and column totals are not equal, ensure sum(row_tot)==sum(col_tot)")
  i<-dim(m)[1]
  j<-dim(m)[2]
  alpha <- rep(1,i)
  beta <- rep(1,j)
  if(verbose==TRUE){
    rd <- paste0("%.",
                 nchar(x = format(x = tol, scientific=FALSE))-2,
                 "f")
    cat(sprintf(rd,c(alpha,beta)), fill = TRUE)
  }
  alpha.old <- alpha+1 
  beta.old <- beta+1
  it <- 1
  d_max <- tol*2
  while(d_max > tol & it < maxit){
    beta.old <- beta
    for(j in 1:j) {
      beta[j] <- col_tot[j]/colSums(x = alpha * apply(X = m, MARGIN = c(1,2), FUN = sum))[j]
    }
    alpha.old <- alpha
    for(i in 1:i) {
      alpha[i] <- row_tot[i]/colSums(x = beta * apply(X = m, MARGIN = c(2,1), FUN = sum))[i]
    }
    it<-it+1
    d <- c(alpha-alpha.old, beta-beta.old)
    d_max <- max(abs(d))
    if(verbose==TRUE)
      cat(sprintf(rd,c(alpha,beta)), fill = TRUE)
  }
  return(
    list(N = c(alpha %*% t(beta)) * m,
         theta = c(mu = 1, alpha = alpha, beta = beta))
  )
}