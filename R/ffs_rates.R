#' Estimation of Bilateral Migrant Flows from Bilateral Migrant Stocks Using Rates Approaches
#'
#' Estimates migrant transitions flows between two sequential migrant stock tables using approached based on rates.
#' @param m1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param m2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param M Numeric value for the global sum of migration flows, used for \code{dennett} approach.
#' @param method Method to estimate flows. Can take values \code{dennett} or \code{rogers-von-rabenau}. See detials section for more information. Uses \code{dennett} as default. 
#' 
#' @return Estimates migrant transitions flows based on migration rates.
#' 
#' When \code{method = "dennett"} migration are derived from the matrix supplied to \code{m1}. Dennett uses bilateral migrant stocks at begining of period. Rates then multiplied by global migration flows supplied in \code{M}.
#' 
#' When \code{method = "rogers-von-rabenau"} a matrix of growth rates are derived from the changes in inital poplations stock \code{m1} to obtain \code{m2};
#' \deqn{P^{t+1} = g P^{t}}
#' and then multiplied by the corresponding populations at risk in \code{m1}. Can result in negative flows.
#' 
#' #' @references 
#' Dennett, A. (2015). Estimating an Annual Time Series of Global Migration Flows - An Alternative Methodology for Using Migrant Stock Data. \emph{Global Dynamics: Approaches from Complexity Science}, 125–142. https://doi.org/10.1002/9781118937464.ch7
#' 
#' Rogers, A., & Von Rabenau, B. (1971). Estimation of interregional migration streams from place-of-birth-by-residence data. \emph{Demography}, 8(2), 185–194.
#' 
#' @author Guy J. Abel
#' @seealso \code{\link{ffs_demo}}, \code{\link{ffs_rates}}
#' @export
#'
#' @examples
#' s1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' s2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
#'              nrow = 4, ncol = 4, byrow = TRUE)
#' reg <- LETTERS[1:4]
#' dimnames(s1) <- dimnames(s2) <- list(pob = reg, por = reg)
#' s1; s2
#' 
#' # calculate total migration flows for dennett approach
#' n <- colSums(s2) - colSums(s1)
#' 
#' ffs_rates(m1 = s1, M =  sum(abs(n)), method = "dennett" )
#' ffs_rates(m1 = s1, m2 = s2, method = "rogers-von-rabenau" )
ffs_rates <- function(m1 = NULL, m2 = NULL, M = NULL, method = "dennett"){
  if(method == "dennett"){
    P <- m1
    diag(P) <- 0
    # set up flow probabilities
    PS <- P/sum(P)
    # calculate flow
    x <- M * PS
  }
  if(method == "rogers-von-rabenau"){
    g <- solve(a = m1, b = m2)
    pop_expose <- matrix(colSums(m1), nrow = nrow(m1), ncol = ncol(m1))
    x <- g * pop_expose
    diag(x) <- 0
  }
  dn <- dimnames(m1)
  names(dn) <- c("orig", "dest")
  dimnames(x) <- dn
  return(x)
}
