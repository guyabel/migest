\name{ipf2}
\alias{ipf2}
\title{
Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination Migration Flow Table with Known Margins.
}
\description{
The \code{ipf2} function finds the maximum likelihood estimates for fitted values in the log-linear model:
\deqn{\log y_{ij} = \log \alpha_{i} + \log \beta_{j} + \log m_{ij} }
where \eqn{m_{ij}} is a set of prior estimates for \eqn{y_{ij}} and itself is no more complex than the one being fitted. 
}
\usage{
ipf2(rtot = NULL, ctot = NULL, m = matrix(1, length(rtot), length(ctot)), tol = 1e-05, 
        maxit = 500, verbose = FALSE)
}
\arguments{
  \item{rtot}{
Origin (row) totals to constrain indirect estimates to.
}
  \item{ctot}{
Destination (column) totals to constrain indirect estimates to.
}
  \item{m}{
Auxiliary matrix. By default set to 1 for all origin-destination combinations.
}
  \item{tol}{
Tolerance level for parameter estimation.
}
  \item{maxit}{
Maximum number of verboseations for parameter estimation.
}
  \item{verbose}{
Print the parameter estimates at each verboseation. By default \code{TRUE}.
}
}
\details{
Iterative Proportional Fitting routine set up in a similar manner to Agresti (2002, p.343). This is equivalent to a conditional maximisation of the likelihood, as discussed by Willekens (1999), and hence provides identical indirect estimates to those obtained from the \code{\link{cm2}} routine. 

The user must ensure that the row and column totals are equal in sum. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) to equal those provided in the row and column totals.

If only one of the margins is known, the function can still be run. The indirect estimates will correspond to the log-linear model without the \eqn{\alpha_{i}} term if (\code{rtot = NULL}) or without the \eqn{\beta_{j}} term if (\code{ctot = NULL})
}
\value{
Returns a \code{list} object with
  \item{mu }{Origin-Destination matrix of indirect estimates}
  \item{it }{Iteration count}
  \item{tol }{Tolerance level at final verboseation}
}
\references{
Agresti, A. (2002). \emph{Categorical Data Analysis} 2nd edition. Wiley. 

Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of Migration Flows: From Entropy to EM. \emph{Mathematical Population Studies} 7 (3), 239--78.
}
\author{
Guy J. Abel
}
\seealso{
\code{\link{cm2}}, \code{\link{ipf3}}
}
\examples{
## with Willekens (1999) data
dn <- LETTERS[1:2]
y <- ipf2(rtot = c(18, 20), ctot = c(16, 22), m = matrix(c(5, 1, 2, 7), ncol = 2, 
        dimnames = list(orig = dn, dest = dn)))
round(addmargins(y$mu),2)

## with all elements of offset equal
y <- ipf2(rtot = c(18, 20), ctot = c(16, 22))
round(addmargins(y$mu),2)

## with bigger matrix
dn <- LETTERS[1:3]
y <- ipf2(rtot = c(170, 120, 410), ctot = c(500, 140, 60), 
        m = matrix(c(50, 10, 220, 120, 120, 30, 545, 0, 10), ncol = 3, 
        dimnames = list(orig = dn, dest = dn)))
# display with row and col totals
round(addmargins(y$mu))

## only one margin known
dn <- LETTERS[1:2]
y <- ipf2(rtot = c(18, 20), ctot = NULL, m = matrix(c(5, 1, 2, 7), ncol = 2, 
        dimnames = list(orig = dn, dest = dn)))
round(addmargins(y$mu))
}
