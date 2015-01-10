\name{ipf3.qi}
\alias{ipf3.qi}
\title{
Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Migrant Type Migration Flow Tables with Known Origin and Destination Margins and Diagonal Elements.
}
\description{
This function is predominantly intended to be used within the \code{\link{ffs}} routine.

The \code{ipf3} function finds the maximum likelihood estimates for fitted values in the log-linear model:
\deqn{ \log y_{ijk} = \log \alpha_{i} + \log \beta_{j} + \log \lambda_{k} + \log \gamma_{ik} + \log \kappa_{jk} + \log \delta_{ijk}I(i=j) + \log m_{ijk} }
where \eqn{m_{ijk}} is a set of prior estimates for \eqn{y_{ijk}} and is no more complex than the matrices being fitted. The \eqn{\delta_{ijk}I(i=j)} term ensures a saturated fit on the diagonal elements of each \eqn{(i,j)} matrix.
}
\usage{
ipf3.qi(rtot = NULL, ctot = NULL, dtot = NULL, m = NULL, speed = TRUE, tol = 1e-05, 
        maxit = 500, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rtot}{
Matrix of origin totals (by migrant characteristic) to constrain indirect estimates to. Row of matrix corresponds to origin and column of table corresponds to migrant type/characteristic.
}
  \item{ctot}{
Matrix of destination totals (by migrant characteristic) to constrain indirect estimates to. Row of matrix corresponds to destination and column of table corresponds to migrant type/characteristic.
}
  \item{dtot}{
Array with counts on diagonal to constrain diagonal elements of the indirect estimates too. By default these are taken as their maximum possible values given the relevant margins totals in each table. If user specifies their own array of diagonal totals, values on the non-diagonals in the array can take any positive number (they are ultimately ignored).
}
  \item{m}{
Array of auxiliary data. By default set to 1 for all origin-destination-migrant type combinations.
}
  \item{speed}{
Speeds up the IPF algorithm by minimising sufficient statistics.
}
  \item{tol}{
Tolerance level for parameter estimation.
}
  \item{maxit}{
Maximum number of verboseations for parameter estimation.
}
  \item{verbose}{
Print the parameter estimates at each verboseation. By default \code{FALSE}.
}
}
\details{
Iterative Proportional Fitting routine set up using the partial likelihood derivatives illustrated in Abel (2013). The arguments \code{rtot} and \code{ctot} take the row-table and column-table specific known margins. By default the diagonal values are taken as their maximum possible values given the relevant margins totals in each table. Diagonal values can be added by the user, but care must be taken to ensure resulting diagonals are feasible given the set of margins. 

The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) equal those provided in the row and column totals.
}
\value{
Returns a \code{list} object with
  \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
  \item{it }{Iteration count}
  \item{tol }{Tolerance level at final verboseation}
}
\references{
Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
}
\author{
Guy J. Abel
}
\seealso{
\code{\link{ipf3}}, \code{\link{ffs}}, \code{\link{fm}}
}
\examples{
## create row-table and column-table specific known margins.
dn <- LETTERS[1:4]
P1 <- matrix(c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200), 4, 4, 
        dimnames = list(pob = dn, por = dn), byrow = TRUE)
P2 <- matrix(c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180), 4, 4, 
        dimnames = list(pob = dn, por = dn), byrow = TRUE)
# display with row and col totals
addmargins(P1)
addmargins(P2)

# run ipf
y <- ipf3.qi(rtot = t(P1), ctot = P2)
# display with row, col and table totals
round(addmargins(y$mu), 1)
# origin-destination flow table
round(fm(y$mu), 1)

## with alternative offset term
dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 4))
y <- ipf3.qi(rtot = t(P1), ctot = P2, m = dis)
# display with row, col and table totals
round(addmargins(y$mu), 1)
# origin-destination flow table
round(fm(y$mu), 1) 
}
