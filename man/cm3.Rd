\name{cm3}
\alias{cm3}
\title{
Conditional Maximisation Routine for the Indirect Estimation of Origin-Destination-Migrant Type Migration Flow Tables with Known Origin and Destination Margins.
}
\description{
The \code{cm3} function finds the maximum likelihood estimates for parameters in the log-linear model:
\deqn{ \log y_{ijk} = \log \alpha_{i} + \log \beta_{j} + \log m_{ijk} }
as introduced by Abel (2005). The \eqn{\alpha_{i}} and  \eqn{\beta_{j}} represent background information related to  the characteristics of the origin and destinations respectively. The \eqn{m_{ijk}} factor represents auxiliary information on origin-destination migration flows by a migrant characteristic (such as age, sex, disability, household type, economic status, etc.). This method is useful for combining data from detailed data collection processes (such as a Census) with more up-to-date information on migration inflows and outflows (where details on movements by migrant characteristics are not known).
}
\usage{
cm3(rtot = NULL, ctot = NULL, m, tol = 1e-05, maxit = 500, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rtot}{
Origin (row) totals to constrain indirect estimates to.
}
  \item{ctot}{
Destination (column) totals to constrain indirect estimates to.
}
  \item{m}{
Array of auxiliary data. By default set to 1 for all origin-destination-migrant type combinations.
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
Parameter estimates were obtained using the conditional maximisation of the likelihood, as discussed by Abel (2005) and Raymer et. al. (2007). 

The user must ensure that the row and column totals are equal in sum. Care must also be taken to allow the row and column dimension of the auxiliary matrix (\code{m}) to equal those provided in the row and column totals.
}
\value{
Returns a \code{list} object with:
  \item{N }{Array of origin-destination matrices of indirect estimates}
  \item{theta }{Collection of parameter estimates}
}
\references{
Abel, G. J. (2005) \emph{The Indirect Estimation of Elderly Migrant Flows in England and Wales} (MS.c. Thesis). University of Southampton

Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and registration data to estimate detailed elderly migration flows in England and Wales. \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)} 170 (4), 891--908.
}
\author{
Guy J. Abel
}
\seealso{
\code{\link{cm2}}, \code{\link{ipf3}}
}
\examples{
## over two tables
dn <- LETTERS[1:2]
y <- cm3(rtot = c(18, 20) * 2, ctot = c(16, 22) * 2, 
        m = array(c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2), 
        dimnames = list(orig = dn, dest = dn, type = c("ILL", "HEALTHY"))))
# display with row, col and table totals
y

## over three tables
y <- cm3(rtot = c(170, 120, 410), ctot = c(500, 140, 60), 
        m = array(c(5, 1, 2, 7, 4, 2, 5, 9, 5, 4, 3, 1), dim = c(2, 2, 3), 
        dimnames = list(orig = dn, dest = dn, type = c("0--15", "15-60", ">60"))),
        verbose = FALSE)
# display with row, col and table totals
y
}
