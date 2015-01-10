\name{ffs}
\alias{ffs}
\title{
Flows from Stocks Methodology
}
\description{
Estimates migrant transitions flows between two sequential migrant stock tables.
}
\usage{
ffs(P1, P2, d, b, m = NULL, method = "stocks", b.mat = NULL, d.mat = NULL, 
 b.deduct="native.gt0", ...)
}
\arguments{
  \item{P1}{
Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
}
  \item{P2}{
Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
}
  \item{d}{
Number of deaths between time \emph{t} and \emph{t}+1 in each region.
}
  \item{b}{
Number of births between time \emph{t} and \emph{t}+1 in each region.
}
  \item{m}{
Array of auxiliary data. By default set to 1 for all origin-destination-migrant type combinations.
}
  \item{method}{
Method used to adjust row margin totals of \code{P1} and \code{P2} to equal. By default \code{method="stocks"}. Can also take values \code{method="outside"} or \code{method="deaths"}. See details for explanation on each method.
}
  \item{b.mat}{
A matrix containing the number of births during the period in each birthplace (rows) by place of residence (columns) combination created by the user. By default, this argument is \code{NULL}, and hence within the function a \code{b.mat} is formed as a diagonal matrix of \code{b} (i.e. all births happen in their respective place of residence at the end of the period, there are no infant migrants.)
}
  \item{d.mat}{
A matrix containing the number of deaths during the period in each birthplace (rows) by place of residence (columns) combination created by the user. By default, this argument is \code{NULL}, and hence within the function a \code{d.mat} is formed as a proportional allocation of \code{d} over all populations (i.e. the mortality rate in each place of birth sub-group (native born and all foreign born stocks) is the same.)
}
  \item{b.deduct}{
Method used to deduct births. By default \code{b.deduct="native.gt0"} deducts births from diagonals of stock table (i.e. the native born populations) with the exception of regions where this would lead to a negative adjusted population. In these select regions, births are spread over all population stocks (both native and foreign) thus avoiding potential negative flows. Can also take \code{b.deduct="native"} in which all births are deducted from the diagonals of stock table (i.e. native born populations only).
}
  \item{\dots}{
Additional arguments passes to \code{\link{ipf3.qi}}.
}
}
\details{
Estimates migrant transitions flows between two sequential migrant stock tables as shown in Abel (2013), when \code{method="outside"}. The length of \code{b} and \code{d} must equal the number of rows in \code{P1} and number of columns in \code{P2}.

Setting \code{method="stocks"} estimates migration flows using an alternative demographic accounting method to adjusted stock tables to match the row totals of the stock table after demographic accounting. Setting \code{method="deaths"} uses the calculation of the deaths by place of residence table to match the row totals of the stock table after demographic accounting. Note, when \code{b} and \code{d} are equal, the same estimated flows from stocks are obtained regardless of the \code{method} argument. Both of these options maintain the net migration flow implied by the population, birth and death data. I still need to write up these methods. 

Setting \code{b.deduct="native.gt0"} allows estimates to correct for cases where the number of births far exceeds the change in the native born population. Such cases potentially occur where place of birth stock data imply a change in native born population which directly conflict with changes in demographic data unless there is mass migration of all new borns.
}
\value{
Returns a \code{list} object with:
  \item{mu }{Array of indirect estimates of origin-destination matrices by place of birth.}
  \item{it }{Iteration count.}
  \item{tol }{Tolerance level at final verboseation.}
  \item{y }{Array of indirect estimates of origin-destination matrices by place of birth with additional rows and columns for births, deaths and moves to other regions.}
}
\references{
Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522
Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
}
\author{
Guy J. Abel
}
\seealso{
\code{\link{ipf3.qi}}, \code{\link{fm}}
}
\examples{
## create P1 and P2 stock tables
dn <- LETTERS[1:4]
P1 <- matrix(c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200), 4, 4, 
        dimnames = list(pob = dn, por = dn), byrow = TRUE)
P2 <- matrix(c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180), 4, 4, 
        dimnames = list(pob = dn, por = dn), byrow = TRUE)
# display with row and col totals
addmargins(P1)
addmargins(P2)

# no births and deaths
b <- rep(0, 4)
d <- rep(0, 4)

y <- ffs(P1, P2, d, b)
# display with row, col and table totals
round(addmargins(y$mu), 1)
# display with row and col totals
round(fm(y$mu), 1)

## alternative offset term
dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 4))
y <- ffs(P1, P2, d, b, dis)
# display with row, col and table totals
round(addmargins(y$mu), 1)
# display with row and col totals
round(fm(y$mu), 1)

## alternative P2 and changes in population from natural increase
P2 <- matrix(c(1060, 60, 10, 10, 45, 540, 40, 0, 70, 75, 770, 70, 30, 30, 20, 230), 4, 4, 
        dimnames = list(pob = dn, por = dn), byrow = TRUE)
# display with row and col totals
addmargins(P2)
b <- c(80, 20, 40, 60)
d <- c(70, 30, 50, 10)

y <- ffs(P1, P2, d, b, method="outside")
# display with row, col and table totals
round(addmargins(y$mu), 1)
# display with row and col totals
round(fm(y$mu), 1)
}
