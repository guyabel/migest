\name{fm}
\alias{fm}
\title{
Extract a Classic Origin-Destination Migration Flow Matrix.
}
\description{
Extract a classic origin-destination migration flow matrix from a more detailed dis-aggregation of flows stored in an (\code{array}) object.
}
\usage{
fm(y)
}
\arguments{
  \item{y}{
Array of origin-destination matrices, where the first and second dimensions correspond to origin and destination respectively. Higher dimension(s) refer to additional migrant characteristic(s).
}
}
\details{
Sums over the first and second dimension and removes counts on diagonals.
}
\value{
Returns a \code{matrix} object of origin-destination flows
}
\author{
Guy J. Abel
}
\examples{
dn <- LETTERS[1:2]
y <- cm3(rtot = c(18, 20) * 2, ctot = c(16, 22) * 2, 
        m = array(c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2), 
        dimnames = list(orig = dn, dest = dn, type = c("ILL", "HEALTHY"))))
round(addmargins(y$N))
round(addmargins(fm(y$N))) 
}
