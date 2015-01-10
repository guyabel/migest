\name{migest-package}
\alias{migest-package}
\alias{migest}
\docType{package}
\title{
Useful R code for the Estimation of Migration
}
\description{
The migest package contains a collection of R functions that are useful to estimate migration when there is missing data. Included is the ffs routine to estimate migration flows from stock data, as well as various other functions to indirectly estimate flows where marginal totals are known. Functions are illustrated with dummy data.
}
\details{
\tabular{ll}{
Package: \tab migest\cr
Type: \tab Package\cr
License: \tab GPL-2\cr
}
The routines in this package include methods to estimate missing migration data when marginal totals are known. These can be grouped into functions for origin-destination matrices (\code{\link{cm2}} and \code{\link{ipf2}}) and functions for more than two dimensions (\code{\link{cm3}}, \code{\link{ipf3}} and \code{\link{ipf3.qi}}). All these routines are based on indirect estimation methods where marginal totals are known, under a Poisson regression (log-linear) model assumption. 
}
\author{
Guy J. Abel
}
\references{
Blog posts with some additional details of the implementation of functions in the package can be found at:
\url{http://gjabel.wordpress.com/category/r/migest/} 

Abel, G. J. (2005) \emph{The Indirect Estimation of Elderly Migrant Flows in England and Wales} (MS.c. Thesis). University of Southampton 

Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546

Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522

Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and registration data to estimate detailed elderly migration flows in England and Wales. \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)} 170 (4), 891--908.

Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of Migration Flows: From Entropy to EM. \emph{Mathematical Population Studies} 7 (3), 239--78.
}
