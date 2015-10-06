\name{migest-package}
\alias{migest-package}
\alias{migest}
\docType{package}
\title{
Methods for the Indirect Estimation of Bilateral Migration
}
\description{
The migest package contains a collection of R functions for indirect methods to estimate bilateral migration flows in the presence of partial or missing data. Methods might be relevant to other categorical data situations on non-migration data, where for example, marginal totals are known and only auxiliary bilateral data is available.
}
\details{
\tabular{ll}{
Package: \tab migest\cr
Type: \tab Package\cr
License: \tab GPL-2\cr
}
The estimation methods in this package can be grouped as 1) functions for origin-destination matrices (\code{\link{cm2}} and \code{\link{ipf2}}) and 2) functions for origin-destination matrices categorised by a further set of characteristics, such as ethnicity, employment or health status (\code{\link{cm3}}, \code{\link{ipf3}} and \code{\link{ipf3.qi}}). All these routines are based on indirect estimation methods where marginal totals are known, and a Poisson regression (log-linear) model is assumed. 

The flow from stock function, \code{\link{ffs}} acts as a wrapper for a combination of some of these estimation routines with further adjustments for changes in foreign born stocks over a period. The demo files, \code{demo(cfplot_reg)} and \code{demo(cfplot_nat)}, produce circular migration flow plots for migration estimates from Abel and Sander (2014), which were derived using the \code{\link{ffs}} function.

Blog posts with some additional details of the implementation of functions in the package can be found at
\url{http://gjabel.wordpress.com/category/r/migest/} 

Github repo:
\url{http://github.com/gjabel/migest} 

}
\author{
Guy J. Abel
}
\references{
Abel, G. J. (2015). Abel, G. J. (2015). Estimates of Global Bilateral Migration Flows by Gender Between 1960 and 2010. \emph{Vienna Institute of Demography Woking Papers} 5/2015.

Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546

Abel, G. J. (2005) \emph{The Indirect Estimation of Elderly Migrant Flows in England and Wales} (MS.c. Thesis). University of Southampton 

Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522

Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and registration data to estimate detailed elderly migration flows in England and Wales. \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)} 170 (4), 891--908.

Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of Migration Flows: From Entropy to EM. \emph{Mathematical Population Studies} 7 (3), 239--78.
}
