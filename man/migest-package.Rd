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
The estimation methods in this package can be grouped as 1) functions for origin-destination matrices (\code{\link{cm2}} and \code{\link{ipf2}}) and 2) functions for origin-destination matrices categorized by a further set of characteristics, such as ethnicity, employment or health status (\code{\link{cm3}}, \code{\link{ipf3}} and \code{\link{ipf3_qi}}). Each of these routines are based on indirect estimation methods where marginal totals are known, and a Poisson regression (log-linear) model is assumed. 

The \code{\link{ffs_diff}}, \code{\link{ffs_rates}} and \code{\link{ffs_demo}} functions provide different methods to estimate migration bilateral flows from changes in stocks, see Abel and Cohen (2019) for a review of different methods. The demo files, \code{demo(cfplot_reg2)}, \code{demo(cfplot_reg)} and \code{demo(cfplot_nat)}, produce circular migration flow plots for migration estimates from Abel(2018) and Abel and Sander (2014), which were derived using the \code{\link{ffs_demo}} function.

Github repo:
\url{https://github.com/guyabel/migest} 

}
\author{
Guy J. Abel
}
\references{
Abel and Cohen (2019) Bilateral international migration flow estimates for 200 countries \emph{Scientific Data} 6 (1), 1-13

Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by Gender between 1960 and 2015. \emph{International Migration Review} 52 (3), 809–852.

Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546

Abel, G. J. (2005) \emph{The Indirect Estimation of Elderly Migrant Flows in England and Wales} (MS.c. Thesis). University of Southampton 

Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522

Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and registration data to estimate detailed elderly migration flows in England and Wales. \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)} 170 (4), 891--908.

Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of Migration Flows: From Entropy to EM. \emph{Mathematical Population Studies} 7 (3), 239--78.
}
