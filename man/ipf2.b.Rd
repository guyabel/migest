\name{ipf2.b}
\alias{ipf2.b}
\title{
Iterative Proportional Fitting Routine for the Indirect Estimation of Origin-Destination-Type Migration Flow Tables with Known Origin and Destination Margins and Block Diagonal Elements.
}
\description{
The \code{ipf2.b} function finds the maximum likelihood estimates for fitted values in the log-linear model:
\deqn{ \log y_{pq} = \log \alpha_{p} + \log \beta_{q} + \log \lambda_{ij}I(p \in i, q \in j) + \log m_{pq} }
where \eqn{m_{pq}} is a prior estimate for \eqn{y_{pq}} and is no more complex than the matrices being fitted. The \eqn{\lambda_{ij}I(p \in i, q \in j)} term ensures a saturated fit on the block the \eqn{(i,j)} block.
}
\usage{
ipf2.b(rtot = NULL, ctot = NULL, btot = NULL, b = NULL, m = NULL, tol = 1e-05, 
        maxit = 500, verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rtot}{
Origin (row) totals to constrain indirect estimates to.
}
  \item{ctot}{
Destination (column) totals to constrain indirect estimates to.
}
  \item{btot}{
Matrix to constrain block totals to. Blocks are defined using the \code{\link{block.matrix}} using with \code{b} defining the block sizes.
}
  \item{b}{
Size of blocks in the diagonal of the matrix.
}
  \item{m}{
Array of auxiliary data. By default set to 1 for all origin-destination-migrant type combinations.
}
  \item{tol}{
Tolerance level for parameter estimation.
}
  \item{maxit}{
Maximum number of iterations for parameter estimation.
}
  \item{verbose}{
Print the parameter estimates at each iteration. By default \code{FALSE}.
}
  \item{\dots}{
Additional arguments passes to \code{\link{block.matrix}}.
}
}
\details{
Iterative Proportional Fitting routine set up using the partial likelihood derivatives. The arguments \code{rtot} and \code{ctot} take the row-table and column-table specific known margins. The \code{btot} take the totals over the blocks in the matrix defined with \code{b}. Diagonal values can be added by the user, but care must be taken to ensure resulting diagonals are feasible given the set of margins. 

The user must ensure that the row and column totals in each table sum to the same value. Care must also be taken to allow the dimension of the auxiliary matrix (\code{m}) equal those provided in the row and column totals.
}
\value{
Returns a \code{list} object with
  \item{mu }{Array of indirect estimates of origin-destination matrices by migrant characteristic}
  \item{it }{Iteration count}
  \item{tol }{Tolerance level at final iteration}
}
%\references{
%Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
%}
\author{
Guy J. Abel
}
\seealso{
\code{\link{ipf2}}, \code{\link{block.matrix}}
}
\examples{
#block sums
b.mat <- matrix(c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0), nrow = 4, byrow = TRUE)

y <- ipf2.b(rtot= c(30,20,30,10,20,5,0,10,5,5,5,10),
            ctot = c(45,10,10,5,5,10,50,5,10,0,0,0),
            btot = b.mat, b = c(2,3,4,3), dimnames = LETTERS[1:4])

addmargins(b.mat)
addmargins(y$mu)
}
