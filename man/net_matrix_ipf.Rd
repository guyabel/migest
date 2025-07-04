% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_matrix_ipf.R
\name{net_matrix_ipf}
\alias{net_matrix_ipf}
\title{Estimate Migration Flows to Match Net Totals via Iterative Proportional Fitting}
\usage{
net_matrix_ipf(
  net_tot,
  m,
  zero_mask = NULL,
  maxit = 500,
  tol = 1e-06,
  verbose = FALSE
)
}
\arguments{
\item{net_tot}{A numeric vector of net migration totals for each region. Must sum to zero.}

\item{m}{A square numeric matrix providing prior flow estimates. Must have dimensions \code{length(net_tot) × length(net_tot)}.}

\item{zero_mask}{A logical matrix of the same dimensions as \code{m}, where \code{TRUE} indicates forbidden (structurally zero) flows. Defaults to disallowing diagonal flows.}

\item{maxit}{Maximum number of iterations to perform. Default is \code{500}.}

\item{tol}{Convergence tolerance based on maximum change in \eqn{\alpha} between iterations. Default is \code{1e-6}.}

\item{verbose}{Logical flag to print progress and \eqn{\alpha} updates during iterations. Default is \code{FALSE}.}
}
\value{
A named list with components:
\describe{
\item{\code{n}}{Estimated matrix of flows satisfying the net constraints.}
\item{\code{it}}{Number of iterations used.}
\item{\code{tol}}{Convergence tolerance used.}
\item{\code{value}}{Sum of squared residuals between actual and target net flows.}
\item{\code{convergence}}{Logical indicator of convergence within tolerance.}
\item{\code{message}}{Text description of convergence result.}
}
}
\description{
The \code{net_matrix_ipf} function finds the maximum likelihood estimates for a flow matrix under the multiplicative log-linear model:
\deqn{\log y_{ij} = \log \alpha_i + \log \alpha_j^{-1} + \log m_{ij}}
where \eqn{y_{ij}} is the estimated migration flow from origin \eqn{i} to destination \eqn{j}, and \eqn{m_{ij}} is the prior flow.
The function iteratively adjusts origin and destination scaling factors (\eqn{\alpha}) to match directional net migration totals.
}
\details{
The function avoids matrix inversion by updating \eqn{\alpha} using a closed-form solution to a quadratic equation at each step.
Only directional net flows (column sums minus row sums) are matched, not marginal totals. Flows are constrained to be non-negative.
If multiple positive roots are available when solving the quadratic, the smaller root is selected for improved stability.
}
\examples{
m <- matrix(c(0, 100, 30, 70,
              50,   0, 45,  5,
              60,  35,  0, 40,
              20,  25, 20,  0),
            nrow = 4, byrow = TRUE,
            dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
addmargins(m)
sum_region(m)

net <- c(30, 40, -15, -55)
result <- net_matrix_ipf(net_tot = net, m = m)
result$n |>
  addmargins() |>
  round(2)
sum_region(result$n)
}
\seealso{
\code{\link{net_matrix_entropy}()} for entropy-based estimation minimizing KL divergence,
\code{\link{net_matrix_lp}()} for L1-loss linear programming,
and \code{\link{net_matrix_optim}()} for least-squares (L2) optimization.
}
\author{
Guy J. Abel, Peter W. F. Smith
}
