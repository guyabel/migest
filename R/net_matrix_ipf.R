#' Estimate Migration Flows to Match Net Totals via Iterative Proportional Fitting
#'
#' The \code{net_matrix_ipf} function finds the maximum likelihood estimates for a flow matrix under the multiplicative log-linear model:
#' \deqn{\log y_{ij} = \log \alpha_i + \log \alpha_j^{-1} + \log m_{ij}}
#' where \eqn{y_{ij}} is the estimated migration flow from origin \eqn{i} to destination \eqn{j}, and \eqn{m_{ij}} is the prior flow.
#' The function iteratively adjusts origin and destination scaling factors (\eqn{\alpha}) to match directional net migration totals.
#'
#' @param net_tot A numeric vector of net migration totals for each region. Must sum to zero.
#' @param m A square numeric matrix providing prior flow estimates. Must have dimensions \code{length(net_tot) × length(net_tot)}.
#' @param zero_mask A logical matrix of the same dimensions as \code{m}, where \code{TRUE} indicates forbidden (structurally zero) flows. Defaults to disallowing diagonal flows.
#' @param maxit Maximum number of iterations to perform. Default is \code{500}.
#' @param tol Convergence tolerance based on maximum change in \eqn{\alpha} between iterations. Default is \code{1e-6}.
#' @param verbose Logical flag to print progress and \eqn{\alpha} updates during iterations. Default is \code{FALSE}.
#'
#' @return A named list with components:
#' \describe{
#'   \item{\code{n}}{Estimated matrix of flows satisfying the net constraints.}
#'   \item{\code{it}}{Number of iterations used.}
#'   \item{\code{tol}}{Convergence tolerance used.}
#'   \item{\code{value}}{Sum of squared residuals between actual and target net flows.}
#'   \item{\code{convergence}}{Logical indicator of convergence within tolerance.}
#'   \item{\code{message}}{Text description of convergence result.}
#' }
#'
#' @details
#' The function avoids matrix inversion by updating \eqn{\alpha} using a closed-form solution to a quadratic equation at each step.
#' Only directional net flows (column sums minus row sums) are matched, not marginal totals. Flows are constrained to be non-negative.
#' If multiple positive roots are available when solving the quadratic, the smaller root is selected for improved stability.
#'
#' @author Guy J. Abel, Peter W. F. Smith
#'
#' @seealso
#' \code{\link{net_matrix_entropy}()} for entropy-based estimation minimizing KL divergence,
#' \code{\link{net_matrix_lp}()} for L1-loss linear programming,
#' and \code{\link{net_matrix_optim}()} for least-squares (L2) optimization.
#'
#' @examples
#' m <- matrix(c(0, 100, 30, 70,
#'               50,   0, 45,  5,
#'               60,  35,  0, 40,
#'               20,  25, 20,  0),
#'             nrow = 4, byrow = TRUE,
#'             dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
#' addmargins(m)
#' sum_region(m)
#'
#' net <- c(30, 40, -15, -55)
#' result <- net_matrix_ipf(net_tot = net, m = m)
#' result$n |>
#'   addmargins() |>
#'   round(2)
#' sum_region(result$n)
#' @export
net_matrix_ipf <- function(net_tot, m, zero_mask = NULL, maxit = 500, tol = 1e-6, verbose = FALSE) {
  n <- length(net_tot)
  if (abs(sum(net_tot)) > tol) stop("net_tot must sum to zero")
  if (!identical(dim(m), c(n, n))) stop("m must be a square matrix matching net_tot")
  if (!is.null(zero_mask) && !identical(dim(zero_mask), dim(m))) {
    stop("zero_mask must match dimensions of m")
  }
  # Default: forbid self-migration
  if (is.null(zero_mask)) {
    zero_mask <- diag(n) == 1
  } else {
    if (!identical(dim(zero_mask), dim(m))) stop("zero_mask must match dimensions of 'm'")
  }

  m[zero_mask] <- 0  # exclude structurally fixed flows

  alpha <- rep(1, n)
  d_max <- Inf
  it <- 0

  while (d_max > tol && it < maxit) {
    alpha_old <- alpha

    for (i in 1:n) {
      a <- sum((1 / alpha_old) * m[i, ])
      b <- net_tot[i]
      c <- -sum(alpha_old * m[, i])

      roots <- quadratic_eqn(a = a, b = b, c = c)
      roots <- roots[roots > 0 & is.finite(roots)]
      alpha[i] <- if (length(roots)) min(roots) else 1
    }

    d_max <- max(abs(alpha_old - alpha), na.rm = TRUE)

    if (verbose && (it < 20 || it %% 10 == 0)) {
      cat("Iteration:", it, "\n")
      cat("Alpha:", round(alpha, 6), "\n\n")
    }

    it <- it + 1
  }

  mu <- diag(alpha) %*% m %*% diag(1 / alpha)
  mu[zero_mask] <- 0
  if (!is.null(dimnames(m))) dimnames(mu) <- dimnames(m)

  list(
    n = mu,
    it = it,
    tol = tol,
    value = sum((colSums(mu) - rowSums(mu) - net_tot)^2),
    convergence = is.finite(d_max) && d_max < tol,
    message = if (is.finite(d_max) && d_max < tol) "Converged" else "Did not converge"
  )
}
