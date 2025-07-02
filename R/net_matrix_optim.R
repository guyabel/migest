#' Estimate Migration Flows to Match Net Totals via Quadratic Optimization
#'
#' Solves for an origin–destination flow matrix that satisfies directional net migration constraints
#' while minimizing squared deviation from a prior matrix.
#'
#' @param net_tot A numeric vector of net migration totals for each region. Must sum to zero.
#' @param m A square numeric matrix providing prior flow estimates. Must have dimensions \code{length(net_tot) × length(net_tot)}.
#' @param zero_mask A logical matrix of the same dimensions as \code{m}, where \code{TRUE} indicates forbidden (structurally zero) flows. Defaults to disallowing diagonal flows.
#' @param maxit Maximum number of iterations to perform. Default is \code{500}.
#' @param tol Numeric tolerance for checking whether \code{sum(net_tot) == 0}. Default is \code{1e-6}.
#'
#' @return A named list with components:
#' \describe{
#'   \item{\code{n}}{Estimated matrix of flows satisfying the net constraints.}
#'   \item{\code{it}}{Number of optimization iterations (if available).}
#'   \item{\code{tol}}{Tolerance used for the net flow balance check.}
#'   \item{\code{value}}{Objective function value (sum of squared deviations).}
#'   \item{\code{convergence}}{Logical indicating successful convergence.}
#'   \item{\code{message}}{Solver message or status.}
#' }
#'
#' @details
#' The function minimizes:
#' \deqn{\sum_{i,j} (y_{ij} - m_{ij})^2}
#' subject to directional net flow constraints:
#' \deqn{\sum_j y_{ji} - \sum_j y_{ij} = \text{net}_i}
#' and non-negativity constraints on all flows. Structural zeros are enforced using \code{zero_mask}.
#' Internally uses \code{optim()} or a constrained quadratic programming solver.
#'
#' @seealso
#' \code{\link{net_matrix_entropy}()} for KL divergence minimization,
#' \code{\link{net_matrix_ipf}()} for iterative proportional fitting,
#' and \code{\link{net_matrix_lp}()} for linear programming with L1 loss.
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
#' result <- net_matrix_optim(net_tot = net, m = m)
#' result$n |>
#'   addmargins() |>
#'   round(2)
#' sum_region(result$n)
#' @export
net_matrix_optim <- function(net_tot, m, zero_mask = NULL, maxit = 500, tol = 1e-06) {
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

  # Indices of free (optimizable) cells
  free_indices <- which(!zero_mask)
  start_vals <- m[free_indices]

  # Loss function: squared difference between estimated and target net totals
  loss <- function(free_params) {
    full_matrix <- matrix(0, n, n)
    full_matrix[free_indices] <- free_params
    net_est <- colSums(full_matrix) - rowSums(full_matrix)
    sum((net_est - net_tot)^2)
  }

  # Optimize using L-BFGS-B
  result <- stats::optim(
    par = start_vals,
    fn = loss,
    method = "L-BFGS-B",
    lower = 0,
    control = list(
      factr = tol / .Machine$double.eps,
      pgtol = tol,
      maxit = maxit
    )
  )

  # Rebuild full matrix from optimized values
  mu <- matrix(0, n, n)
  mu[free_indices] <- result$par
  mu[zero_mask] <- 0

  # Preserve original dimnames
  if (!is.null(dimnames(m))) {
    dimnames(mu) <- dimnames(m)
  }

  list(
    n = mu,
    it = result$counts,
    tol = tol,
    value = result$value,
    convergence = result$convergence == 0,
    message = result$message
  )
}
