#' Estimate Migration Flows to Match Net Totals via Linear Programming
#'
#' Solves for an origin-destination flow matrix that satisfies directional net migration constraints
#' while minimizing the total absolute deviation from a prior matrix. This method uses linear
#' programming with split variables to minimize L1 error, optionally respecting a structural zero mask.
#'
#' @param net_tot A numeric vector of net migration totals for each region. Must sum to zero.
#' @param m A square numeric matrix providing prior flow estimates. Must have dimensions \code{length(net_tot) × length(net_tot)}.
#' @param zero_mask A logical matrix of the same dimensions as \code{m}, where \code{TRUE} indicates forbidden (structurally zero) flows. Defaults to disallowing diagonal flows.
#' @param tol A numeric tolerance for checking that \code{sum(net_tot) == 0}. Default is \code{1e-6}.
#'
#' @return A named list with components:
#' \describe{
#'   \item{\code{n}}{Estimated matrix of flows satisfying the net constraints.}
#'   \item{\code{it}}{Number of iterations (always \code{1} for LP method).}
#'   \item{\code{tol}}{Tolerance used for checking net flow balance.}
#'   \item{\code{value}}{Total L1 deviation from prior matrix \code{m}.}
#'   \item{\code{convergence}}{Logical indicator of successful solve.}
#'   \item{\code{message}}{Text summary of convergence status.}
#' }
#'
#' @details
#' This function uses \code{lpSolve::lp()} to solve a linear program. The estimated matrix minimizes the sum of absolute deviations from the prior matrix \code{m}, subject to directional net flow constraints:
#' \deqn{\sum_j x_{ji} - \sum_j x_{ij} = \text{net}_i}
#' Structural zeros are enforced by the \code{zero_mask}. All flows are constrained to be non-negative.
#'
#' @seealso
#' \code{\link{net_matrix_entropy}()} for KL divergence minimization,
#' \code{\link{net_matrix_ipf}()} for iterative proportional fitting (IPF),
#' and \code{\link{net_matrix_optim}()} for least-squares (L2) flow estimation.
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
#' result <- net_matrix_lp(net_tot = net, m = m)
#' result$n |>
#'   addmargins() |>
#'   round(2)
#' sum_region(result$n)
#' @importFrom lpSolve lp
#' @export
net_matrix_lp <- function(net_tot, m, zero_mask = NULL, tol = 1e-6) {
  if (abs(sum(net_tot)) > tol) stop("net_tot must sum to zero")
  n <- length(net_tot)
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
  m[zero_mask] <- 0

  # Set up active decision variables
  active_cells <- which(!zero_mask, arr.ind = TRUE)
  num_vars <- nrow(active_cells)

  # LP model: split variables for absolute deviation from m
  f_obj <- rep(1, 2 * num_vars)  # minimize sum of deviations

  # Constraint matrix for net_tot balance
  constr_mat <- matrix(0, n, 2 * num_vars)
  for (k in 1:num_vars) {
    i <- active_cells[k, 1]
    j <- active_cells[k, 2]
    # Directional net constraint: inflow - outflow
    constr_mat[i, k] <- -1      # x_ij+
    constr_mat[i, k + num_vars] <- +1
    constr_mat[j, k] <- +1      # x_ij+
    constr_mat[j, k + num_vars] <- -1
  }

  rhs <- net_tot
  dir <- rep("=", n)

  res <- lpSolve::lp(
    direction = "min",
    objective.in = f_obj,
    const.mat = constr_mat,
    const.dir = dir,
    const.rhs = rhs,
    all.int = FALSE
  )

  if (res$status != 0) {
    warning("LP solver failed to converge.")
    return(list(
      n = matrix(NA, n, n),
      it = NA,
      tol = tol,
      value = NA,
      convergence = FALSE,
      message = "LP failed"
    ))
  }

  x_plus <- res$solution[1:num_vars]
  x_minus <- res$solution[(num_vars + 1):(2 * num_vars)]
  flow_vec <- x_plus - x_minus

  mu <- matrix(0, n, n)
  for (k in 1:num_vars) {
    i <- active_cells[k, 1]
    j <- active_cells[k, 2]
    mu[i, j] <- flow_vec[k]
  }

  if (!is.null(dimnames(m))) dimnames(mu) <- dimnames(m)

  list(
    n = mu,
    it = 1,
    tol = tol,
    value = sum(abs(mu - m), na.rm = TRUE),
    convergence = TRUE,
    message = "Converged"
  )
}
