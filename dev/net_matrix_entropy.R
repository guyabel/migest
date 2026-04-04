#' Estimate Migration Flows to Match Net Totals via Entropy Minimization
#'
#' Solves for an origin–destination flow matrix that satisfies directional net migration constraints
#' while minimizing Kullback–Leibler (KL) divergence from a prior matrix. This yields a smooth,
#' information-theoretically regularized solution that balances fidelity to prior patterns with net flow requirements.
#'
#' @param net_tot A numeric vector of net migration totals for each region. Must sum to zero.
#' @param m A square numeric matrix providing prior flow estimates. Must have dimensions \code{length(net_tot) × length(net_tot)}.
#' @param zero_mask A logical matrix of the same dimensions as \code{m}, where \code{TRUE} indicates forbidden (structurally zero) flows. Defaults to disallowing diagonal flows.
#' @param tol Numeric tolerance for checking whether \code{sum(net_tot) == 0}. Default is \code{1e-6}.
#' @param verbose Logical flag to print solver diagnostics from \code{CVXR}. Default is \code{FALSE}.
#'
#' @return A named list with components:
#' \describe{
#'   \item{\code{n}}{Estimated matrix of flows satisfying the net constraints.}
#'   \item{\code{it}}{Number of iterations (always \code{1} for this solver).}
#'   \item{\code{tol}}{Tolerance used for the net flow balance check.}
#'   \item{\code{value}}{Sum of squared deviation from target net flows.}
#'   \item{\code{convergence}}{Logical indicating successful optimization.}
#'   \item{\code{message}}{Solver message returned by \code{CVXR}.}
#' }
#'
#' @details
#' This function minimizes the KL divergence between the estimated matrix \eqn{y_{ij}} and the prior matrix \eqn{m_{ij}}:
#' \deqn{\sum_{i,j} \left[y_{ij} \log\left(\frac{y_{ij}}{m_{ij}}\right) - y_{ij} + m_{ij}\right]}
#' subject to directional net flow constraints:
#' \deqn{\sum_j y_{ji} - \sum_j y_{ij} = \text{net}_i}
#' All flows are constrained to be non-negative. Structural zeros are enforced via \code{zero_mask}.
#' Internally uses \code{CVXR::kl_div()} for DCP-compliant KL minimization.
#'
#' @seealso
#' \code{\link{net_matrix_lp}()} for linear programming using L1 loss,
#' \code{\link{net_matrix_ipf}()} for iterative proportional fitting with multiplicative scaling,
#' and \code{\link{net_matrix_optim}()} for quadratic loss minimization.
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
#' result <- net_matrix_entropy(net_tot = net, m = m)
#' result$n |>
#'   addmargins() |>
#'   round(2)
#' sum_region(result$n)
#' @importFrom CVXR Variable Problem Minimize kl_div sum_entries solve
#' @export
net_matrix_entropy <- function(net_tot, m, zero_mask = NULL, tol = 1e-6, verbose = FALSE) {
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

  # Define CVXR variable
  M_var <- CVXR::Variable(n, n)
  net_expr <- CVXR::sum_entries(M_var, axis = 2) - CVXR::sum_entries(M_var, axis = 1)

  # Constraints
  constraints <- list(net_expr == net_tot, M_var >= 0)
  if (any(zero_mask)) constraints <- c(constraints, M_var[zero_mask] == 0)

  # KL divergence objective: y * log(y / m) − y + m
  objective <- CVXR::Minimize(
    CVXR::sum_entries(CVXR::kl_div(M_var, pmax(m, 1e-10)))
  )

  problem <- CVXR::Problem(objective, constraints)
  result <- CVXR::solve(problem, verbose = verbose)

  status <- result$status
  success <- status == "optimal"

  mu <- if (success) result$getValue(M_var) else matrix(NA, n, n)
  mu[is.na(mu)] <- 0
  if (!is.null(dimnames(m))) dimnames(mu) <- dimnames(m)

  list(
    n = mu,
    it = 1,
    tol = tol,
    value = sum((colSums(mu) - rowSums(mu) - net_tot)^2),
    convergence = success,
    message = paste("CVXR status:", status)
  )
}
