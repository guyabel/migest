library(pracma)

Transf <- function(x, p) {
  stopifnot(is.numeric(p), length(p) == 1, p > 0)
  if (p <= 1 && abs(p - 0.5) > 1e-8) {
    return(((x + 1)^(2 - 1/p) - 1) / (2 - 1/p))
  } else if (abs(p - 0.5) < 1e-8) {
    return(log(1 + x))
  } else { # p > 1
    return(x^2 / Transf(x, 1/p))
  }
}

net_matrix_beyer <- function(net_tot, m, zero_mask = NULL,
                             n_iter = 3, tol = 1e-6, verbose = FALSE) {
  n <- nrow(m)
  if (abs(sum(net_tot)) > tol) stop("net_tot must sum to zero")
  if (!identical(dim(m), c(n, n))) stop("m must be square with dimension n")

  if (is.null(zero_mask)) zero_mask <- diag(n) == 1
  m[zero_mask] <- 0

  corridors <- which(!zero_mask, arr.ind = TRUE)
  n_corridors <- nrow(corridors)
  bilateral_flows <- as.numeric(m[!zero_mask])

  for (iter in seq_len(n_iter)) {
    correction_factors <- rep(1, n)

    for (countryID in seq_len(n)) {
      outflows <- bilateral_flows[corridors[,1] == countryID]
      inflows  <- bilateral_flows[corridors[,2] == countryID]
      netmigration <- net_tot[countryID]

      if (abs(netmigration) < tol) {
        sol <- 1.0
      } else {
        fun <- function(p) {
          sum(Transf(outflows, p)) - sum(Transf(inflows, 1/p)) + netmigration
        }
        # Extract numeric root from brentDekker result
        sol <- brentDekker(fun, a = 1e-6, b = 50, tol = tol)$root
      }
      correction_factors[countryID] <- sol
    }

    for (corrID in seq_len(n_corridors)) {
      flow <- bilateral_flows[corrID]
      origin <- corridors[corrID,1]
      destin <- corridors[corrID,2]

      origin_corr <- Transf(flow, correction_factors[origin])
      destin_corr <- Transf(flow, 1 / correction_factors[destin])

      bilateral_flows[corrID] <- sqrt(origin_corr * destin_corr)
    }

    if (verbose) {
      cat("Iteration", iter, "correction factors:", round(correction_factors, 4), "\n")
    }
  }

  mu <- matrix(0, n, n)
  mu[!zero_mask] <- bilateral_flows
  dimnames(mu) <- dimnames(m)

  list(
    n = mu,
    value = sum((colSums(mu) - rowSums(mu) - net_tot)^2),
    convergence = TRUE,
    message = "Beyer adjustment applied"
  )
}

m <- matrix(c(0, 100, 30, 70,
              50,   0, 45,  5,
              60,  35,  0, 40,
              20,  25, 20,  0),
            nrow = 4, byrow = TRUE,
            dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
addmargins(m)
library(migest)
sum_region(m)

zero_mask <- diag(4) == 1
net <- c(30, 40, -15, -55)


result <- net_matrix_beyer(net, m, zero_mask, n_iter = 20, tol = 1e-8, verbose = TRUE)
round(addmargins(result$n), 1)
round(colSums(result$n) - rowSums(result$n), 2)  # check net totals
net

