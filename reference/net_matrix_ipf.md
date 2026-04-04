# Estimate Migration Flows to Match Net Totals via Iterative Proportional Fitting

The `net_matrix_ipf` function finds the maximum likelihood estimates for
a flow matrix under the multiplicative log-linear model: \$\$\log
y\_{ij} = \log \alpha_i + \log \alpha_j^{-1} + \log m\_{ij}\$\$ where
\\y\_{ij}\\ is the estimated migration flow from origin \\i\\ to
destination \\j\\, and \\m\_{ij}\\ is the prior flow. The function
iteratively adjusts origin and destination scaling factors (\\\alpha\\)
to match directional net migration totals.

## Usage

``` r
net_matrix_ipf(
  net_tot,
  m,
  zero_mask = NULL,
  maxit = 500,
  tol = 1e-06,
  verbose = FALSE
)
```

## Arguments

- net_tot:

  A numeric vector of net migration totals for each region. Must sum to
  zero.

- m:

  A square numeric matrix providing prior flow estimates. Must have
  dimensions `length(net_tot) × length(net_tot)`.

- zero_mask:

  A logical matrix of the same dimensions as `m`, where `TRUE` indicates
  forbidden (structurally zero) flows. Defaults to disallowing diagonal
  flows.

- maxit:

  Maximum number of iterations to perform. Default is `500`.

- tol:

  Convergence tolerance based on maximum change in \\\alpha\\ between
  iterations. Default is `1e-6`.

- verbose:

  Logical flag to print progress and \\\alpha\\ updates during
  iterations. Default is `FALSE`.

## Value

A named list with components:

- `n`:

  Estimated matrix of flows satisfying the net constraints.

- `it`:

  Number of iterations used.

- `tol`:

  Convergence tolerance used.

- `value`:

  Sum of squared residuals between actual and target net flows.

- `convergence`:

  Logical indicator of convergence within tolerance.

- `message`:

  Text description of convergence result.

## Details

The function avoids matrix inversion by updating \\\alpha\\ using a
closed-form solution to a quadratic equation at each step. Only
directional net flows (column sums minus row sums) are matched, not
marginal totals. Flows are constrained to be non-negative. If multiple
positive roots are available when solving the quadratic, the smaller
root is selected for improved stability.

## Author

Guy J. Abel, Peter W. F. Smith

## Examples

``` r
m <- matrix(c(0, 100, 30, 70,
              50,   0, 45,  5,
              60,  35,  0, 40,
              20,  25, 20,  0),
            nrow = 4, byrow = TRUE,
            dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
addmargins(m)
#>      dest
#> orig    A   B  C   D Sum
#>   A     0 100 30  70 200
#>   B    50   0 45   5 100
#>   C    60  35  0  40 135
#>   D    20  25 20   0  65
#>   Sum 130 160 95 115 500
sum_region(m)
#> # A tibble: 4 × 5
#>   region out_mig in_mig  turn   net
#>   <chr>    <dbl>  <dbl> <dbl> <dbl>
#> 1 A          200    130   330   -70
#> 2 B          100    160   260    60
#> 3 C          135     95   230   -40
#> 4 D           65    115   180    50

net <- c(30, 40, -15, -55)
result <- net_matrix_ipf(net_tot = net, m = m)
result$n |>
  addmargins() |>
  round(2)
#>      dest
#> orig       A      B      C     D    Sum
#>   A     0.00  80.58  26.05 34.78 141.41
#>   B    62.05   0.00  48.48  3.08 113.62
#>   C    69.11  32.48   0.00 22.89 124.48
#>   D    40.25  40.55  34.95  0.00 115.75
#>   Sum 171.41 153.62 109.48 60.75 495.26
sum_region(result$n)
#> # A tibble: 4 × 5
#>   region out_mig in_mig  turn   net
#>   <chr>    <dbl>  <dbl> <dbl> <dbl>
#> 1 A         141.  171.   313.  30.0
#> 2 B         114.  154.   267.  40.0
#> 3 C         124.  109.   234. -15.0
#> 4 D         116.   60.8  177. -55.0
```
