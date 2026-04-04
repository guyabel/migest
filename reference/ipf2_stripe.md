# iterative proportional fitting routine for the indirect estimation of origin-destination-type migration flow tables with known origin and destination margins and stripe elements.

The `ipf2.b` function finds the maximum likelihood estimates for fitted
values in the log-linear model: \$\$ \log y\_{pq} = \log \alpha\_{p} +
\log \beta\_{q} + \log \lambda\_{ij}I(p \in i, q \in j) + \log m\_{pq}
\$\$ where \\m\_{pq}\\ is a prior estimate for \\y\_{pq}\\ and is no
more complex than the matrices being fitted. The \\\lambda\_{ij}I(p \in
i, q \in j)\\ term ensures a saturated fit on the block the \\(i,j)\\
block.

## Usage

``` r
ipf2_stripe(
  row_tot = NULL,
  col_tot = NULL,
  stripe_tot = NULL,
  stripe = NULL,
  m = NULL,
  tol = 1e-05,
  maxit = 500,
  verbose = TRUE,
  ...
)
```

## Arguments

- row_tot:

  Vector of origin totals to constrain the sum of the imputed cell rows.

- col_tot:

  Vector of destination totals to constrain the sum of the imputed cell
  columns.

- stripe_tot:

  Matrix of stripe totals to constrain the sum of the imputed cell
  blocks.

- stripe:

  Matrix of stripe structure corresponding to `stripe_tot`.

- m:

  Matrix of auxiliary data. By default set to 1 for all
  origin-destination combinations.

- tol:

  Numeric value for the tolerance level used in the parameter
  estimation.

- maxit:

  Numeric value for the maximum number of iterations used in the
  parameter estimation.

- verbose:

  Logical value to indicate the print the parameter estimates at each
  iteration. By default `FALSE`.

- ...:

  Additional arguments passes to [`stripe_matrix`](stripe_matrix.md).

## Value

Iterative Proportional Fitting routine set up using the partial
likelihood derivatives. The arguments `row_tot` and `col_tot` take the
row-table and column-table specific known margins. The `stripe_tot` take
the totals over the stripes in the matrix defined with `b`. Diagonal
values can be added by the user, but care must be taken to ensure
resulting diagonals are feasible given the set of margins. The user must
ensure that the row and column totals in each table sum to the same
value. Care must also be taken to allow the dimension of the auxiliary
matrix (`m`) equal those provided in the row and column totals. Returns
a `list` object with

- mu :

  Array of indirect estimates of origin-destination matrices by migrant
  characteristic

- it :

  Iteration count

- tol :

  Tolerance level at final iteration

## See also

[`stripe_matrix`](stripe_matrix.md), [`block_matrix`](block_matrix.md)

## Author

Guy J. Abel

## Examples

``` r
y <- ipf2_stripe(row_tot = c(85, 70, 35, 30, 60, 55, 65),
 stripe_tot = matrix(c(15,20,50,
                35,10,25,
                5 ,0 ,30,
                10,10,10,
                30,30,0,
                15,30,10,
                35,25,5 ), ncol = 3, byrow = TRUE),
 stripe = stripe_matrix(x = 1:21, s = c(2,2,3), byrow = TRUE))
#> 1 78 
#> 2 0 
 addmargins(y$mu)
#>       A1   A2   B1   B2        C1        C2        C3 Sum
#> A1   7.5  7.5 10.0 10.0 16.666667 16.666667 16.666667  85
#> A2  17.5 17.5  5.0  5.0  8.333333  8.333333  8.333333  70
#> B1   2.5  2.5  0.0  0.0 10.000000 10.000000 10.000000  35
#> B2   5.0  5.0  5.0  5.0  3.333333  3.333333  3.333333  30
#> C1  15.0 15.0 15.0 15.0  0.000000  0.000000  0.000000  60
#> C2   7.5  7.5 15.0 15.0  3.333333  3.333333  3.333333  55
#> C3  17.5 17.5 12.5 12.5  1.666667  1.666667  1.666667  65
#> Sum 72.5 72.5 62.5 62.5 43.333333 43.333333 43.333333 400
```
