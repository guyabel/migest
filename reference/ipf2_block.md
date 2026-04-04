# Iterative proportional fitting routine for the indirect estimation of origin-destination-type migration flow tables with known origin and destination margins and block diagonal elements.

The `ipf2.b` function finds the maximum likelihood estimates for fitted
values in the log-linear model: \$\$ \log y\_{pq} = \log \alpha\_{p} +
\log \beta\_{q} + \log \lambda\_{ij}I(p \in i, q \in j) + \log m\_{pq}
\$\$ where \\m\_{pq}\\ is a prior estimate for \\y\_{pq}\\ and is no
more complex than the matrices being fitted. The \\\lambda\_{ij}I(p \in
i, q \in j)\\ term ensures a saturated fit on the block the \\(i,j)\\
block.

## Usage

``` r
ipf2_block(
  row_tot = NULL,
  col_tot = NULL,
  block_tot = NULL,
  block = NULL,
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

- block_tot:

  Matrix of block totals to constrain the sum of the imputed cell
  blocks.

- block:

  Matrix of block structure corresponding to `block_tot`.

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

  Additional arguments passes to [`block_matrix`](block_matrix.md).

## Value

Iterative Proportional Fitting routine set up using the partial
likelihood derivatives. The arguments `row_tot` and `col_tot` take the
row-table and column-table specific known margins. The `block_tot` take
the totals over the blocks in the matrix defined with `b`. Diagonal
values can be added by the user, but care must be taken to ensure
resulting diagonals are feasible given the set of margins.

The user must ensure that the row and column totals in each table sum to
the same value. Care must also be taken to allow the dimension of the
auxiliary matrix (`m`) equal those provided in the row and column
totals.

Returns a `list` object with

- mu :

  Array of indirect estimates of origin-destination matrices by migrant
  characteristic

- it :

  Iteration count

- tol :

  Tolerance level at final iteration

## See also

[`block_matrix`](block_matrix.md), [`stripe_matrix`](stripe_matrix.md)

## Author

Guy J. Abel

## Examples

``` r
y <- ipf2_block(row_tot= c(30,20,30,10,20,5,0,10,5,5,5,10),
                col_tot = c(45,10,10,5,5,10,50,5,10,0,0,0),
                block_tot = matrix(data = c(0,0 ,50,0, 35,0,25,0, 10,10,0,0, 10,10,0,0),
                              nrow = 4, byrow = TRUE),
                block = block_matrix(x = 1:16, b = c(2,3,4,3)))
#> 1 38 
#> 2 3.552714e-15 
addmargins(y$mu)
#>            A1         A2    B1    B2    B3         C1        C2        C3
#> A1   0.000000  0.0000000  0.00 0.000 0.000  4.0000000 20.000000 2.0000000
#> A2   0.000000  0.0000000  0.00 0.000 0.000  2.6666667 13.333333 1.3333333
#> B1  14.318182  3.1818182  0.00 0.000 0.000  1.6666667  8.333333 0.8333333
#> B2   4.772727  1.0606061  0.00 0.000 0.000  0.5555556  2.777778 0.2777778
#> B3   9.545455  2.1212121  0.00 0.000 0.000  1.1111111  5.555556 0.5555556
#> C1   2.045455  0.4545455  1.25 0.625 0.625  0.0000000  0.000000 0.0000000
#> C2   0.000000  0.0000000  0.00 0.000 0.000  0.0000000  0.000000 0.0000000
#> C3   4.090909  0.9090909  2.50 1.250 1.250  0.0000000  0.000000 0.0000000
#> C4   2.045455  0.4545455  1.25 0.625 0.625  0.0000000  0.000000 0.0000000
#> D1   2.045455  0.4545455  1.25 0.625 0.625  0.0000000  0.000000 0.0000000
#> D2   2.045455  0.4545455  1.25 0.625 0.625  0.0000000  0.000000 0.0000000
#> D3   4.090909  0.9090909  2.50 1.250 1.250  0.0000000  0.000000 0.0000000
#> Sum 45.000000 10.0000000 10.00 5.000 5.000 10.0000000 50.000000 5.0000000
#>             C4 D1 D2 D3 Sum
#> A1   4.0000000  0  0  0  30
#> A2   2.6666667  0  0  0  20
#> B1   1.6666667  0  0  0  30
#> B2   0.5555556  0  0  0  10
#> B3   1.1111111  0  0  0  20
#> C1   0.0000000  0  0  0   5
#> C2   0.0000000  0  0  0   0
#> C3   0.0000000  0  0  0  10
#> C4   0.0000000  0  0  0   5
#> D1   0.0000000  0  0  0   5
#> D2   0.0000000  0  0  0   5
#> D3   0.0000000  0  0  0  10
#> Sum 10.0000000  0  0  0 150
```
