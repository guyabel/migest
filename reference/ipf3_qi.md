# Iterative proportional fitting routine for the indirect estimation of origin-destination-migrant type migration flow tables with known origin and destination margins and diagonal elements.

This function is predominantly intended to be used within the
[`ffs`](ffs_demo.md) routine.

## Usage

``` r
ipf3_qi(
  row_tot = NULL,
  col_tot = NULL,
  diag_count = NULL,
  m = NULL,
  speed = TRUE,
  tol = 1e-05,
  maxit = 500,
  verbose = TRUE
)
```

## Arguments

- row_tot:

  Vector of origin totals to constrain the sum of the imputed cell rows.

- col_tot:

  Vector of destination totals to constrain the sum of the imputed cell
  columns.

- diag_count:

  Array with counts on diagonal to constrain diagonal elements of the
  indirect estimates too. By default these are taken as their maximum
  possible values given the relevant margins totals in each table. If
  user specifies their own array of diagonal totals, values on the
  non-diagonals in the array can take any positive number (they are
  ultimately ignored).

- m:

  Array of auxiliary data. By default set to 1 for all
  origin-destination-migrant typologies combinations.

- speed:

  Speeds up the IPF algorithm by minimizing sufficient statistics.

- tol:

  Numeric value for the tolerance level used in the parameter
  estimation.

- maxit:

  Numeric value for the maximum number of iterations used in the
  parameter estimation.

- verbose:

  Logical value to indicate the print the parameter estimates at each
  iteration. By default `FALSE`.

## Value

Iterative Proportional Fitting routine set up using the partial
likelihood derivatives illustrated in Abel (2013). The arguments
`row_tot` and `col_tot` take the row-table and column-table specific
known margins. By default the diagonal values are taken as their maximum
possible values given the relevant margins totals in each table.
Diagonal values can be added by the user, but care must be taken to
ensure resulting diagonals are feasible given the set of margins.

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

## Details

The `ipf3` function finds the maximum likelihood estimates for fitted
values in the log-linear model: \$\$ \log y\_{ijk} = \log \alpha\_{i} +
\log \beta\_{j} + \log \lambda\_{k} + \log \gamma\_{ik} + \log
\kappa\_{jk} + \log \delta\_{ijk}I(i=j) + \log m\_{ijk} \$\$ where
\\m\_{ijk}\\ is a set of prior estimates for \\y\_{ijk}\\ and is no more
complex than the matrices being fitted. The \\\delta\_{ijk}I(i=j)\\ term
ensures a saturated fit on the diagonal elements of each \\(i,j)\\
matrix.

## References

Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place
of Birth. *Demographic Research* 28, (18) 505-546

## See also

[`ipf3`](ipf3.md), [`ffs_demo`](ffs_demo.md)

## Author

Guy J. Abel

## Examples

``` r
# \donttest{
## create row-table and column-table specific known margins.
dn <- LETTERS[1:4]
P1 <- matrix(c(1000, 100,  10,   0, 
               55,   555,  50,   5, 
               80,    40, 800 , 40, 
               20,    25,  20, 200), 
             nrow = 4, ncol = 4, byrow = TRUE, 
             dimnames = list(pob = dn, por = dn))
P2 <- matrix(c(950, 100,  60,   0, 
                80, 505,  75,   5, 
                90,  30, 800,  40, 
                40,  45,   0, 180), 
             nrow = 4, ncol = 4, byrow = TRUE, 
             dimnames = list(pob = dn, por = dn))
# display with row and col totals
addmargins(P1)
#>      por
#> pob      A   B   C   D  Sum
#>   A   1000 100  10   0 1110
#>   B     55 555  50   5  665
#>   C     80  40 800  40  960
#>   D     20  25  20 200  265
#>   Sum 1155 720 880 245 3000
addmargins(P2)
#>      por
#> pob      A   B   C   D  Sum
#>   A    950 100  60   0 1110
#>   B     80 505  75   5  665
#>   C     90  30 800  40  960
#>   D     40  45   0 180  265
#>   Sum 1160 680 935 225 3000

# # run ipf
# y <- ipf3_qi(row_tot = t(P1), col_tot = P2)
# # display with row, col and table totals
# round(addmargins(y$mu), 1)
# # origin-destination flow table
# round(sum_od(y$mu), 1)

## with alternative offset term
# dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 4))
# y <- ipf3_qi(row_tot = t(P1), col_tot = P2, m = dis)
# # display with row, col and table totals
# round(addmargins(y$mu), 1)
# # origin-destination flow table
# round(sum_od(y$mu), 1)
# } 
```
