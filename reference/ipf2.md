# Iterative proportional fitting routine for the indirect estimation of origin-destination migration flow table with known margins.

The `ipf2` function finds the maximum likelihood estimates for fitted
values in the log-linear model: \$\$\log y\_{ij} = \log \alpha\_{i} +
\log \beta\_{j} + \log m\_{ij} \$\$ where \\m\_{ij}\\ is a set of prior
estimates for \\y\_{ij}\\ and itself is no more complex than the one
being fitted.

## Usage

``` r
ipf2(
  row_tot = NULL,
  col_tot = NULL,
  m = matrix(1, length(row_tot), length(col_tot)),
  tol = 1e-05,
  maxit = 500,
  verbose = FALSE
)
```

## Arguments

- row_tot:

  Vector of origin totals to constrain the sum of the imputed cell rows.

- col_tot:

  Vector of destination totals to constrain the sum of the imputed cell
  columns.

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

## Value

Iterative Proportional Fitting routine set up in a similar manner to
Agresti (2002, p.343). This is equivalent to a conditional maximization
of the likelihood, as discussed by Willekens (1999), and hence provides
identical indirect estimates to those obtained from the [`cm2`](cm2.md)
routine.

The user must ensure that the row and column totals are equal in sum.
Care must also be taken to allow the dimension of the auxiliary matrix
(`m`) to equal those provided in the row and column totals.

If only one of the margins is known, the function can still be run. The
indirect estimates will correspond to the log-linear model without the
\\\alpha\_{i}\\ term if (`row_tot = NULL`) or without the \\\beta\_{j}\\
term if (`col_tot = NULL`)

Returns a `list` object with

- mu :

  Origin-Destination matrix of indirect estimates

- it :

  Iteration count

- tol :

  Tolerance level at final iteration

## References

Agresti, A. (2002). *Categorical Data Analysis* 2nd edition. Wiley.

Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of
Migration Flows: From Entropy to EM. *Mathematical Population Studies* 7
(3), 239–78.

## See also

[`cm2`](cm2.md), [`ipf3`](ipf3.md)

## Author

Guy J. Abel

## Examples

``` r
## with Willekens (1999) data
dn <- LETTERS[1:2]
y <- ipf2(row_tot = c(18, 20), col_tot = c(16, 22), 
          m = matrix(c(5, 1, 2, 7), ncol = 2, 
                     dimnames = list(orig = dn, dest = dn)))
round(addmargins(y$mu),2)
#>      dest
#> orig      A     B Sum
#>   A   13.25  4.75  18
#>   B    2.75 17.25  20
#>   Sum 16.00 22.00  38

## with all elements of offset equal
y <- ipf2(row_tot = c(18, 20), col_tot = c(16, 22))
round(addmargins(y$mu),2)
#>                 Sum
#>      7.58 10.42  18
#>      8.42 11.58  20
#> Sum 16.00 22.00  38

## with bigger matrix
dn <- LETTERS[1:3]
y <- ipf2(row_tot = c(170, 120, 410), col_tot = c(500, 140, 60), 
          m = matrix(c(50, 10, 220, 120, 120, 30, 545, 0, 10), ncol = 3, 
                     dimnames = list(orig = dn, dest = dn)))
# display with row and col totals
round(addmargins(y$mu))
#>      dest
#> orig    A   B  C Sum
#>   A    72  40 59 170
#>   B    32  88  0 120
#>   C   396  12  1 410
#>   Sum 500 140 60 700

## only one margin known
dn <- LETTERS[1:2]
y <- ipf2(row_tot = c(18, 20), col_tot = NULL, 
          m = matrix(c(5, 1, 2, 7), ncol = 2, 
                     dimnames = list(orig = dn, dest = dn)))
round(addmargins(y$mu))
#>      dest
#> orig   A  B Sum
#>   A   13  5  18
#>   B    2 18  20
#>   Sum 15 23  38
```
