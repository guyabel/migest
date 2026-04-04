# Iterative proportional fitting routine for the indirect estimation of origin-destination-migrant type migration flow tables with known origin and destination margins.

The `ipf3` function finds the maximum likelihood estimates for fitted
values in the log-linear model: \$\$ \log y\_{ijk} = \log \alpha\_{i} +
\log \beta\_{j} + \log \lambda\_{k} + \log \gamma\_{ik} + \log
\kappa\_{jk} + \log m\_{ijk} \$\$ where \\m\_{ijk}\\ is a set of prior
estimates for \\y\_{ijk}\\ and is no more complex than the matrices
being fitted.

## Usage

``` r
ipf3(
  row_tot = NULL,
  col_tot = NULL,
  m = NULL,
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

- m:

  Array of auxiliary data. By default set to 1 for all
  origin-destination-migrant typologies combinations.

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
Agresti (2002, p.343). The arguments `row_tot` and `col_tot` take the
row-table and column-table specific known margins.

The user must ensure that the row and column totals in each table sum to
the same value. Care must also be taken to allow the dimension of the
auxiliary matrix (`m`) to equal those provided in the row and column
totals.

Returns a `list` object with

- mu :

  Array of indirect estimates of origin-destination matrices by migrant
  characteristic

- it :

  Iteration count

- tol :

  Tolerance level at final iteration

## References

Abel and Cohen (2019) Bilateral international migration flow estimates
for 200 countries *Scientific Data* 6 (1), 1-13

Azose & Raftery (2019) Estimation of emigration, return migration, and
transit migration between all pairs of countries *Proceedings of the
National Academy of Sciences* 116 (1) 116-122

Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place
of Birth. *Demographic Research* 28, (18) 505-546

Agresti, A. (2002). *Categorical Data Analysis* 2nd edition. Wiley.

## See also

[`ipf3_qi`](ipf3_qi.md), [`ipf2`](ipf2.md)

## Author

Guy J. Abel

## Examples

``` r
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

# run ipf
y <- ipf3(row_tot = t(P1), col_tot = P2)
#> 1 996 
#> 2 1.136868e-13 
# display with row, col and table totals
round(addmargins(y$mu), 1)
#> , , pob = A
#> 
#>      dest
#> orig      A     B    C D  Sum
#>   A   855.9  90.1 54.1 0 1000
#>   B    85.6   9.0  5.4 0  100
#>   C     8.6   0.9  0.5 0   10
#>   D     0.0   0.0  0.0 0    0
#>   Sum 950.0 100.0 60.0 0 1110
#> 
#> , , pob = B
#> 
#>      dest
#> orig     A     B    C   D Sum
#>   A    6.6  41.8  6.2 0.4  55
#>   B   66.8 421.5 62.6 4.2 555
#>   C    6.0  38.0  5.6 0.4  50
#>   D    0.6   3.8  0.6 0.0   5
#>   Sum 80.0 505.0 75.0 5.0 665
#> 
#> , , pob = C
#> 
#>      dest
#> orig     A    B     C    D Sum
#>   A    7.5  2.5  66.7  3.3  80
#>   B    3.8  1.3  33.3  1.7  40
#>   C   75.0 25.0 666.7 33.3 800
#>   D    3.8  1.3  33.3  1.7  40
#>   Sum 90.0 30.0 800.0 40.0 960
#> 
#> , , pob = D
#> 
#>      dest
#> orig     A    B C     D Sum
#>   A    3.0  3.4 0  13.6  20
#>   B    3.8  4.2 0  17.0  25
#>   C    3.0  3.4 0  13.6  20
#>   D   30.2 34.0 0 135.8 200
#>   Sum 40.0 45.0 0 180.0 265
#> 
#> , , pob = Sum
#> 
#>      dest
#> orig       A     B     C     D  Sum
#>   A    873.0 137.8 126.9  17.3 1155
#>   B    159.9 436.0 101.3  22.8  720
#>   C     92.6  67.3 672.8  47.3  880
#>   D     34.5  39.0  33.9 137.6  245
#>   Sum 1160.0 680.0 935.0 225.0 3000
#> 
# origin-destination flow table
round(sum_od(y$mu), 1)
#>      dest
#> orig      A     B     C    D   Sum
#>   A     0.0 137.8 126.9 17.3 282.0
#>   B   159.9   0.0 101.3 22.8 284.0
#>   C    92.6  67.3   0.0 47.3 207.2
#>   D    34.5  39.0  33.9  0.0 107.4
#>   Sum 287.0 244.0 262.2 87.4 880.6

## with alternative offset term
dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 4))
y <- ipf3(row_tot = t(P1), col_tot = P2, m = dis)
#> 1 990 
#> 2 267.9774 
#> 3 13.70557 
#> 4 1.609235 
#> 5 0.176649 
#> 6 0.01924174 
#> 7 0.002094159 
#> 8 0.000227895 
#> 9 2.480023e-05 
#> 10 2.698833e-06 
# display with row, col and table totals
round(addmargins(y$mu), 1)
#> , , pob = A
#> 
#>      dest
#> orig      A     B    C D  Sum
#>   A   847.7  96.5 55.8 0 1000
#>   B    93.3   2.7  4.1 0  100
#>   C     9.1   0.9  0.1 0   10
#>   D     0.0   0.0  0.0 0    0
#>   Sum 950.0 100.0 60.0 0 1110
#> 
#> , , pob = B
#> 
#>      dest
#> orig     A     B    C   D Sum
#>   A    2.3  49.3  3.2 0.2  55
#>   B   74.8 404.4 71.1 4.7 555
#>   C    2.6  46.9  0.4 0.1  50
#>   D    0.3   4.5  0.2 0.0   5
#>   Sum 80.0 505.0 75.0 5.0 665
#> 
#> , , pob = C
#> 
#>      dest
#> orig     A    B     C    D Sum
#>   A    1.2  0.5  77.5  0.9  80
#>   B    0.9  0.1  38.6  0.5  40
#>   C   87.0 29.1 645.3 38.6 800
#>   D    1.0  0.3  38.7  0.0  40
#>   Sum 90.0 30.0 800.0 40.0 960
#> 
#> , , pob = D
#> 
#>      dest
#> orig     A    B C     D Sum
#>   A    0.4  0.6 0  19.0  20
#>   B    0.7  0.2 0  24.1  25
#>   C    0.6  0.8 0  18.5  20
#>   D   38.3 43.4 0 118.3 200
#>   Sum 40.0 45.0 0 180.0 265
#> 
#> , , pob = Sum
#> 
#>      dest
#> orig       A     B     C     D  Sum
#>   A    851.5 146.8 136.6  20.1 1155
#>   B    169.6 407.4 113.8  29.3  720
#>   C     99.3  77.7 645.8  57.3  880
#>   D     39.6  48.2  38.9 118.4  245
#>   Sum 1160.0 680.0 935.0 225.0 3000
#> 
# origin-destination flow table
round(sum_od(y$mu), 1)
#>      dest
#> orig      A     B     C     D   Sum
#>   A     0.0 146.8 136.6  20.1 303.5
#>   B   169.6   0.0 113.8  29.3 312.6
#>   C    99.3  77.7   0.0  57.3 234.2
#>   D    39.6  48.2  38.9   0.0 126.6
#>   Sum 308.5 272.6 289.2 106.6 977.0
```
