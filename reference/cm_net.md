# Conditional maximization routine for the indirect estimation of origin-destination-type migration flow tables with known net migration totals.

The `cm_net` function finds the maximum likelihood estimates for fitted
values in the log-linear model: \$\$\log y\_{ij} = \log \alpha\_{i} +
\log \alpha\_{i}^{-1} + \log m\_{ij} \$\$

## Usage

``` r
cm_net(
  net_tot = NULL,
  m = NULL,
  tol = 1e-06,
  maxit = 500,
  verbose = TRUE,
  alpha0 = rep(1, length(net_tot))
)
```

## Arguments

- net_tot:

  Vector of net migration totals to constrain the sum of the imputed
  cell row and columns. Elements must sum to zero.

- m:

  Array of auxiliary data. By default, set to 1 for all
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

- alpha0:

  Vector of initial estimates for alpha

## Value

Conditional maximisation routine set up using the partial likelihood
derivatives. The argument `net_tot` takes the known net migration
totals. The user must ensure that the net migration totals sum globally
to zero.

Returns a `list` object with

- mu :

  Array of indirect estimates of origin-destination matrices by migrant
  characteristic

- it :

  Iteration count

- tol :

  Tolerance level at final iteration

## Author

Guy J. Abel, Peter W. F. Smith

## Examples

``` r
m <- matrix(data = 1:16, nrow = 4)
# m[lower.tri(m)] <- t(m)[lower.tri(m)]
addmargins(m)
#>                 Sum
#>      1  5  9 13  28
#>      2  6 10 14  32
#>      3  7 11 15  36
#>      4  8 12 16  40
#> Sum 10 26 42 58 136
sum_net(m)
#> [1] -18  -6   6  18

y <- cm_net(net_tot = c(30, 40, -15, -55), m = m)
#> iteration: 0 
#> alpha parameters: 1 1 1 1 
#> 
#> iteration: 1 
#> alpha parameters: 0.2668645 0.4718705 1.308365 2.074099 
#> 
#> iteration: 2 
#> alpha parameters: 0.3411136 0.5168407 1.249673 1.830115 
#> 
#> iteration: 3 
#> alpha parameters: 0.321874 0.4988266 1.264682 1.897978 
#> 
#> iteration: 4 
#> alpha parameters: 0.3269606 0.5034982 1.260477 1.879734 
#> 
#> iteration: 5 
#> alpha parameters: 0.3255818 0.5022216 1.261608 1.884656 
#> 
#> iteration: 6 
#> alpha parameters: 0.3259533 0.5025649 1.261303 1.883327 
#> 
#> iteration: 7 
#> alpha parameters: 0.325853 0.5024722 1.261385 1.883686 
#> 
#> iteration: 8 
#> alpha parameters: 0.3258801 0.5024972 1.261363 1.883589 
#> 
#> iteration: 9 
#> alpha parameters: 0.3258728 0.5024905 1.261369 1.883615 
#> 
#> iteration: 10 
#> alpha parameters: 0.3258748 0.5024923 1.261367 1.883608 
#> 
#> iteration: 11 
#> alpha parameters: 0.3258742 0.5024918 1.261368 1.88361 
#> 
addmargins(y$n)
#>      dest
#> orig          A         B         C         D        Sum
#>   A    1.000000  3.242583  2.325150  2.249068   8.816802
#>   B    3.083961  6.000000  3.983707  3.734790  16.802458
#>   C   11.612152 17.571573 11.000000 10.044818  50.228543
#>   D   23.120683 29.988296 17.919688 16.000000  87.028668
#>   Sum 38.816796 56.802453 35.228545 32.028677 162.876471
sum_net(y$n)
#>         A         B         C         D 
#>  29.99999  39.99999 -15.00000 -54.99999 

m <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
            nrow = 4, ncol = 4, byrow = TRUE,
            dimnames = list(orig = LETTERS[1:4], dest = LETTERS[1:4]))
addmargins(m)
#>      dest
#> orig    A   B  C   D Sum
#>   A     0 100 30  70 200
#>   B    50   0 45   5 100
#>   C    60  35  0  40 135
#>   D    20  25 20   0  65
#>   Sum 130 160 95 115 500
sum_net(m)
#>   A   B   C   D 
#> -70  60 -40  50 

y <- cm_net(net_tot = c(-100, 125, -75, 50), m = m)
#> iteration: 0 
#> alpha parameters: 1 1 1 1 
#> 
#> iteration: 1 
#> alpha parameters: 1.094097 0.7858951 1.161443 1 
#> 
#> iteration: 2 
#> alpha parameters: 1.016884 0.8650374 1.10878 1.051379 
#> 
#> iteration: 3 
#> alpha parameters: 1.064952 0.8226093 1.128406 1.017637 
#> 
#> iteration: 4 
#> alpha parameters: 1.035856 0.8460655 1.11986 1.037046 
#> 
#> iteration: 5 
#> alpha parameters: 1.052807 0.8325 1.124076 1.026025 
#> 
#> iteration: 6 
#> alpha parameters: 1.042904 0.8402779 1.121866 1.032308 
#> 
#> iteration: 7 
#> alpha parameters: 1.048632 0.8357758 1.123085 1.028704 
#> 
#> iteration: 8 
#> alpha parameters: 1.045312 0.8383713 1.1224 1.030778 
#> 
#> iteration: 9 
#> alpha parameters: 1.04723 0.8368707 1.122791 1.029582 
#> 
#> iteration: 10 
#> alpha parameters: 1.046121 0.837737 1.122567 1.030272 
#> 
#> iteration: 11 
#> alpha parameters: 1.046762 0.8372364 1.122696 1.029874 
#> 
#> iteration: 12 
#> alpha parameters: 1.046392 0.8375255 1.122622 1.030104 
#> 
#> iteration: 13 
#> alpha parameters: 1.046606 0.8373585 1.122665 1.029971 
#> 
#> iteration: 14 
#> alpha parameters: 1.046482 0.837455 1.12264 1.030048 
#> 
#> iteration: 15 
#> alpha parameters: 1.046554 0.8373992 1.122654 1.030003 
#> 
#> iteration: 16 
#> alpha parameters: 1.046512 0.8374314 1.122646 1.030029 
#> 
#> iteration: 17 
#> alpha parameters: 1.046536 0.8374128 1.122651 1.030014 
#> 
#> iteration: 18 
#> alpha parameters: 1.046522 0.8374236 1.122648 1.030023 
#> 
#> iteration: 19 
#> alpha parameters: 1.04653 0.8374174 1.122649 1.030018 
#> 
#> iteration: 20 
#> alpha parameters: 1.046526 0.837421 1.122648 1.030021 
#> 
addmargins(y$n)
#>      dest
#> orig          A         B        C          D       Sum
#>   A     0.00000 124.97056 27.96585  71.121910 224.05832
#>   B    40.00942   0.00000 33.56693   4.065067  77.64142
#>   C    64.36422  46.92119  0.00000  43.597199 154.88260
#>   D    19.68451  30.74980 18.34980   0.000000  68.78412
#>   Sum 124.05815 202.64155 79.88258 118.784175 525.36645
sum_net(y$n)
#>          A          B          C          D 
#> -100.00017  125.00013  -75.00002   50.00006 
```
