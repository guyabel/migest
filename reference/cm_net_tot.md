# Conditional maximization routine for the indirect estimation of origin-destination-type migration flow tables with known net migration and grand totals.

The `cm_net` function finds the maximum likelihood estimates for fitted
values in the log-linear model: \$\$\log y\_{ij} = \log \alpha\_{i} +
\log \alpha\_{i}^{-1} + \log m\_{ij} \$\$

## Usage

``` r
cm_net_tot(
  net_tot = NULL,
  tot = NULL,
  m = NULL,
  tol = 1e-06,
  maxit = 500,
  verbose = TRUE,
  alpha0 = rep(1, length(net_tot)),
  lambda0 = 1,
  alpha_constrained = TRUE
)
```

## Arguments

- net_tot:

  Vector of net migration totals to constrain the sum of the imputed
  cell row and columns. Elements must sum to zero.

- tot:

  Numeric value of grand total to constrain sum of all imputed cells.

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

- lambda0:

  Numeric value of initial estimates for lambda

- alpha_constrained:

  Logical value to indicate if the first alpha should be constrain to
  unity. By default `TRUE`

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

y <- cm_net_tot(net_tot = c(30, 40, -15, -55), tot = 200, m = m)
#> iteration: 0 
#> alpha parameters: 1 1 1 1 
#> lambda parameter: 1 
#> 
#> iteration: 1 
#> alpha parameters: 1 0.4718705 1.308365 2.074099 
#> lambda parameter: 1.470588 
#> 
#> iteration: 2 
#> alpha parameters: 1 0.7041053 1.419844 1.963206 
#> lambda parameter: 1.244108 
#> 
#> iteration: 3 
#> alpha parameters: 1 0.7039241 1.625029 2.377481 
#> lambda parameter: 1.414181 
#> 
#> iteration: 4 
#> alpha parameters: 1 0.8390437 1.766222 2.483845 
#> lambda parameter: 1.35715 
#> 
#> iteration: 5 
#> alpha parameters: 1 0.8875586 1.930152 2.749782 
#> lambda parameter: 1.402129 
#> 
#> iteration: 6 
#> alpha parameters: 1 0.9738902 2.067601 2.913777 
#> lambda parameter: 1.385633 
#> 
#> iteration: 7 
#> alpha parameters: 1 1.030103 2.203614 3.113081 
#> lambda parameter: 1.393776 
#> 
#> iteration: 8 
#> alpha parameters: 1 1.093015 2.324243 3.27301 
#> lambda parameter: 1.386325 
#> 
#> iteration: 9 
#> alpha parameters: 1 1.143593 2.436551 3.431774 
#> lambda parameter: 1.384671 
#> 
#> iteration: 10 
#> alpha parameters: 1 1.192199 2.537068 3.569476 
#> lambda parameter: 1.37922 
#> 
#> iteration: 11 
#> alpha parameters: 1 1.233947 2.6281 3.696884 
#> lambda parameter: 1.375277 
#> 
#> iteration: 12 
#> alpha parameters: 1 1.271882 2.709221 3.809386 
#> lambda parameter: 1.370603 
#> 
#> iteration: 13 
#> alpha parameters: 1 1.305011 2.781574 3.910514 
#> lambda parameter: 1.366532 
#> 
#> iteration: 14 
#> alpha parameters: 1 1.334422 2.8456 3.999813 
#> lambda parameter: 1.362544 
#> 
#> iteration: 15 
#> alpha parameters: 1 1.360144 2.902146 4.078939 
#> lambda parameter: 1.35897 
#> 
#> iteration: 16 
#> alpha parameters: 1 1.382726 2.951859 4.148502 
#> lambda parameter: 1.355681 
#> 
#> iteration: 17 
#> alpha parameters: 1 1.402423 2.995463 4.209617 
#> lambda parameter: 1.352744 
#> 
#> iteration: 18 
#> alpha parameters: 1 1.419607 3.033593 4.263088 
#> lambda parameter: 1.350111 
#> 
#> iteration: 19 
#> alpha parameters: 1 1.434549 3.066869 4.309798 
#> lambda parameter: 1.347779 
#> 
#> iteration: 20 
#> alpha parameters: 1 1.447532 3.095847 4.350499 
#> lambda parameter: 1.345718 
#> 
#> iteration: 30 
#> alpha parameters: 1 1.511314 3.239287 4.552373 
#> lambda parameter: 1.335161 
#> 
#> iteration: 40 
#> alpha parameters: 1 1.525818 3.272159 4.598732 
#> lambda parameter: 1.332664 
#> 
#> iteration: 50 
#> alpha parameters: 1 1.529049 3.279494 4.609081 
#> lambda parameter: 1.332103 
#> 
#> iteration: 60 
#> alpha parameters: 1 1.529765 3.281121 4.611377 
#> lambda parameter: 1.331979 
#> 
#> iteration: 70 
#> alpha parameters: 1 1.529924 3.281481 4.611886 
#> lambda parameter: 1.331951 
#> 
#> iteration: 80 
#> alpha parameters: 1 1.529959 3.281561 4.611999 
#> lambda parameter: 1.331945 
#> 
#> iteration: 90 
#> alpha parameters: 1 1.529967 3.281579 4.612024 
#> lambda parameter: 1.331944 
#> 
addmargins(y$n)
#>      dest
#> orig          A         B         C         D       Sum
#>   A    1.331944  4.352851  3.652964  3.754375  13.09213
#>   B    4.075660  7.991663  6.209906  6.185921  24.46315
#>   C   13.112639 19.997919 14.651381 14.215708  61.97765
#>   D   24.571829 32.120731 22.463416 21.311100 100.46708
#>   Sum 43.092072 64.463165 46.977667 45.467104 200.00001
sum_net(y$n)
#>         A         B         C         D 
#>  29.99994  40.00002 -14.99998 -54.99997 

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

y <- cm_net_tot(net_tot = c(-100, 125, -75, 50), tot = 600, m = m)
#> iteration: 0 
#> alpha parameters: 1 1 1 1 
#> lambda parameter: 1 
#> 
#> iteration: 1 
#> alpha parameters: 1 0.7858951 1.161443 1 
#> lambda parameter: 1.2 
#> 
#> iteration: 2 
#> alpha parameters: 1 0.8851468 1.014562 1.052841 
#> lambda parameter: 1.124338 
#> 
#> iteration: 3 
#> alpha parameters: 1 0.8346426 1.090831 1.014231 
#> lambda parameter: 1.181481 
#> 
#> iteration: 4 
#> alpha parameters: 1 0.865191 1.044125 1.039642 
#> lambda parameter: 1.153662 
#> 
#> iteration: 5 
#> alpha parameters: 1 0.8489046 1.070983 1.02594 
#> lambda parameter: 1.17151 
#> 
#> iteration: 6 
#> alpha parameters: 1 0.8589402 1.055837 1.03464 
#> lambda parameter: 1.161992 
#> 
#> iteration: 7 
#> alpha parameters: 1 0.8535558 1.064845 1.030086 
#> lambda parameter: 1.167777 
#> 
#> iteration: 8 
#> alpha parameters: 1 0.8568633 1.059868 1.032968 
#> lambda parameter: 1.164594 
#> 
#> iteration: 9 
#> alpha parameters: 1 0.8550793 1.062853 1.031458 
#> lambda parameter: 1.166491 
#> 
#> iteration: 10 
#> alpha parameters: 1 0.8561689 1.061209 1.032408 
#> lambda parameter: 1.165434 
#> 
#> iteration: 11 
#> alpha parameters: 1 0.8555778 1.062194 1.031907 
#> lambda parameter: 1.166059 
#> 
#> iteration: 12 
#> alpha parameters: 1 0.8559368 1.061651 1.032219 
#> lambda parameter: 1.165709 
#> 
#> iteration: 13 
#> alpha parameters: 1 0.855741 1.061976 1.032053 
#> lambda parameter: 1.165915 
#> 
#> iteration: 14 
#> alpha parameters: 1 0.8558593 1.061796 1.032156 
#> lambda parameter: 1.165799 
#> 
#> iteration: 15 
#> alpha parameters: 1 0.8557944 1.061903 1.032101 
#> lambda parameter: 1.165867 
#> 
#> iteration: 16 
#> alpha parameters: 1 0.8558334 1.061843 1.032135 
#> lambda parameter: 1.165828 
#> 
#> iteration: 17 
#> alpha parameters: 1 0.8558119 1.061879 1.032117 
#> lambda parameter: 1.165851 
#> 
#> iteration: 18 
#> alpha parameters: 1 0.8558248 1.061859 1.032128 
#> lambda parameter: 1.165838 
#> 
#> iteration: 19 
#> alpha parameters: 1 0.8558177 1.061871 1.032122 
#> lambda parameter: 1.165846 
#> 
#> iteration: 20 
#> alpha parameters: 1 0.8558219 1.061864 1.032126 
#> lambda parameter: 1.165841 
#> 
addmargins(y$n)
#>      dest
#> orig          A         B        C          D       Sum
#>   A     0.00000 136.22513 32.93756  79.068944 248.23163
#>   B    49.88761   0.00000 42.28296   4.833488  97.00406
#>   C    74.27815  50.62851  0.00000  47.977516 172.88418
#>   D    24.06590  35.15032 22.66377   0.000000  81.87999
#>   Sum 148.23166 222.00396 97.88429 131.879947 599.99986
sum_net(y$n)
#>         A         B         C         D 
#> -99.99997 124.99990 -74.99989  49.99995 
```
