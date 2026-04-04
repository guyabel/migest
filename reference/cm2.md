# Conditional maximization routine for the indirect estimation of origin-destination migration flow table with known margins

The `cm2` function finds the maximum likelihood estimates for parameters
in the log-linear model: \$\$ \log y\_{ij} = \log \alpha_i + \log
\beta_j + \log m\_{ij} \$\$ as introduced by Willekens (1999). The
\\\alpha_i\\ and \\\beta_j\\ represent background information related to
the characteristics of the origin and destinations respectively. The
\\m\_{ij}\\ factor represents auxiliary information on migration flows,
which imposes its interaction structure onto the estimated flow matrix.

## Usage

``` r
cm2(
  row_tot = NULL,
  col_tot = NULL,
  m = matrix(data = 1, nrow = length(row_tot), ncol = length(col_tot)),
  tol = 1e-06,
  maxit = 500,
  verbose = TRUE,
  rtot = row_tot,
  ctot = col_tot
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

- rtot:

  Depreciated. Use `row_tot`

- ctot:

  Depreciated. Use `col_tot`

## Value

Parameter estimates are obtained using the EM algorithm outlined in
Willekens (1999). This is equivalent to a conditional maximization of
the likelihood, as discussed by Raymer et. al. (2007). It also provides
identical indirect estimates to those obtained from the
[`ipf2`](ipf2.md) routine.

The user must ensure that the row and column totals are equal in sum.
Care must also be taken to allow the dimension of the auxiliary matrix
(`m`) to equal those provided in the row (`row_tot`) and column
(`col_tot`) arguments.

Returns a `list` object with

- N :

  Origin-Destination matrix of indirect estimates

- theta :

  Collection of parameter estimates

## References

Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and
registration data to estimate detailed elderly migration flows in
England and Wales. *Journal of the Royal Statistical Society: Series A
(Statistics in Society)* 170 (4), 891–908.

Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of
Migration Flows: From Entropy to EM. *Mathematical Population Studies* 7
(3), 239–78.

## See also

[`ipf2`](ipf2.md)

## Author

Guy J. Abel

## Examples

``` r
## with Willekens (1999) data
r <- LETTERS[1:2]
y <- cm2(row_tot = c(18, 20), col_tot = c(16, 22), 
         m = matrix(c(5, 1, 2, 7), ncol = 2, dimnames = list(orig = r, dest = r)))
#> iteration: 0 
#> alpha parameters: 
#> beta parameters: 1 1 
#> 
#> iteration: 1 
#> alpha parameters: 2.571429 2.5 
#> beta parameters: 1.04186 0.9716088 
#> max difference: 1.571429 
#> 
#> iteration: 2 
#> alpha parameters: 2.516596 2.550005 
#> beta parameters: 1.057293 0.9614029 
#> max difference: 0.05483302 
#> 
#> iteration: 3 
#> alpha parameters: 2.496785 2.568346 
#> beta parameters: 1.062963 0.9576881 
#> max difference: 0.01981086 
#> 
#> iteration: 4 
#> alpha parameters: 2.489561 2.57507 
#> beta parameters: 1.065042 0.95633 
#> max difference: 0.00722337 
#> 
#> iteration: 5 
#> alpha parameters: 2.486919 2.577535 
#> beta parameters: 1.065805 0.9558326 
#> max difference: 0.00264242 
#> 
#> iteration: 6 
#> alpha parameters: 2.485951 2.578438 
#> beta parameters: 1.066084 0.9556504 
#> max difference: 0.000967792 
#> 
#> iteration: 7 
#> alpha parameters: 2.485597 2.578769 
#> beta parameters: 1.066187 0.9555837 
#> max difference: 0.0003546105 
#> 
#> iteration: 8 
#> alpha parameters: 2.485467 2.578891 
#> beta parameters: 1.066224 0.9555592 
#> max difference: 0.0001299542 
#> 
#> iteration: 9 
#> alpha parameters: 2.485419 2.578935 
#> beta parameters: 1.066238 0.9555502 
#> max difference: 4.762717e-05 
#> 
#> iteration: 10 
#> alpha parameters: 2.485401 2.578951 
#> beta parameters: 1.066243 0.9555469 
#> max difference: 1.745534e-05 
#> 
#> iteration: 11 
#> alpha parameters: 2.485395 2.578957 
#> beta parameters: 1.066245 0.9555457 
#> max difference: 6.397427e-06 
#> 
#> iteration: 12 
#> alpha parameters: 2.485393 2.57896 
#> beta parameters: 1.066246 0.9555453 
#> max difference: 2.34468e-06 
#> 
#> iteration: 13 
#> alpha parameters: 2.485392 2.57896 
#> beta parameters: 1.066246 0.9555451 
#> max difference: 8.593346e-07 
#> 
y
#> $n
#>     dest
#> orig         A         B
#>    A 13.250194  4.749808
#>    B  2.749806 17.250192
#> 
#> $theta
#>    alpha1    alpha2     beta1     beta2 
#> 2.4853919 2.5789604 1.0662459 0.9555451 
#> 

## with all elements of offset equal (independence fit)
y <- cm2(row_tot = c(18, 20), col_tot = c(16, 22))
#> iteration: 0 
#> alpha parameters: 
#> beta parameters: 1 1 
#> 
#> iteration: 1 
#> alpha parameters: 9 10 
#> beta parameters: 0.8421053 1.157895 
#> max difference: 9 
#> 
#> iteration: 2 
#> alpha parameters: 9 10 
#> beta parameters: 0.8421053 1.157895 
#> max difference: 0 
#> 
y
#> $n
#>          [,1]     [,2]
#> [1,] 7.578947 10.42105
#> [2,] 8.421053 11.57895
#> 
#> $theta
#>     alpha1     alpha2      beta1      beta2 
#>  9.0000000 10.0000000  0.8421053  1.1578947 
#> 

## with bigger matrix
r <- LETTERS[1:4]
y <- cm2(row_tot = c(250, 100, 140, 110), col_tot = c(150, 150, 180, 120),
         m = matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
                    nrow = 4, ncol = 4, dimnames = list(orig = r, dest = r), byrow = TRUE))
#> iteration: 0 
#> alpha parameters: 
#> beta parameters: 1 1 1 1 
#> 
#> iteration: 1 
#> alpha parameters: 1.25 1 1.037037 1.692308 
#> beta parameters: 1.026916 0.7367243 1.547107 0.8956462 
#> max difference: 0.6923077 
#> 
#> iteration: 2 
#> alpha parameters: 1.367758 0.7971692 1.136122 1.573709 
#> beta parameters: 1.075269 0.6948215 1.66083 0.8265955 
#> max difference: 0.2028308 
#> 
#> iteration: 3 
#> alpha parameters: 1.411084 0.7539557 1.148494 1.525817 
#> beta parameters: 1.093902 0.6835234 1.685758 0.80816 
#> max difference: 0.04789161 
#> 
#> iteration: 4 
#> alpha parameters: 1.424531 0.7429695 1.148635 1.513457 
#> beta parameters: 1.100226 0.6802975 1.691114 0.8033342 
#> max difference: 0.01344736 
#> 
#> iteration: 5 
#> alpha parameters: 1.428599 0.7400386 1.147943 1.510279 
#> beta parameters: 1.10226 0.6793635 1.692281 0.8020325 
#> max difference: 0.004067991 
#> 
#> iteration: 6 
#> alpha parameters: 1.429821 0.7392305 1.147592 1.509436 
#> beta parameters: 1.102895 0.6790904 1.692545 0.8016713 
#> max difference: 0.001221511 
#> 
#> iteration: 7 
#> alpha parameters: 1.430186 0.7390021 1.147459 1.509205 
#> beta parameters: 1.103089 0.67901 1.692608 0.8015688 
#> max difference: 0.0003654829 
#> 
#> iteration: 8 
#> alpha parameters: 1.430296 0.7389364 1.147414 1.50914 
#> beta parameters: 1.103148 0.6789863 1.692623 0.8015392 
#> max difference: 0.0001091228 
#> 
#> iteration: 9 
#> alpha parameters: 1.430328 0.7389173 1.1474 1.509122 
#> beta parameters: 1.103166 0.6789792 1.692627 0.8015306 
#> max difference: 3.253749e-05 
#> 
#> iteration: 10 
#> alpha parameters: 1.430338 0.7389117 1.147396 1.509116 
#> beta parameters: 1.103171 0.6789772 1.692628 0.8015281 
#> max difference: 9.693573e-06 
#> 
#> iteration: 11 
#> alpha parameters: 1.430341 0.73891 1.147394 1.509115 
#> beta parameters: 1.103173 0.6789765 1.692629 0.8015273 
#> max difference: 2.886352e-06 
#> 
#> iteration: 12 
#> alpha parameters: 1.430342 0.7389095 1.147394 1.509114 
#> beta parameters: 1.103173 0.6789764 1.692629 0.8015271 
#> max difference: 8.591439e-07 
#> 
                    
# display with row and col totals
round(addmargins(y$n)) 
#>      dest
#> orig    A   B   C   D Sum
#>   A     0  97  73  80 250
#>   B    41   0  56   3 100
#>   C    76  27   0  37 140
#>   D    33  26  51   0 110
#>   Sum 150 150 180 120 600
```
