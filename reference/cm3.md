# Conditional maximization routine for the indirect estimation of origin-destination-migrant type migration flow tables with known origin and destination margins.

The `cm3` function finds the maximum likelihood estimates for parameters
in the log-linear model: \$\$ \log y\_{ijk} = \log \alpha\_{i} + \log
\beta\_{j} + \log m\_{ijk} \$\$ as introduced by Abel (2005). The
\\\alpha\_{i}\\ and \\\beta\_{j}\\ represent background information
related to the characteristics of the origin and destinations
respectively. The \\m\_{ijk}\\ factor represents auxiliary information
on origin-destination migration flows by a migrant characteristic (such
as age, sex, disability, household type, economic status, etc.). This
method is useful for combining data from detailed data collection
processes (such as a Census) with more up-to-date information on
migration inflows and outflows (where details on movements by migrant
characteristics are not known).

## Usage

``` r
cm3(
  row_tot = NULL,
  col_tot = NULL,
  m = NULL,
  tol = 1e-06,
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
  origin-destination-migrant typology combinations.

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

Parameter estimates were obtained using the conditional maximization of
the likelihood, as discussed by Abel (2005) and Raymer et. al. (2007).

The user must ensure that the row and column totals are equal in sum.
Care must also be taken to allow the row and column dimension of the
auxiliary matrix (`m`) to equal those provided in the row and column
totals.

Returns a `list` object with

- N :

  Origin-Destination matrix of indirect estimates

- theta :

  Collection of parameter estimates

## References

Abel, G. J. (2005) *The Indirect Estimation of Elderly Migrant Flows in
England and Wales* (MS.c. Thesis). University of Southampton

Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and
registration data to estimate detailed elderly migration flows in
England and Wales. *Journal of the Royal Statistical Society: Series A
(Statistics in Society)* 170 (4), 891–908.

## See also

[`cm2`](cm2.md), [`ipf3`](ipf3.md)

## Author

Guy J. Abel

## Examples

``` r
## over two tables
r <- LETTERS[1:2]
y <- cm3(row_tot = c(18, 20) * 2, col_tot = c(16, 22) * 2,
         m = array(c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2),
                   dimnames = list(orig = r, dest = r, type = c("ILL", "HEALTHY"))))
#> iteration: 0 
#> alpha parameters: 
#> beta parameters: 1 1 
#> 
#> iteration: 1 
#> alpha parameters: 2.25 2.105263 
#> beta parameters: 1.204557 0.8900719 
#> max difference: 1.25 
#> 
#> iteration: 2 
#> alpha parameters: 2.108776 2.240291 
#> beta parameters: 1.245143 0.8694605 
#> max difference: 0.1412239 
#> 
#> iteration: 3 
#> alpha parameters: 2.081826 2.2667 
#> beta parameters: 1.253107 0.8654605 
#> max difference: 0.02694999 
#> 
#> iteration: 4 
#> alpha parameters: 2.076582 2.271864 
#> beta parameters: 1.254664 0.8646796 
#> max difference: 0.005244326 
#> 
#> iteration: 5 
#> alpha parameters: 2.075558 2.272874 
#> beta parameters: 1.254969 0.864527 
#> max difference: 0.001024239 
#> 
#> iteration: 6 
#> alpha parameters: 2.075357 2.273071 
#> beta parameters: 1.255028 0.8644972 
#> max difference: 0.0002001792 
#> 
#> iteration: 7 
#> alpha parameters: 2.075318 2.273109 
#> beta parameters: 1.25504 0.8644914 
#> max difference: 3.912878e-05 
#> 
#> iteration: 8 
#> alpha parameters: 2.075311 2.273117 
#> beta parameters: 1.255042 0.8644902 
#> max difference: 7.648661e-06 
#> 
#> iteration: 9 
#> alpha parameters: 2.075309 2.273118 
#> beta parameters: 1.255043 0.86449 
#> max difference: 1.495122e-06 
#> 
#> iteration: 10 
#> alpha parameters: 2.075309 2.273119 
#> beta parameters: 1.255043 0.86449 
#> max difference: 2.922594e-07 
#> 
# display with row, col and table totals
y
#> $n
#> , , type = ILL
#> 
#>     dest
#> orig         A         B
#>    A 13.023008  3.588167
#>    B  2.852862 13.755619
#> 
#> , , type = HEALTHY
#> 
#>     dest
#> orig         A         B
#>    A 10.418407  8.970418
#>    B  5.705723 17.685795
#> 
#> 
#> $theta
#>   alpha1   alpha2    beta1    beta2 
#> 2.075309 2.273119 1.255043 0.864490 
#> 

## over three tables
y <- cm3(row_tot = c(170, 120, 410), col_tot = c(500, 140, 60),
         m = array(c(5, 1, 2, 7,  4, 2, 5, 9,  5, 4, 3, 1), dim = c(2, 2, 3),
                   dimnames = list(orig = r, dest = r, type = c("0--15", "15-60", ">60"))),
                   verbose = FALSE)
# display with row, col and table totals
y
#> $n
#> , , type = 0--15
#> 
#>     dest
#> orig        A         B
#>    A 116.2709  9.922764
#>    B  24.9202 37.217838
#> 
#> , , type = 15-60
#> 
#>     dest
#> orig        A        B
#>    A 93.01674 24.80691
#>    B 49.84040 47.85151
#> 
#> , , type = >60
#> 
#>     dest
#> orig        A         B
#>    A 116.2709 14.884147
#>    B  99.6808  5.316834
#> 
#> 
#> $theta
#>        alpha1        alpha2         beta1         beta2 
#> 3.798846e-171 4.071009e-171 6.121381e+171 1.306024e+171 
#> 
```
