# Scale native born populations to match global differences in births and deaths over period

This function is predominantly intended to be used within the ffs
routines in the migest package. Adjustment to ensure that global
differences in stocks match the global demographic changes from births
and deaths.

## Usage

``` r
nb_scale_global(m1, m2, b, d, verbose = FALSE)
```

## Arguments

- m1:

  Matrix of migrant stock totals at time *t*. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*

- m2:

  Matrix of migrant stock totals at time *t*+1. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*+1.

- b:

  Vector of the number of births between time *t* and *t*+1 in each
  region.

- d:

  Vector of the number of deaths between time *t* and *t*+1 in each
  region.

- verbose:

  Logical value to indicate the print the parameter estimates at each
  iteration. By default `FALSE`.

## Value

List with adjusted `m1` and `m2`.

## See also

[`ffs_demo`](ffs_demo.md)

## Author

Guy J. Abel

## Examples

``` r
# \donttest{
## cant have examples if function not in namespace - i.e. without export 
## so comment all out for own use
# r <- LETTERS[1:4]
# P1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
#              nrow = 4, ncol = 4, dimnames = list(birth = r, dest = r), byrow = TRUE)
# P2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
#              nrow = 4, ncol = 4, dimnames = list(birth = r, dest = r), byrow = TRUE)
# # display with row and col totals
# addmargins(A = P1)
# addmargins(A = P2)
# 
# # births and deaths
# b <- rep(x = 10, 4)
# d <- rep(x = 5, 4)
# # no change in stocks, but 20 more births than deaths...
# sum(P2) - sum(P1) + sum(d) - sum(b)
# # scale
# y <- nb_scale_global (m1 = P1, m2 = P2, b = b, d = d)
# y
# sum(y$m2_adj) - sum(y$m1_adj) + sum(d) - sum(b)
# 
# # check for when extra is positive and odd
# d[1] <- 32
# d
# sum(P2 - P1) - sum(b - d)
# # scale
# y <- nb_scale_global(m1 = P1, m2 = P2, b = b, d = d)
# sum(y$m2_adj) - sum(y$m1_adj) + sum(d) - sum(b)
# }
```
