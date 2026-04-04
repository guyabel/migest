# Estimation of bilateral migrant flows from bilateral migrant stocks using stock differencing approaches

Estimates migrant transitions flows between two sequential migrant stock
tables using differencing approaches commonly used by economists.

## Usage

``` r
ffs_diff(
  stock_start,
  stock_end,
  decrease = "return",
  include_native_born = FALSE
)
```

## Arguments

- stock_start:

  Matrix of migrant stock totals at time *t*. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*

- stock_end:

  Matrix of migrant stock totals at time *t*+1. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*+1.

- decrease:

  How to treat decreases in bilateral stocks over the *t* to *t*+1
  period (so as to avoid a negative bilateral flow estimates). See
  details for possible options. Default is `return`

- include_native_born:

  Logical value to indicate whether to include diagonal elements of
  `stock_start` and `stock_end`. Default of `FALSE` - not include.

## Value

Estimates migrant transitions flows between two sequential migrant stock
tables.

When `decrease = "zero"` all decreases in migrant stocks over there
period are set to zero, following the approach of Bertoli and
Fernandez-Huertas Moraga (2015)

When `decrease = "return"` all decreases in migrant stocks are assumed
to correspond to return flows back to their place of birth, following
the approach of Beine and Parsons (2015)

## References

Beine, Michel, Simone Bertoli, and Jesús Fernández-Huertas Moraga.
(2016). A Practitioners’ Guide to Gravity Models of International
Migration. *The World Economy* 39(4):496–512.

## See also

[`ffs_demo`](ffs_demo.md), [`ffs_rates`](ffs_rates.md)

## Author

Guy J. Abel

## Examples

``` r
s1 <- matrix(data = c(100, 10, 10, 0, 20, 55, 25, 10, 10, 40, 140, 65, 20, 25, 20, 200),
             nrow = 4, ncol = 4, byrow = TRUE)
s2 <- matrix(data = c(75, 25, 5, 15, 20, 45, 30, 15, 30, 40, 150, 35, 10, 50, 5, 200),
             nrow = 4, ncol = 4, byrow = TRUE)
r <- LETTERS[1:4]
dimnames(s1) <- dimnames(s2) <- list(pob = r, por = r)
s1; s2
#>    por
#> pob   A  B   C   D
#>   A 100 10  10   0
#>   B  20 55  25  10
#>   C  10 40 140  65
#>   D  20 25  20 200
#>    por
#> pob  A  B   C   D
#>   A 75 25   5  15
#>   B 20 45  30  15
#>   C 30 40 150  35
#>   D 10 50   5 200

ffs_diff(stock_start = s1, stock_end = s2, decrease = "zero")
#>     dest
#> orig  A  B C  D
#>    A  0 15 0 15
#>    B  0  0 5  5
#>    C 20  0 0  0
#>    D  0 25 0  0
ffs_diff(stock_start = s1, stock_end = s2, decrease = "return")
#>     dest
#> orig  A  B  C  D
#>    A  0 15  0 25
#>    B  0  0  5  5
#>    C 25  0  0 15
#>    D  0 25 30  0
```
