# Estimation of bilateral migrant flows from bilateral migrant stocks using rates approaches

Estimates migrant transitions flows between two sequential migrant stock
tables using approached based on rates.

## Usage

``` r
ffs_rates(stock_start = NULL, stock_end = NULL, M = NULL, method = "dennett")
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

- M:

  Numeric value for the global sum of migration flows, used for
  `dennett` approach.

- method:

  Method to estimate flows. Can take values `dennett` or
  `rogers-von-rabenau`. See details section for more information. Uses
  `dennett` as default.

## Value

Estimates migrant transitions flows based on migration rates.

When `method = "dennett"` migration are derived from the matrix supplied
to `stock_start`. Dennett uses bilateral migrant stocks at beginning of
period. Rates then multiplied by global migration flows supplied in `M`.

When `method = "rogers-von-rabenau"` a matrix of growth rates are
derived from the changes in initial populations stock `stock_start` to
obtain `stock_end`; \$\$P^{t+1} = g P^{t}\$\$ and then multiplied by the
corresponding populations at risk in `stock_start`. Can result in
negative flows.

## References

Dennett, A. (2015). Estimating an Annual Time Series of Global Migration
Flows - An Alternative Methodology for Using Migrant Stock Data. *Global
Dynamics: Approaches from Complexity Science*, 125–142.
https://doi.org/10.1002/9781118937464.ch7

Rogers, A., & Von Rabenau, B. (1971). Estimation of interregional
migration streams from place-of-birth-by-residence data. *Demography*,
8(2), 185–194.

## See also

[`ffs_demo`](ffs_demo.md), `ffs_rates`

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

# calculate total migration flows for dennett approach
n <- colSums(s2) - colSums(s1)

ffs_rates(stock_start = s1, M =  sum(abs(n)), method = "dennett" )
#>     dest
#> orig        A        B        C         D
#>    A 0.000000 2.352941 2.352941  0.000000
#>    B 4.705882 0.000000 5.882353  2.352941
#>    C 2.352941 9.411765 0.000000 15.294118
#>    D 4.705882 5.882353 4.705882  0.000000
ffs_rates(stock_start = s1, stock_end = s2, method = "rogers-von-rabenau" )
#>     dest
#> orig          A         B         C         D
#>    A   0.000000 26.584620 -10.22741  24.16653
#>    B   3.474032  0.000000  11.71799  20.12425
#>    C  34.097610 -0.125662   0.00000 -51.85130
#>    D -12.047899 38.857311 -24.36993   0.00000
```
