# Summary of bilateral flows, counter-flow and net migration flow

Summary of bilateral flows, counter-flow and net migration flow

## Usage

``` r
sum_bilat(m, label = "flow", orig = "orig", dest = "dest", flow = "flow")
```

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- label:

  Character string for the prefix of the calculated columns. Can take
  values `flow` or `stream`

- orig:

  Character string of the origin column name (when `m` is a data frame
  rather than a `matrix`)

- dest:

  Character string of the destination column name (when `m` is a data
  frame rather than a `matrix`)

- flow:

  Character string of the flow column name (when `m` is a data frame
  rather than a `matrix`)

## Value

A `tibble` with columns for orig, destination, corridor, flow,
counter-flow and net flow in each bilateral pair.

## Examples

``` r
# using matrix
r <- LETTERS[1:4]
m <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
            nrow = 4, ncol = 4, dimnames = list(orig = r, dest = r), byrow = TRUE)
m
#>     dest
#> orig  A   B  C  D
#>    A  0 100 30 70
#>    B 50   0 45  5
#>    C 60  35  0 40
#>    D 20  25 20  0
sum_bilat(m)
#> # A tibble: 12 × 8
#>    orig  dest  corridor pair   flow counter_flow net_flow interchange
#>    <chr> <chr> <chr>    <chr> <dbl>        <dbl>    <dbl>       <dbl>
#>  1 B     A     B -> A   A - B    50          100      -50         150
#>  2 C     A     C -> A   A - C    60           30       30          90
#>  3 D     A     D -> A   A - D    20           70      -50          90
#>  4 A     B     A -> B   A - B   100           50       50         150
#>  5 C     B     C -> B   B - C    35           45      -10          80
#>  6 D     B     D -> B   B - D    25            5       20          30
#>  7 A     C     A -> C   A - C    30           60      -30          90
#>  8 B     C     B -> C   B - C    45           35       10          80
#>  9 D     C     D -> C   C - D    20           40      -20          60
#> 10 A     D     A -> D   A - D    70           20       50          90
#> 11 B     D     B -> D   B - D     5           25      -20          30
#> 12 C     D     C -> D   C - D    40           20       20          60

# using data frame
library(dplyr)
library(tidyr)
d <- expand_grid(orig = r, dest = r, sex = c("female", "male")) %>%
  mutate(flow = sample(x = 1:100, size = 32))
d
#> # A tibble: 32 × 4
#>    orig  dest  sex     flow
#>    <chr> <chr> <chr>  <int>
#>  1 A     A     female    15
#>  2 A     A     male      32
#>  3 A     B     female    84
#>  4 A     B     male      91
#>  5 A     C     female    65
#>  6 A     C     male      60
#>  7 A     D     female    25
#>  8 A     D     male      10
#>  9 B     A     female    43
#> 10 B     A     male      95
#> # ℹ 22 more rows

# orig-dest summary of sex-specific flows
d %>%
  group_by(sex) %>%
  sum_bilat()
#> # A tibble: 24 × 9
#> # Groups:   sex [2]
#>    orig  dest  corridor pair  sex     flow counter_flow net_flow interchange
#>    <chr> <chr> <chr>    <chr> <chr>  <int>        <int>    <int>       <int>
#>  1 A     B     A -> B   A - B female    84           43       41         127
#>  2 A     B     A -> B   A - B male      91           95       -4         186
#>  3 A     C     A -> C   A - C female    65           50       15         115
#>  4 A     C     A -> C   A - C male      60           37       23          97
#>  5 A     D     A -> D   A - D female    25           14       11          39
#>  6 A     D     A -> D   A - D male      10           67      -57          77
#>  7 B     A     B -> A   A - B female    43           84      -41         127
#>  8 B     A     B -> A   A - B male      95           91        4         186
#>  9 B     C     B -> C   B - C female    90           19       71         109
#> 10 B     C     B -> C   B - C male      62           78      -16         140
#> # ℹ 14 more rows

# use group_by to distinguish orig-dest tables
d %>%
  group_by(sex) %>%
  sum_bilat()
#> # A tibble: 24 × 9
#> # Groups:   sex [2]
#>    orig  dest  corridor pair  sex     flow counter_flow net_flow interchange
#>    <chr> <chr> <chr>    <chr> <chr>  <int>        <int>    <int>       <int>
#>  1 A     B     A -> B   A - B female    84           43       41         127
#>  2 A     B     A -> B   A - B male      91           95       -4         186
#>  3 A     C     A -> C   A - C female    65           50       15         115
#>  4 A     C     A -> C   A - C male      60           37       23          97
#>  5 A     D     A -> D   A - D female    25           14       11          39
#>  6 A     D     A -> D   A - D male      10           67      -57          77
#>  7 B     A     B -> A   A - B female    43           84      -41         127
#>  8 B     A     B -> A   A - B male      95           91        4         186
#>  9 B     C     B -> C   B - C female    90           19       71         109
#> 10 B     C     B -> C   B - C male      62           78      -16         140
#> # ℹ 14 more rows
```
