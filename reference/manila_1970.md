# Manila female population 1970 by age

Population data for Manila by age in 1960 and 1970

## Usage

``` r
manila_1970
```

## Format

Data frame with 13 rows and 5 columns:

- age_1970:

  Age group in 1970

- pop_1960:

  Enumerated population in 1960

- pop_1970:

  Enumerated population in 1970

- phl_census_sr:

  Census survival ratio derived from the national data.

## Source

Scraped from Table 6 of United Nations Department of Economic and Social
Affairs Population Division. (1992). Preparing Migration Data for
Subnational Population Projections.

## Examples

``` r
# match table 6 - perhaps small error in children net migration numbers in the published table?
net_sr(manila_1970, pop0_col = "pop_1960", pop1_col = "pop_1970", 
       survival_ratio_col = "phl_census_sr", net_children = TRUE)
#> # A tibble: 16 × 9
#>    age_1970 pop_1960 pop_1970 phl_census_sr net_forward net_reverse net_average
#>    <fct>       <dbl>    <dbl>         <dbl>       <dbl>       <dbl>       <dbl>
#>  1 0-4            NA    85870        NA           -235.       -605.       -420.
#>  2 5-9            NA    83054        NA          -8935.     -10486.      -9710.
#>  3 10-14       80275    79489         1.12      -10196.      -9126.      -9661.
#>  4 15-19       70875   101410         0.992      31134.      31400.      31267.
#>  5 20-24       63250    90410         0.973      28877.      29683.      29280.
#>  6 25-29       85618    56055         0.889     -20082.     -22582.     -21332.
#>  7 30-34       75793    44648         0.841     -19117.     -22723.     -20920.
#>  8 35-39       60037    36963         0.957     -20497.     -21416.     -20957.
#>  9 40-44       34813    28873         0.951      -4244.      -4462.      -4353.
#> 10 45-49       31927    23678         0.904      -5189.      -5739.      -5464.
#> 11 50-54       24297    19063         0.930      -3521.      -3788.      -3655.
#> 12 55-59       20207    14484         0.797      -1613.      -2025.      -1819.
#> 13 60-64       13714    10205         0.877      -1822.      -2078.      -1950.
#> 14 65-69        9366     6405         0.835      -1417.      -1697.      -1557.
#> 15 70-74        7921     3746         0.712      -1890.      -2657.      -2274.
#> 16 75+         11114     4779         0.562      -1472.      -2617.      -2045.
#> # ℹ 2 more variables: pop1_forward <dbl>, pop0_reverse <dbl>
```
