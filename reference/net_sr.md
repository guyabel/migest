# Estimate net migration from survival ratios applied to lifetime migration data

Using survival ratios to estimate net migration from lifetime migration
data

## Usage

``` r
net_sr(
  .data,
  pop0_col = "pop0",
  pop1_col = "pop1",
  survival_ratio_col = "sr",
  net_children = FALSE,
  maternal_exposure = c(0.25, 0.75),
  maternal_age_id = 4:9,
  maternal_col = pop1_col
)
```

## Arguments

- .data:

  A data frame with two rows with the total number of lifetime in- and
  out-migrants in separate columns. The first row contains totals at the
  first time point and second row at the second time point.

- pop0_col:

  Character string name of column containing name of initial
  populations. Default `"pop0"`.

- pop1_col:

  Character string name of column containing name of end populations.
  Default `"pop1"`.

- survival_ratio_col:

  Character string name of column containing survivor ratios. Default
  `"sr"`.

- net_children:

  Logical to indicate if to estimate net migration when no survival
  ratio exists. Default `FALSE`.

- maternal_exposure:

  Vector for maternal exposures to interval to be used to estimate net
  migration for each of the unknown children age groups. Length should
  correspond to the number of children age groups where net migration
  estimates are required.

- maternal_age_id:

  Row numbers to indicate which rows correspond to maternal age groups
  at the end of the period.

- maternal_col:

  Name of maternal population column, required for the estimation of net
  migration of children.

## Value

Data frame with estimates of net migration

## References

Bogue, D. J., Hinze, K., & White, M. (1982). Techniques of Estimating
Net Migration. Community and Family Study Center. University of Chicago.

## Examples

``` r
# results to match un manual 1984 (table 24)
net_sr(bombay_1951, pop0_col = "pop_1941", pop1_col = "pop_1951")
#> # A tibble: 13 × 10
#>    age_1941 age_1951 pop_1941 pop_1951    sr net_forward net_reverse net_average
#>    <fct>    <fct>       <dbl>    <dbl> <dbl>       <dbl>       <dbl>       <dbl>
#>  1 0-4      10-14       77135   132870 0.909      62777.      69085.      65931.
#>  2 5-9      15-19       85434   170227 0.957      88441.      92386.      90413.
#>  3 10-14    20-24       79185   263971 0.947     188975.     199530.     194252.
#>  4 15-19    25-29       82603   253964 0.931     177077.     190242.     183659.
#>  5 20-24    30-34      126247   195373 0.922      78935.      85585.      82260.
#>  6 25-29    35-39      155344   151259 0.916       8948.       9768.       9358.
#>  7 30-34    40-44      138843   118383 0.905      -7228.      -7990.      -7609.
#>  8 35-39    45-49      109356    76421 0.885     -20359.     -23005.     -21682.
#>  9 40-44    50-54       81626    65897 0.855      -3877.      -4535.      -4206.
#> 10 45-49    55-59       47062    32265 0.812      -5959.      -7337.      -6648.
#> 11 50-54    60-64       36908    22248 0.754      -5562.      -7382.      -6472.
#> 12 55-59    65-69       15134     9655 0.673       -524.       -779.       -652.
#> 13 60+      70+         25094    10100 0.387        399.       1031.        715.
#> # ℹ 2 more variables: pop1_forward <dbl>, pop0_reverse <dbl>
  
# results to match Bogue, Hinze and White (1982)
library(dplyr)
alabama_1970 %>%
  filter(race == "white", sex == "male") %>%
  select(-race, -sex) %>%
  group_by(age_1970) %>%
  net_sr(pop0_col = "pop_1960", pop1_col = "pop_1970", 
         survival_ratio_col = "us_census_sr")
#> # A tibble: 17 × 9
#>    age_1970 pop_1960 pop_1970 us_census_sr net_forward net_reverse net_average
#>    <fct>       <dbl>    <dbl>        <dbl>       <dbl>       <dbl>       <dbl>
#>  1 0-4        110365   105503        0.955        57.0        59.6        58.3
#>  2 5-9        126116   120884        0.947      1481.       1564.       1523. 
#>  3 10-14      126346   128656        1.00       2063.       2059.       2061. 
#>  4 15-19      119571   118485        1.01      -1704.      -1696.      -1700. 
#>  5 20-24      117525   102296        0.989    -13929.     -14085.     -14007. 
#>  6 25-29       94148    85778        0.973     -5846.      -6007.      -5926. 
#>  7 30-34       72012    74046        0.985      3138.       3187.       3162. 
#>  8 35-39       70867    71636        0.980      2176.       2220.       2198. 
#>  9 40-44       75168    75179        0.972      2127.       2189.       2158. 
#> 10 45-49       76872    73602        0.948       708.        746.        727. 
#> 11 50-54       72068    67679        0.933       466.        499.        482. 
#> 12 55-59       69002    60807        0.885      -273.       -308.       -291. 
#> 13 60-64       60654    52073        0.845       791.        936.        863. 
#> 14 65-69       49127    38786        0.750      1917.       2555.       2236. 
#> 15 70-74       37344    26632        0.675      1436.       2129.       1783. 
#> 16 75-79       32063    17418        0.537       214.        399.        306. 
#> 17 80+         51294    16029        0.296       860.       2908.       1884. 
#> # ℹ 2 more variables: pop1_forward <dbl>, pop0_reverse <dbl>
         
# results to match UN manual 1992 (table 6)
net_sr(manila_1970, pop0_col = "pop_1960", pop1_col = "pop_1970", 
       survival_ratio_col = "phl_census_sr")
#> # A tibble: 16 × 9
#>    age_1970 pop_1960 pop_1970 phl_census_sr net_forward net_reverse net_average
#>    <fct>       <dbl>    <dbl>         <dbl>       <dbl>       <dbl>       <dbl>
#>  1 0-4            NA    85870        NA              0           0           0 
#>  2 5-9            NA    83054        NA              0           0           0 
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
       
# with children net migration estimate
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
