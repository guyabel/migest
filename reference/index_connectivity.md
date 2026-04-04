# Summary indices of migration connectivity

Summary indices of migration connectivity

## Usage

``` r
index_connectivity(
  m = NULL,
  gini_orig_all = FALSE,
  gini_dest_all = FALSE,
  gini_corrected = TRUE,
  orig = "orig",
  dest = "dest",
  flow = "flow",
  long = TRUE
)
```

## Source

Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H.,
Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of
internal migration: issues and measures. Journal of the Royal
Statistical Society: Series A (Statistics in Society), 165(3), 435–464.
https://doi.org/10.1111/1467-985X.00247  

Rogers, A., & Raymer, J. (1998). The Spatial Focus of US Interstate
Migration Flows. International Journal of Population Geography, 4(1),
63–80.
https://doi.org/10.1002/(SICI)1099-1220(199803)4%3A1\<63%3A%3AAID-IJPG87\>3.0.CO%3B2-U  

Rogers, A., & Sweeney, S. (1998). Measuring the Spatial Focus of
Migration Patterns. Professional Geographer, 50(2), 232–242.  

Plane, D., & Mulligan, G. F. (1997). Measuring spatial focusing in a
migration system. Demography, 34(2), 251–262.  

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- gini_orig_all:

  Logical to include gini index values for all origin regions. Default
  `FALSE`.

- gini_dest_all:

  Logical to include gini index values for all destination regions.
  Default `FALSE`.

- gini_corrected:

  Logical to use corrected denominator in Gini index of Bell (2002) or
  original of David A. Plane and Mulligan (1997)

- orig:

  Character string of the origin column name (when `m` is a data frame
  rather than a `matrix`)

- dest:

  Character string of the destination column name (when `m` is a data
  frame rather than a `matrix`)

- flow:

  Character string of the flow column name (when `m` is a data frame
  rather than a `matrix`)

- long:

  Logical to return a long data frame with index values all in one
  column

## Value

A tibble with 12 summary measures:

- connectivity:

  Migration connectivity index of Bell et. al. (2002) for the share of
  non-zero flows. A value of 0 means no connections (all zero flows) and
  1 shows that all regions are connected by migrants.

- inequality_equal:

  Migration inequality index of Bell et. al. (2002) based on a
  distributions of flows compared to equal distributions of expected
  flows . A value of 0 shows complete equality in flows and 1 shows
  maximum inequality.

- inequality_sim:

  Migration inequality index of Bell et. al. (2002) based on a
  distributions of flows compared to distributions of expected flows
  from a Poisson regression independence fit `flow ~ orig + dest`. A
  value of 0 shows complete equality in flows and 1 shows maximum
  inequality.

- gini_total:

  Overall concentration of migration from Bell (2002), corrected from
  Plane and Mulligan (1997). A value of 0 means no spatial focusing and
  1 shows that all migrants are found in one single flow. Calculated
  using
  [`migration.indices::migration.gini.total()`](https://rdrr.io/pkg/migration.indices/man/migration.gini.total.html)

- gini_orig_standardized:

  Relative extent to which the origin selections of out-migrations are
  spatially focused. A value of 0 means no spatial focusing and 1 shows
  maximum focusing. Adapted from
  [`migration.indices::migration.gini.row.standardized()`](https://rdrr.io/pkg/migration.indices/man/migration.gini.row.standardized.html).

- gini_dest_standardized:

  Relative extent to which the destination selections of in-migrations
  are spatially focused. A value of 0 means no spatial focusing and 1
  shows maximum focusing. Adapted from
  [`migration.indices::migration.gini.col.standardized()`](https://rdrr.io/pkg/migration.indices/man/migration.gini.col.standardized.html).

- mwg_orig:

  Origin spatial focusing, from Bell et. al. (2002). Calculated using
  [`migration.indices::migration.weighted.gini.out()`](https://rdrr.io/pkg/migration.indices/man/migration.weighted.gini.out.html)

- mwg_dest:

  Destination spatial focusing, from Bell et. al. (2002). Calculated
  using
  [`migration.indices::migration.weighted.gini.in()`](https://rdrr.io/pkg/migration.indices/man/migration.weighted.gini.in.html)

- mwg_mean:

  Mean spatial focusing, from Bell et. al. (2002). Average of the origin
  and destination migration weighted Gini indices (`mwg_orig` and
  `mwg_dest`). A value of 0 means no spatial focusing and 1 shows that
  all migrants are found in one region. Calculated using
  [`migration.indices::migration.weighted.gini.mean()`](https://rdrr.io/pkg/migration.indices/man/migration.weighted.gini.mean.html)

- cv:

  Coefficient of variation from Rogers and Raymer (1998).

- acv:

  Aggregated system-wide coefficient of variation from Rogers and
  Sweeney (1998), using
  [`migration.indices::migration.acv()`](https://rdrr.io/pkg/migration.indices/man/migration.acv.html)

## Examples

``` r
library(dplyr)
korea_gravity %>%
  filter(year == 2020) %>%
  select(orig, dest, flow) %>%
  index_connectivity()
#> # A tibble: 11 × 2
#>    measure                  value
#>    <chr>                    <dbl>
#>  1 connectivity            1     
#>  2 inequality_equal        0.541 
#>  3 inequality_sim          0.281 
#>  4 gini_total              0.709 
#>  5 gini_orig_standardized  0.0493
#>  6 gini_dest_standardized  0.0517
#>  7 mwg_orig                0.0370
#>  8 mwg_dest                0.0389
#>  9 mwg_mean                0.0379
#> 10 cv                     17.9   
#> 11 acv                     3.43  
```
