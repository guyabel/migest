# Summary indices of migration impact

Summary indices of migration impact

## Usage

``` r
index_impact(
  m,
  p,
  pop = "pop",
  reg = "region",
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

Shryock, H. S., & Siegel, J. S. (1976). The Methods and Materials of
Demography. (E. G. Stockwell (ed.); Condensed). Academic Press.  

United Nations Department of Economic and Social Affairs Population
Division. (1970). Methods of measuring internal migration. United
Nations Department of Economic and Social Affairs Population Division -
1970 - Methods of measuring internal migration
<https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/files/documents/2020/Jan/manual_vi_methods_of_measuring_internal_migration.pdf>

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- p:

  A data frame or named vector for the total population. When data
  frame, column of populations labelled using `pop` and region names
  labelled `reg`.

- pop:

  Character string of the population column name

- reg:

  Character string of the region column name. Must match dimension names
  or values in origin and destination columns of `m`.

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

A tibble with 4 summary measures where

- effectivness:

  Migration effectiveness index (MEI) from Shryock et al. (1975). Values
  range between 0 and 100. High values indicate migration is an
  efficient mechanism of population redistribution, generating a large
  net migration. Conversely, low values denote that migration is closely
  balanced, leading to comparatively little redistribution.

- anmr:

  Aggregate net migration rate from Bell et. al. (2002). The population
  weighted version of `mei`.

- perference:

  Index of preference, given in UN DESA (1983). From Bachi (1957) and
  Shryock et al. (1975) - measures size of migration compared to
  expected flows based on unifrom migration. Can go from 0 to infinity

- velocity:

  Index of velocity, given in UN DESA (1983). From Bogue, Shryock, Jr. &
  Hoermann (1957) - measures size of migration compared to expected
  flows based on population size alone. Can go from 0 to infinity

## Examples

``` r
# single year
library(dplyr)
m <- korea_gravity %>%
  filter(year == 2020,
         orig != dest) %>%
  select(orig, dest, flow)
m
#> # A tibble: 272 × 3
#>    orig  dest                flow
#>    <chr> <chr>              <int>
#>  1 Seoul Busan              20990
#>  2 Seoul Daegu              15216
#>  3 Seoul Incheon            38409
#>  4 Seoul Gwangju            11232
#>  5 Seoul Daejeon            15116
#>  6 Seoul Ulsan               7062
#>  7 Seoul Sejong              5107
#>  8 Seoul Gyeonggi-do       266375
#>  9 Seoul Gangwon-do         20048
#> 10 Seoul Chungcheongbuk-do  14574
#> # ℹ 262 more rows
p <- korea_gravity %>%
  filter(year == 2020) %>%
  distinct(dest, dest_pop)
p
#> # A tibble: 17 × 2
#>    dest              dest_pop
#>    <chr>                <dbl>
#>  1 Seoul                9.67 
#>  2 Busan                3.39 
#>  3 Daegu                2.42 
#>  4 Incheon              2.94 
#>  5 Gwangju              1.45 
#>  6 Daejeon              1.46 
#>  7 Ulsan                1.14 
#>  8 Sejong               0.356
#>  9 Gyeonggi-do         13.4  
#> 10 Gangwon-do           1.54 
#> 11 Chungcheongbuk-do    1.60 
#> 12 Chungcheongnam-do    2.12 
#> 13 Jeollabuk-do         1.80 
#> 14 Jeollanam-do         1.85 
#> 15 Gyeongsangbuk-do     2.64 
#> 16 Gyeongsangnam-do     3.34 
#> 17 Jeju                 0.675
index_impact(m = m, p = p, pop = "dest_pop", reg = "dest")
#> # A tibble: 4 × 2
#>   measure            value
#>   <chr>              <dbl>
#> 1 effectivness        7.67
#> 2 anmr           375133.  
#> 3 preference        375.  
#> 4 velocity     18339531.  

# multiple years
library(tidyr)
library(purrr)

korea_gravity %>%
  select(year, orig, dest, flow, dest_pop) %>%
  group_nest(year) %>%
  mutate(m = map(.x = data, .f = ~select(.x, orig, dest, flow)),
         p = map(.x = data, .f = ~distinct(.x, dest, dest_pop)),
         i = map2(.x = m, .y = p,
                  .f = ~index_impact(
                    m = .x, p = .y, pop = "dest_pop", reg = "dest", long = FALSE
                  ))) %>%
  select(-data, -m, -p) %>%
  unnest(i)
#> # A tibble: 9 × 5
#>    year effectivness    anmr preference  velocity
#>   <int>        <dbl>   <dbl>      <dbl>     <dbl>
#> 1  2012         6.07 299565.       409. 20186642.
#> 2  2013         5.72 270956.       371. 17602058.
#> 3  2014         5.36 261867.       434. 21183582.
#> 4  2015         7.73 382561.       459. 22714406.
#> 5  2016         8.47 402031.       401. 19034527.
#> 6  2017         7.99 371841.       417. 19398445.
#> 7  2018         9.29 435327.       398. 18672576.
#> 8  2019         6.94 319291.       391. 18005614.
#> 9  2020         7.67 375133.       375. 18339531.
```
