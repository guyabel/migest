# Summary indices of migration distance

Summary indices of migration distance

## Usage

``` r
index_distance(
  m = NULL,
  d = NULL,
  orig = "orig",
  dest = "dest",
  flow = "flow",
  dist = "dist",
  long = TRUE
)
```

## Source

Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H.,
Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of
internal migration: issues and measures. Journal of the Royal
Statistical Society: Series A (Statistics in Society), 165(3), 435–464.
https://doi.org/10.1111/1467-985X.00247

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- d:

  A `matrix` or data frame of origin-destination distances. For `matrix`
  the first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `dist`. Region names should match those
  in `m`.

- orig:

  Character string of the origin column name (when `m` is a data frame
  rather than a `matrix`)

- dest:

  Character string of the destination column name (when `m` is a data
  frame rather than a `matrix`)

- flow:

  Character string of the flow column name (when `m` is a data frame
  rather than a `matrix`)

- dist:

  Character string of the distance column name (when `dist` is a data
  frame rather than a `matrix`)

- long:

  Logical to return a long data frame with index values all in one
  column

## Value

A tibble with 3 summary measures where

- mean:

  Mean migration distance from Bell et. al. (2002) - not discussed in
  text but given in Table 6

- median:

  Mean migration distance from Bell et. al. (2002)

- decay:

  Distance decay parameter obtained from a Poisson regression model
  (`flow ~ orig + dest + log(dist)`)

## Examples

``` r
# single year
index_distance(
  m = subset(korea_gravity, year == 2020),
  d = subset(korea_gravity, year == 2020),
  dist = "dist_cent"
)
#> # A tibble: 3 × 2
#>   measure   value
#>   <chr>     <dbl>
#> 1 mean    113.   
#> 2 median   74.8  
#> 3 decay    -0.920

# multiple years
library(dplyr)
library(tidyr)
library(purrr)

korea_gravity %>%
  select(year, orig, dest, flow, dist_cent) %>%
  group_nest(year) %>%
  mutate(i = map2(
    .x = data, .y = data,
    .f = ~index_distance(m = .x, d = .y, dist = "dist_cent", long = FALSE)
  )) %>%
  select(-data) %>%
  unnest(i)
#> # A tibble: 9 × 4
#>    year  mean median  decay
#>   <int> <dbl>  <dbl>  <dbl>
#> 1  2012  115.   90.8 -0.844
#> 2  2013  115.   90.3 -0.856
#> 3  2014  116.   90.3 -0.877
#> 4  2015  116.   90.3 -0.893
#> 5  2016  115.   78.7 -0.886
#> 6  2017  115.   79.0 -0.904
#> 7  2018  114.   77.9 -0.906
#> 8  2019  115.   79.3 -0.903
#> 9  2020  113.   74.8 -0.920
```
