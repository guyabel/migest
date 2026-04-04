# Sum bilateral data to include aggregate bilateral totals for origin and destination meta areas

Expand matrix of data frame of migration data to include aggregate sums
for corresponding origin and destination meta regions.

## Usage

``` r
sum_expand(
  m,
  return_matrix = FALSE,
  guess_order = TRUE,
  area_first = TRUE,
  orig = "orig",
  dest = "dest",
  flow = "flow",
  orig_area = "orig_area",
  dest_area = "dest_area"
)
```

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- return_matrix:

  Logical to return a matrix. Default `FALSE`.

- guess_order:

  Logical to return a matrix or data frame ordered by origin and
  destination with area names at the end of each block. Default `TRUE`.
  If `FALSE` returns matrix or data frame based on alphabetical order of
  origin and destinations.

- area_first:

  Order area sums to be placed before the origin and destination values.
  Default `TRUE`

- orig:

  Character string of the origin column name (when `m` is a data frame
  rather than a `matrix`)

- dest:

  Character string of the destination column name (when `m` is a data
  frame rather than a `matrix`)

- flow:

  Character string of the flow column name (when `m` is a data frame
  rather than a `matrix`)

- orig_area:

  Vector of labels for the origin areas of each row of `m`.

- dest_area:

  Vector of labels for the destination areas of each row of `m`.

## Value

A `tibble` or `matrix` with additional row and columns (for matrices)
for aggregate sums for origin and destination meta-regions

## Examples

``` r
##
## from matrix
##
m <- block_matrix(x = 1:16, b = c(2,3,4,2))
m
#>    A1 A2 B1 B2 B3 C1 C2 C3 C4 D1 D2
#> A1  1  1  5  5  5  9  9  9  9 13 13
#> A2  1  1  5  5  5  9  9  9  9 13 13
#> B1  2  2  6  6  6 10 10 10 10 14 14
#> B2  2  2  6  6  6 10 10 10 10 14 14
#> B3  2  2  6  6  6 10 10 10 10 14 14
#> C1  3  3  7  7  7 11 11 11 11 15 15
#> C2  3  3  7  7  7 11 11 11 11 15 15
#> C3  3  3  7  7  7 11 11 11 11 15 15
#> C4  3  3  7  7  7 11 11 11 11 15 15
#> D1  4  4  8  8  8 12 12 12 12 16 16
#> D2  4  4  8  8  8 12 12 12 12 16 16

# requires a vector of origin and destination areas
a <- rep(LETTERS[1:4], times = c(2,3,4,2))
a
#>  [1] "A" "A" "B" "B" "B" "C" "C" "C" "C" "D" "D"
sum_expand(m = m, orig_area = a, dest_area = a)
#> # A tibble: 225 × 3
#>    orig  dest   flow
#>    <chr> <chr> <int>
#>  1 A     A         4
#>  2 A     A1        2
#>  3 A     A2        2
#>  4 A     B        30
#>  5 A     B1       10
#>  6 A     B2       10
#>  7 A     B3       10
#>  8 A     C        72
#>  9 A     C1       18
#> 10 A     C2       18
#> # ℹ 215 more rows

# place area sums after regions
sum_expand(m = m, orig_area = a, dest_area = a, area_first = FALSE)
#> # A tibble: 225 × 3
#>    orig  dest   flow
#>    <chr> <chr> <int>
#>  1 A1    A1        1
#>  2 A1    A2        1
#>  3 A1    A         2
#>  4 A1    B1        5
#>  5 A1    B2        5
#>  6 A1    B3        5
#>  7 A1    B        15
#>  8 A1    C1        9
#>  9 A1    C2        9
#> 10 A1    C3        9
#> # ℹ 215 more rows

##
## from large data frame
##
if (FALSE) { # \dontrun{
library(tidyverse)
library(countrycode)

# download Abel and Cohen (2019) estimates
f <- read_csv("https://ndownloader.figshare.com/files/38016762", show_types = FALSE)
f

# 1990-1995 flow estimates
f %>%
  filter(year0 == 1990) %>%
  mutate(
    orig_area = countrycode(sourcevar = orig, custom_dict = dict_ims,
                            origin = "iso3c", destination = "region"),
    dest_area = countrycode(sourcevar = dest, custom_dict = dict_ims,
                            origin = "iso3c", destination = "region")
  ) %>%
  sum_expand(flow = "da_pb_closed", return_matrix = FALSE)

# by group (period)
f %>%
  mutate(
    orig_area = countrycode(sourcevar = orig, custom_dict = dict_ims,
                            origin = "iso3c", destination = "region"),
    dest_area = countrycode(sourcevar = dest, custom_dict = dict_ims,
                            origin = "iso3c", destination = "region")
  ) %>%
  group_by(year0) %>%
  sum_expand(flow = "da_pb_closed", return_matrix = FALSE)
} # }
```
