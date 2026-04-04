# Sum and lump together small flows into a "other" category

Lump together regions/countries if their flows are below a given
threshold.

## Usage

``` r
sum_lump(
  m,
  threshold = 1,
  lump = "flow",
  other_level = "other",
  complete = FALSE,
  fill = 0,
  return_matrix = TRUE,
  orig = "orig",
  dest = "dest",
  flow = "flow"
)
```

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- threshold:

  Numeric value used to determine small flows, origins or destinations
  that will be grouped (lumped) together.

- lump:

  Character string to indicate where to apply the threshold. Choose from
  the `flow` values, `in` migration region and/or `out` migration
  region.

- other_level:

  Character string for the origin and/or destination label for the
  lumped values below the `threshold`. Default `"other"`.

- complete:

  Logical value to return a `tibble` with complete the
  origin-destination combinations

- fill:

  Numeric value for to fill small cells below the `threshold` when
  `complete = TRUE`. Default of zero.

- return_matrix:

  Logical to return a matrix. Default `FALSE`.

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

A `tibble` with an additional `other` origins and/or destinations region
based on the grouping together of small values below the `threshold`
argument and the `lump` argument to indicate on where to apply the
threshold.

## Details

The `lump` argument can take values `flow` or `bilat` to apply the
threshold to the data values for between region migration, `in` or `imm`
to apply the threshold to the incoming region region and `out` or `emi`
to apply the threshold to outgoing region region.

## Examples

``` r
r <- LETTERS[1:4]
m <- matrix(data = c(0, 100, 30, 10, 50, 0, 50, 5, 10, 40, 0, 40, 20, 25, 20, 0),
            nrow = 4, ncol = 4, dimnames = list(orig = r, dest = r), byrow = TRUE)
m
#>     dest
#> orig  A   B  C  D
#>    A  0 100 30 10
#>    B 50   0 50  5
#>    C 10  40  0 40
#>    D 20  25 20  0

# threshold on in and out region
sum_lump(m, threshold = 100, lump = c("in", "out"))
#> Joining with `by = join_by(dest)`
#> Joining with `by = join_by(orig)`
#> # A tibble: 9 × 3
#>   orig  dest   flow
#>   <chr> <chr> <dbl>
#> 1 A     B       100
#> 2 A     C        30
#> 3 A     other    10
#> 4 B     B         0
#> 5 B     C        50
#> 6 B     other    55
#> 7 other B        65
#> 8 other C        20
#> 9 other other    70

# threshold on flows (default)
sum_lump(m, threshold = 40)
#> # A tibble: 6 × 3
#>   orig  dest   flow
#>   <chr> <chr> <dbl>
#> 1 A     B       100
#> 2 B     A        50
#> 3 B     C        50
#> 4 C     B        40
#> 5 C     D        40
#> 6 other other   120

# return a matrix (only possible when input is a matrix and
# complete = TRUE) with small values replaced by zeros
sum_lump(m, threshold = 50, complete = TRUE)
#>        dest
#> orig      A   B   C   D other
#>   A       0 100   0   0     0
#>   B      50   0  50   0     0
#>   C       0   0   0   0     0
#>   D       0   0   0   0     0
#>   other   0   0   0   0   200

# return a data frame with small values replaced with zero
sum_lump(m, threshold = 80, complete = TRUE, return_matrix = FALSE)
#> # A tibble: 25 × 3
#>    orig  dest   flow
#>    <chr> <chr> <dbl>
#>  1 A     A         0
#>  2 A     B       100
#>  3 A     C         0
#>  4 A     D         0
#>  5 A     other     0
#>  6 B     A         0
#>  7 B     B         0
#>  8 B     C         0
#>  9 B     D         0
#> 10 B     other     0
#> # ℹ 15 more rows

if (FALSE) { # \dontrun{
# data frame (tidy) format
library(tidyverse)

# download Abel and Cohen (2019) estimates
f <- read_csv("https://ndownloader.figshare.com/files/38016762", show_types = FALSE)
f

# large 1990-1995 flow estimates
f %>%
  filter(year0 == 1990) %>%
  sum_lump(flow = "da_pb_closed", threshold = 1e5)

# large flow estimates for each year
f %>%
  group_by(year0) %>%
  sum_lump(flow = "da_pb_closed", threshold = 1e5)
} # }
```
