# Unilateral summaries of in-, out-, turnover and net-migration totals from an origin-destination migration flow matrix or data frame.

Unilateral summaries of in-, out-, turnover and net-migration totals
from an origin-destination migration flow matrix or data frame.

Alias for sum_region() for international data

Alias for sum_region() with more general naming

Alias for sum_unilat() with more explicit naming

## Usage

``` r
sum_region(
  m,
  drop_diagonal = TRUE,
  orig = "orig",
  dest = "dest",
  flow = "flow",
  international = FALSE,
  include_net = TRUE,
  na_rm = TRUE
)

sum_country(
  m,
  drop_diagonal = TRUE,
  orig = "orig",
  dest = "dest",
  flow = "flow",
  include_net = TRUE,
  international = TRUE,
  na_rm = TRUE
)

sum_unilat(
  m,
  drop_diagonal = TRUE,
  orig = "orig",
  dest = "dest",
  flow = "flow",
  include_net = TRUE,
  international = TRUE,
  na_rm = TRUE
)

sum_unilateral(
  m,
  drop_diagonal = TRUE,
  orig = "orig",
  dest = "dest",
  flow = "flow",
  include_net = TRUE,
  international = TRUE,
  na_rm = TRUE
)
```

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- drop_diagonal:

  Logical to indicate dropping of diagonal terms, where the origin and
  destination are the same, in the calculation of totals. Default
  `TRUE`.

- orig:

  Character string of the origin column name (when `m` is a data frame
  rather than a `matrix`)

- dest:

  Character string of the destination column name (when `m` is a data
  frame rather than a `matrix`)

- flow:

  Character string of the flow column name (when `m` is a data frame
  rather than a `matrix`)

- international:

  Logical to indicate if flows are international.

- include_net:

  Logical to indicate inclusion of a net migration total column for each
  region, in addition to the total in- and out-flows. Default `TRUE`.

- na_rm:

  Logical to indicate if to remove NA values in `m` when calculating in
  and out migration flow totals. Default set to `TRUE`.

## Value

A `tibble` with total in-, out- and turnover of flows for each region.

## Examples

``` r
# matrix
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
sum_region(m)
#> # A tibble: 4 × 5
#>   region out_mig in_mig  turn   net
#>   <chr>    <dbl>  <dbl> <dbl> <dbl>
#> 1 A          200    130   330   -70
#> 2 B          100    160   260    60
#> 3 C          135     95   230   -40
#> 4 D           65    115   180    50

if (FALSE) { # \dontrun{
# data frame (tidy) format
library(tidyverse)

# download Abel and Cohen (2019) estimates
f <- read_csv("https://ndownloader.figshare.com/files/38016762", show_col_types = FALSE)
f

# single period
f %>%
  filter(year0 == 1990) %>%
  sum_country(flow = "da_pb_closed")

# all periods using group_by
f %>%
  group_by(year0) %>%
  sum_country(flow = "da_pb_closed")
} # }
```
