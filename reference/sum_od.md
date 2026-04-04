# Extract a classic origin-destination migration flow matrix.

Extract a classic origin-destination migration flow matrix from a more
detailed dis-aggregation of flows stored in an (`array`). Primarily
intended to work with output from [`ffs_demo`](ffs_demo.md).

## Usage

``` r
sum_od(x = NULL, zero_diag = TRUE, add_margins = TRUE)
```

## Arguments

- x:

  Array of origin-destination matrices, where the first and second
  dimensions correspond to origin and destination respectively. Higher
  dimension(s) refer to additional migrant characteristic(s).

- zero_diag:

  Logical to indicate if to set diagonal terms to zero. Default `TRUE`.

- add_margins:

  Logical to indicate if to add row and column for immigration and
  emigration totals. Default `TRUE`

## Value

Matrix from summing over the first and second dimension. Set diagonals
to zero.

Returns a `matrix` object of origin-destination flows

## See also

[`ffs_demo`](ffs_demo.md)
