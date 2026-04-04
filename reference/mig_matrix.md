# Helper function to format migration input

Helper function to format migration input

## Usage

``` r
mig_matrix(m, array = TRUE, orig = "orig", dest = "dest", flow = "flow")
```

## Arguments

- m:

  A `matrix` or data frame of origin-destination flows. For `matrix` the
  first and second dimensions correspond to origin and destination
  respectively. For a data frame ensure the correct column names are
  passed to `orig`, `dest` and `flow`.

- array:

  Logical on return of array of all dimensions or origin-destination
  matrix (summed over all other dimensions)

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

Formatted matrix
