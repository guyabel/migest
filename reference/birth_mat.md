# Calculate births for each element of place of birth - place of residence stock matrix

This function is predominantly intended to be used within the ffs
routines in the migest package.

## Usage

``` r
birth_mat(b_por = NULL, m2 = NULL, method = "native", non_negative = TRUE)
```

## Arguments

- b_por:

  Vector of numeric values for births in each place of residence

- m2:

  Matrix of migrant stock totals at time *t*+1. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*+1.

- method:

  Character string of either `"native"` or `"proportion"` to choose
  method to distribute births. The `"proportion"` method assumes the
  rate of non-migration increase in each place of birth sub-group
  (native born and all foreign born stocks) is the same. The `"native"`
  method ensures that all births (non-migration increases) in stocks
  belong to the native born population (they do not move straight after
  birth).

- non_negative:

  Adjust birth matrix calculation to ensure all deductions from `m2`
  will result in positive population counts. On rare occasions when
  working with international stock data the number of births can exceed
  the increase in the number of native born population.

## Value

Matrix of place of birth by place of residence for new-born’s
