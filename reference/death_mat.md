# Calculate deaths for each element of place of birth - place of residence stock matrix

This function is predominantly intended to be used within the `ffs`
routines in the migest package.

## Usage

``` r
death_mat(
  d_por = NULL,
  m1 = NULL,
  method = "proportion",
  m2 = NULL,
  b_por = NULL
)
```

## Arguments

- d_por:

  Vector of numeric values for deaths in each place of residence.

- m1:

  Matrix of migrant stock totals at time *t*. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*. Used to distribute deaths proportionally to each migrant stock
  population.

- method:

  Character string of either `"proportion"` or `"accounting"` to choose
  method to distribute deaths. The `"proportion"` method assumes the
  mortality rate in each place of birth sub-group (native born and all
  foreign born stocks) is the same. The `"accounting"` method ensures
  that the the deaths by place of birth matches that implied by
  demographic accounting. Still needs to be explored fully.

- m2:

  Matrix of migrant stock totals at time *t*+1. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*+1. Used to distribute deaths proportionally to each migrant stock
  population. For use when `method = "accounting"`

- b_por:

  Vector of numeric values for births in each place of residence. For
  use when `method = "accounting"`.

## Value

Matrix of place of death by place of residence
