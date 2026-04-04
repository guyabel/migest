# Quickly create IPF seed

This function is predominantly intended to be used within the ipf
routines in the migest package.

## Usage

``` r
ipf_seed(m = NULL, R = NULL, n_dim = NULL, dn = NULL)
```

## Arguments

- m:

  Matrix, Array or NULL to build seed. If NULL seed will be 1 for all
  elements.

- R:

  Number of rows, columns and possibly n_dimensions for seed matrix or
  array.

- n_dim:

  Numeric integer for the number of n_dimensions - 2 for matrix, 3 or
  more for an array

- dn:

  Vector of character strings for n_dimension names

## Value

An `array` or `matrix`

## Author

Guy J. Abel
