# Create a stripped matrix with non-uniform block sizes.

Create a stripped matrix with non-uniform block sizes.

## Usage

``` r
stripe_matrix(x = NULL, s = NULL, byrow = FALSE, dimnames = NULL)
```

## Arguments

- x:

  Vector of numbers to identify each stripe.

- s:

  Vector of values for the size of the stripes, order depending on
  `byrow`

- byrow:

  Logical value. If `FALSE` (the default) the stripes are filled by
  columns, otherwise the stripes in the matrix are filled by rows.

- dimnames:

  Character string of name attribute for the basis of the stripped
  matrix. If `NULL` a vector of the same length of `s` provides the
  basis of row and column names.

## Value

Returns a `matrix` with stripe sizes determined by the `s` argument.
Each stripe is filled with the same value taken from `x`.

## See also

[`block_matrix`](block_matrix.md), [`ipf2_stripe`](ipf2_stripe.md)

## Author

Guy J. Abel

## Examples

``` r
stripe_matrix(x = 1:44, s = c(2,3,4,2), dimnames = LETTERS[1:4], byrow = TRUE)
#>    A1 A2 B1 B2 B3 C1 C2 C3 C4 D1 D2
#> A1  1  1  2  2  2  3  3  3  3  4  4
#> A2  5  5  6  6  6  7  7  7  7  8  8
#> B1  9  9 10 10 10 11 11 11 11 12 12
#> B2 13 13 14 14 14 15 15 15 15 16 16
#> B3 17 17 18 18 18 19 19 19 19 20 20
#> C1 21 21 22 22 22 23 23 23 23 24 24
#> C2 25 25 26 26 26 27 27 27 27 28 28
#> C3 29 29 30 30 30 31 31 31 31 32 32
#> C4 33 33 34 34 34 35 35 35 35 36 36
#> D1 37 37 38 38 38 39 39 39 39 40 40
#> D2 41 41 42 42 42 43 43 43 43 44 44
```
