# Create a block matrix with non-uniform block sizes.

Creates a `matrix` with differing size blocks

## Usage

``` r
block_matrix(x = NULL, b = NULL, byrow = FALSE, dimnames = NULL)
```

## Arguments

- x:

  Vector of numbers to identify each block.

- b:

  Numeric value for the size of the blocks within the matrix ordered
  depending on `byrow`

- byrow:

  Logical value. If `FALSE` (the default) the blocks are filled by
  columns, otherwise the blocks in the matrix are filled by rows.

- dimnames:

  Character string of name attribute for the basis of the block matrix.
  If `NULL` a vector of the same length of `b` provides the basis of row
  and column names.#'

## Value

Returns a `matrix` with block sizes determined by the `b` argument. Each
block is filled with the same value taken from `x`.

## See also

[`stripe_matrix`](stripe_matrix.md)

## Author

Guy J. Abel

## Examples

``` r
block_matrix(x = 1:16, b = c(2,3,4,2))
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

block_matrix(x = 1:25, b = c(2,3,4,2,1))
#>    A1 A2 B1 B2 B3 C1 C2 C3 C4 D1 D2 E1
#> A1  1  1  6  6  6 11 11 11 11 16 16 21
#> A2  1  1  6  6  6 11 11 11 11 16 16 21
#> B1  2  2  7  7  7 12 12 12 12 17 17 22
#> B2  2  2  7  7  7 12 12 12 12 17 17 22
#> B3  2  2  7  7  7 12 12 12 12 17 17 22
#> C1  3  3  8  8  8 13 13 13 13 18 18 23
#> C2  3  3  8  8  8 13 13 13 13 18 18 23
#> C3  3  3  8  8  8 13 13 13 13 18 18 23
#> C4  3  3  8  8  8 13 13 13 13 18 18 23
#> D1  4  4  9  9  9 14 14 14 14 19 19 24
#> D2  4  4  9  9  9 14 14 14 14 19 19 24
#> E1  5  5 10 10 10 15 15 15 15 20 20 25
```
