# Sum over a selected block in a block matrix

Returns of a sum of a block within a `matrix`. This function is
predominantly intended to be used within the
[`ipf2_block`](ipf2_block.md) routine.

## Usage

``` r
block_sum(block = NULL, m = NULL, block_id = NULL)
```

## Arguments

- block:

  Numeric value of block to summed. To be matched against the matrix in
  `block_id`.

- m:

  Matrix of all blocks combined.

- block_id:

  Matrix of the same dimensions of `m` used to identify blocks.

## Value

Returns a numeric value of the sum of a single block.

## See also

[`block_matrix`](block_matrix.md), [`stripe_matrix`](stripe_matrix.md),
[`ipf2_block`](ipf2_block.md)

## Author

Guy J. Abel

## Examples

``` r
m <- matrix(data = 100:220, nrow = 11, ncol = 11)
b <- block_matrix(x = 1:16, b = c(2, 3, 4, 2))
block_sum(block = 1, m = m, block_id = b)
#> [1] 424
block_sum(block = 4, m = m, block_id = b)
#> [1] 460
block_sum(block = 16, m = m, block_id = b)
#> [1] 856
```
