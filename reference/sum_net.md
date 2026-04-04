# Calculate net migration from an origin-destination migration flow matrix.

Sums each regions flows to obtain net migration sums.

## Usage

``` r
sum_net(m, region = 1:dim(m)[1])
```

## Arguments

- m:

  Matrix of origin-destination flows, where the first and second
  dimensions correspond to origin and destination respectively.

- region:

  Integer value corresponding to the region that the net migration sum
  is desired. Will return sums for all regions by default.

## Value

Returns a numeric value of the sum of a single block.

## Author

Guy J. Abel

## Examples

``` r
r <- LETTERS[1:4]
m <- matrix(data = 1:16, nrow = 4, ncol = 4,
            dimnames = list(orig = r, dest = r))
m
#>     dest
#> orig A B  C  D
#>    A 1 5  9 13
#>    B 2 6 10 14
#>    C 3 7 11 15
#>    D 4 8 12 16
sum_net(m)
#>   A   B   C   D 
#> -18  -6   6  18 
```
