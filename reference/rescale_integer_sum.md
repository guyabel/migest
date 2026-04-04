# Rescale integer vector to a set sum

For when you want to rescale a set of numbers to sum to a given value
and do not want all rescaled values to be integers.

## Usage

``` r
rescale_integer_sum(x, tot)
```

## Arguments

- x:

  Vector of numeric values

- tot:

  Numeric integer value to rescale sum to.

## Value

Vector or integer values that sum to to `tot`

## See also

[`ipf3_qi`](ipf3_qi.md), [`ffs_diff`](ffs_diff.md)

## Author

Guy J. Abel

## Examples

``` r
x <- rnorm(n = 10, mean = 5, sd = 20)
y <- rescale_integer_sum(x, tot = 10)
#> Error in sample.int(length(x), size, replace, prob): invalid first argument
y
#> Error: object 'y' not found
sum(y)
#> Error: object 'y' not found

for(i in 1:10){
  y <- rescale_integer_sum(x = rpois(n = 10, lambda = 10), tot = 1000)
  print(sum(y))
}
#> [1] 1000
#> [1] 1000
#> [1] 1000
#> [1] 1000
#> [1] 1000
#> [1] 1000
#> [1] 1000
#> [1] 1000
#> [1] 1000
#> [1] 1000
```
