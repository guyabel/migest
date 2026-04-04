# Rescale net migration total to a global zero sum

Modify a set of net migration (or any numbers) so that they sum to zero.

## Usage

``` r
rescale_net(
  x,
  method = "no-switches",
  w = rep(1, length(x)),
  integer_result = TRUE
)
```

## Arguments

- x:

  Vector of net migration values

- method:

  Method used to adjust net migration values of `x` to obtain a global
  zero sum. By default `method="no-switches"`. Can also take values
  `method="switches"`. See details for explanation on each method.

- w:

  Weights used in rescaling method

- integer_result:

  Logical operator to indicate if output should be integers, default is
  `TRUE`.

## Value

Rescales net migration for a number of regions in vector `x` to sum to
zero. When `method="no-switches"` rescaling of values are done for the
positive and negative values separately, to ensure the final global sum
is zero. When `method="switches"` the mean of the unscaled net migration
is subtracted from each value.

## References

Abel, G. J. (2018). Non-zero trajectories for long-run net migration
assumptions in global population projection models. *Demographic
Research* 38, (54) 1635–1662

## Author

Guy J. Abel

## Examples

``` r
# net migration in regions countries (does not add up to zero)
x <- c(-200, -30, -5, 0, 10, 20, 60, 80)
x
#> [1] -200  -30   -5    0   10   20   60   80
sum(x)
#> [1] -65
# rescale 
y1 <- rescale_net(x)
y1
#> [1] -172  -26   -4    0   12   24   71   95
sum(y1)
#> [1] 0
# rescale without integer restriction
y2 <- rescale_net(x, integer_result = FALSE)
y2
#> [1] -172.340426  -25.851064   -4.308511    0.000000   11.911765   23.823529
#> [7]   71.470588   95.294118
sum(y2)
#> [1] 7.993606e-15
# rescale allowing switching of signs (small negative value becomes positive)
y3 <- rescale_net(x, method = "switches")
y3
#> [1] -191  -22    3    8   18   28   68   88
sum(y3)
#> [1] 0
```
