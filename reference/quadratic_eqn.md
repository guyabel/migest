# Solutions from the quadratic equation

General function to solve classic quadratic equation: \$\$ a x^2 + b x +
c = 0 \$\$

## Usage

``` r
quadratic_eqn(a, b, c)
```

## Source

Adapted from https://rpubs.com/kikihatzistavrou/80124

## Arguments

- a:

  Numeric value for quadratic term of x.

- b:

  Numeric value for multiplicative term of x.

- c:

  Numeric value for constant term.

## Value

Vector of two values corresponding to the roots for the quadratic
equation.

## Author

Guy J. Abel

## Examples

``` r
quadratic_eqn(a = 2, b = 4, c = -6)
#> [1]  1 -3
```
