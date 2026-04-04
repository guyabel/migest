# Multiplicative component descriptions of origin-destination flow tables based on total reference coding system.

Multiplicative component descriptions of origin-destination flow tables
based on total reference coding system.

## Usage

``` r
multi_comp2(m)
```

## Arguments

- m:

  `matrix` of migration flows

## Value

`matrix` of multiplicative components of `m`. When output is an array
the total for each table of origin-destination flows is used.

## References

Rogers, A., Willekens, F., Little, J., & Raymer, J. (2002). Describing
migration spatial structure. Papers in Regional Science, 81(1), 29–48.
https://doi.org/10.1007/s101100100090

Raymer, J., Bonaguidi, A., & Valentini, A. (2006). Describing and
projecting the age and spatial structures of interregional migration in
Italy. Population, Space and Place, 12(5), 371–388.
https://doi.org/10.1002/psp.414

## Examples

``` r
r <- LETTERS[1:2]
m0 <- array(c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2),
            dimnames = list(orig = r, dest = r, type = c("ILL", "HEALTHY")))
addmargins(m0)
#> , , type = ILL
#> 
#>      dest
#> orig  A B Sum
#>   A   5 2   7
#>   B   1 7   8
#>   Sum 6 9  15
#> 
#> , , type = HEALTHY
#> 
#>      dest
#> orig  A  B Sum
#>   A   4  5   9
#>   B   2  9  11
#>   Sum 6 14  20
#> 
#> , , type = Sum
#> 
#>      dest
#> orig   A  B Sum
#>   A    9  7  16
#>   B    3 16  19
#>   Sum 12 23  35
#> 
multi_comp2(m = m0)
#> , , type = ILL
#> 
#>      dest
#> orig           A          B        Sum
#>   A    1.7857143  0.4761905  0.4666667
#>   B    0.3125000  1.4583333  0.5333333
#>   Sum  0.4000000  0.6000000 15.0000000
#> 
#> , , type = HEALTHY
#> 
#>      dest
#> orig           A          B        Sum
#>   A    1.4814815  0.7936508  0.4500000
#>   B    0.6060606  1.1688312  0.5500000
#>   Sum  0.3000000  0.7000000 20.0000000
#> 
```
