# Summary indices of age migration profile based on parameters from a Rogers and Castro schedule

Summary indices of age migration profile based on parameters from a
Rogers and Castro schedule

## Usage

``` r
index_age_rc(pars = NULL, long = TRUE)
```

## Source

Rogers, A., & Castro, L. J. (1981). Model Migration Schedules. In IIASA
Research Report (Vol. 81, Issue RR-81-30).
http://webarchive.iiasa.ac.at/Admin/PUB/Documents/RR-81-030.pdf

## Arguments

- pars:

  Named vector or parameters parameters from a Rogers and Castro
  schedule

- long:

  Logical to return a long data frame with index values all in one
  column

## Value

A tibble with at least five summary measures

## Examples

``` r
library(dplyr)
library(tibble)
rc_model_fund %>%
  deframe() %>%
  index_age_rc()
#> # A tibble: 5 × 2
#>   measure           value
#>   <chr>             <dbl>
#> 1 peaking          20    
#> 2 child_dependency  0.333
#> 3 labor_dependency  3    
#> 4 labor_asymmetry   4    
#> 5 regularity        1    
```
