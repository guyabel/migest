# Summary indices of migration age profile

Summary measures of migration age profiles as proposed by Rogers (1975),
Bell et. al. (2002), Bell and Muhidin (2009) and Bernard, Bell and
Charles-Edwards (2014)

## Usage

``` r
index_age(
  d = NULL,
  age,
  mi,
  age_min = 5,
  age_max = 65,
  breadth = 5,
  age_col = "age",
  mi_col = "mi",
  long = TRUE
)
```

## Source

Rogers, A. (1975). Introduction to Multiregional Mathematical
Demography. Wiley.

Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H.,
Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of
internal migration: issues and measures. Journal of the Royal
Statistical Society: Series A (Statistics in Society), 165(3), 435–464.
https://doi.org/10.1111/1467-985X.00247

Bell, M., & Muhidin, S. (2009). Cross-National Comparisons of Internal
Migration (Research Paper 2009/30; Human Development Reports).

Bernard, A., Bell, M., & Charles-Edwards, E. (2014). Improved measures
for the cross-national comparison of age profiles of internal migration.
Population Studies, 68(2), 179–195.
https://doi.org/10.1080/00324728.2014.890243

## Arguments

- d:

  Data frame of age specific migration intensities. If used, ensure the
  correct column names are passed to `age_col` and `mi_col`.

- age:

  Numeric vector of ages. Used if `d = NULL`.

- mi:

  Numeric vector of migration intensities corresponding to each value of
  `age`. Used if `d = NULL`.

- age_min:

  Numeric value for minimum age for peak calculations. Taken as 5 by
  default.

- age_max:

  Numeric value for maximum age for peak calculations. Taken as 65 by
  default.

- breadth:

  Numeric value for number of age groups around peak to be used in
  breadth_peak measure. Default of `5`.

- age_col:

  Character string of the age column name (when `d` is provided)

- mi_col:

  Character string of the migration intensities column name (when `d` is
  provided)

- long:

  Logical to return a long data frame with index values all in one
  column

## Value

A tibble with 8 summary measures where

- gmr:

  Gross migraproduction rate of Rogers (1975)

- peak_mi:

  Peak migration intensities, from Bell et. al. (2002)

- peak_age:

  Corresponding age of `peak_mi`, from Bell et. al. (2002)

- peak_breadth:

  Breadth of peak, from Bell and Muhidin (2009)

- peak_share:

  Percentage share of peak breadth of all migration, from Bell and
  Muhidin (2009)

- murc:

  Maximum upward rate of change of Bernard, Bell and Charles-Edwards
  (2014)

- mdrc:

  Maximum downward rate of change of Bernard, Bell and Charles-Edwards
  (2014)

- asymmetry:

  Asymmetry between the `murc` and `mudc`, from Bernard, Bell and
  Charles-Edwards (2014)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
ipumsi_age %>%
  filter(sample == "BRA2000") %>%
  mutate(mi = migrants/population) %>%
  index_age()
#> # A tibble: 8 × 2
#>   measure        value
#>   <chr>          <dbl>
#> 1 gmr            7.82 
#> 2 peak_mi       14.3  
#> 3 peak_age      24    
#> 4 peak_breadth 147.   
#> 5 peak_share    18.8  
#> 6 murc          19    
#> 7 mdrc          32    
#> 8 asymmetry      0.594
  
ipumsi_age %>%
  group_by(sample) %>%
  mutate(mi = migrants/population) %>%
  index_age(long = FALSE)
#> # A tibble: 2 × 9
#>   sample    gmr peak_mi peak_age peak_breadth peak_share  murc  mdrc asymmetry
#>   <chr>   <dbl>   <dbl>    <dbl>        <dbl>      <dbl> <dbl> <dbl>     <dbl>
#> 1 BRA2000  7.82    14.3       24         147.       18.8    19    32     0.594
#> 2 FRA2006  9.55    29.5       26         295.       30.8    18    30     0.6  
```
