# Summary indices of migration intensity

Summary indices of migration intensity

## Usage

``` r
index_intensity(mig_total = NULL, pop_total = NULL, n = NULL, long = TRUE)
```

## Source

Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H.,
Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of
internal migration: issues and measures. Journal of the Royal
Statistical Society: Series A (Statistics in Society), 165(3), 435–464.
https://doi.org/10.1111/1467-985X.00247

Courgeau, D. (1973). Migrants et migrations. Population, 28(1), 95–129.
https://doi.org/10.2307/1530972

Bernard, A., Rowe, F., Bell, M., Ueffing, P., Charles-Edwards, E., &
Zhu, Y. (2017). Comparing internal migration across the countries of
Latin America: A multidimensional approach. Plos One, 12(3), e0173895.
https://doi.org/10.1371/journal.pone.0173895

## Arguments

- mig_total:

  Numeric value for the total number of migrations.

- pop_total:

  Numeric value for the total population.

- n:

  Numeric value for the number of regions used in the definition of
  migration for `mig_total`.

- long:

  Logical to return a long data frame with index values all in one
  column

## Value

A tibble with 2 summary measures where

- cmp:

  Crude migration probability from Bell et. al. (2002), sometimes known
  as crude migration intensity, e.g. Bernard (2017)

- courgeau_k:

  Intensity measure of Courgeau (1973)

## Examples

``` r
# single year
library(dplyr)
m <- korea_gravity %>%
  filter(year == 2020,
         orig != dest)
m
#> # A tibble: 272 × 20
#>    orig  dest    year   flow dist_cent dist_min dist_pw contig orig_pop dest_pop
#>    <chr> <chr>  <int>  <int>     <dbl>    <dbl>   <dbl> <lgl>     <dbl>    <dbl>
#>  1 Seoul Busan   2020  20990     319.     294.    324.  FALSE      9.67    3.39 
#>  2 Seoul Daegu   2020  15216     241.     209.    236.  FALSE      9.67    2.42 
#>  3 Seoul Inche…  2020  38409      44.1      0      25.0 TRUE       9.67    2.94 
#>  4 Seoul Gwang…  2020  11232     266.     244.    265.  FALSE      9.67    1.45 
#>  5 Seoul Daeje…  2020  15116     139.     109.    138.  FALSE      9.67    1.46 
#>  6 Seoul Ulsan   2020   7062     300.     263.    307.  FALSE      9.67    1.14 
#>  7 Seoul Sejong  2020   5107     112.      79.6   117.  FALSE      9.67    0.356
#>  8 Seoul Gyeon…  2020 266375      17.9      0      13.2 TRUE       9.67   13.4  
#>  9 Seoul Gangw…  2020  20048     116.      34.3   113.  FALSE      9.67    1.54 
#> 10 Seoul Chung…  2020  14574     118.      57.4   108.  FALSE      9.67    1.60 
#> # ℹ 262 more rows
#> # ℹ 10 more variables: orig_area <units>, dest_area <units>, orig_gdp_pc <dbl>,
#> #   orig_ginc_pc <dbl>, orig_iinc_pc <dbl>, orig_pconsum_pc <dbl>,
#> #   dest_gdp_pc <dbl>, dest_ginc_pc <dbl>, dest_iinc_pc <dbl>,
#> #   dest_pconsum_pc <dbl>
p <- korea_gravity %>%
  filter(year == 2020) %>%
  distinct(dest, dest_pop)
p
#> # A tibble: 17 × 2
#>    dest              dest_pop
#>    <chr>                <dbl>
#>  1 Seoul                9.67 
#>  2 Busan                3.39 
#>  3 Daegu                2.42 
#>  4 Incheon              2.94 
#>  5 Gwangju              1.45 
#>  6 Daejeon              1.46 
#>  7 Ulsan                1.14 
#>  8 Sejong               0.356
#>  9 Gyeonggi-do         13.4  
#> 10 Gangwon-do           1.54 
#> 11 Chungcheongbuk-do    1.60 
#> 12 Chungcheongnam-do    2.12 
#> 13 Jeollabuk-do         1.80 
#> 14 Jeollanam-do         1.85 
#> 15 Gyeongsangbuk-do     2.64 
#> 16 Gyeongsangnam-do     3.34 
#> 17 Jeju                 0.675
index_intensity(mig_total = sum(m$flow), pop_total = sum(p$dest_pop*1e6), n = nrow(p))
#> # A tibble: 2 × 2
#>   measure    value
#>   <chr>      <dbl>
#> 1 cmp        4.89 
#> 2 courgeau_k 0.863

# multiple years
library(tidyr)
library(purrr) 
mm <- korea_gravity  %>%
 filter(orig != dest) %>%
  group_by(year) %>%
  summarise(m = sum(flow))
mm
#> # A tibble: 9 × 2
#>    year       m
#>   <int>   <int>
#> 1  2012 2512740
#> 2  2013 2423429
#> 3  2014 2507796
#> 4  2015 2551424
#> 5  2016 2453342
#> 6  2017 2410930
#> 7  2018 2429184
#> 8  2019 2384948
#> 9  2020 2534114

pp <- korea_gravity %>%
  group_by(year) %>%
  distinct(dest, dest_pop) %>%
  summarise(p = sum(dest_pop)*1e6,
            n = n_distinct(dest))
pp
#> # A tibble: 9 × 3
#>    year        p     n
#>   <int>    <dbl> <int>
#> 1  2012 50948272    17
#> 2  2013 51141463    17
#> 3  2014 51327916    17
#> 4  2015 51529338    17
#> 5  2016 51696216    17
#> 6  2017 51778544    17
#> 7  2018 51826059    17
#> 8  2019 51849861    17
#> 9  2020 51829023    17

library(purrr)
library(tidyr)
mm %>%
  left_join(pp) %>%
  mutate(i = pmap(
    .l = list(m, p, n),
    .f = ~index_intensity(mig_total = ..1, pop_total = ..2,n = ..3, long = FALSE)
  )) %>%
  unnest(cols = i)
#> Joining with `by = join_by(year)`
#> # A tibble: 9 × 6
#>    year       m        p     n   cmp courgeau_k
#>   <int>   <int>    <dbl> <int> <dbl>      <dbl>
#> 1  2012 2512740 50948272    17  4.93      0.870
#> 2  2013 2423429 51141463    17  4.74      0.836
#> 3  2014 2507796 51327916    17  4.89      0.862
#> 4  2015 2551424 51529338    17  4.95      0.874
#> 5  2016 2453342 51696216    17  4.75      0.838
#> 6  2017 2410930 51778544    17  4.66      0.822
#> 7  2018 2429184 51826059    17  4.69      0.827
#> 8  2019 2384948 51849861    17  4.60      0.812
#> 9  2020 2534114 51829023    17  4.89      0.863
```
