# Annual origin destination migration flows between Korean regions alongside selected geographic, economic and demographic variables.

Origin-destination migration flows between 2012 and 2020 based on first
level administrative regions.

## Usage

``` r
korea_gravity
```

## Format

Data frame with 2,601 rows and 20 columns:

- orig:

  Origin region

- dest:

  Destination region

- year:

  Year of flow

- flow:

  Migration flow. Data obtained from KOSIS

- dist_cent:

  Distance (in km) between geographic centroids, calculated from
  `geosphere::distm()`

- dist_min:

  Minimum distance (in km) between regions, calculated from
  `sf::st_distance()`

- dist_pw:

  Distance (in km) between population weighted centroids, calculated
  from `geosphere::distm()` using WorldPop estimates of 2020 regional
  population centroids

- contig:

  Indicate if regions share a border

- orig_pop:

  Population (in millions) of origin region. Data obtained from KOSIS.

- dest_pop:

  Population (in millions) of destination region. Data obtained from
  KOSIS.

- orig_area:

  Geographic area (in km^2) of origin region, calculated from
  `sf::st_area()`

- dest_area:

  Geographic area (in km^2) of destination region, calculated from
  `sf::st_area()`

- orig_gdp_pc:

  GDP per capita of origin region. Data obtained from KOSIS.

- orig_ginc_pc:

  Gross regional income per capita of origin region. Data obtained from
  KOSIS.

- orig_iinc_pc:

  Individual income per capita of origin region. Data obtained from
  KOSIS.

- orig_pconsum_pc:

  Personal consumption per capita of origin region. Data obtained from
  KOSIS.

- dest_gdp_pc:

  GDP per capita of destination region. Data obtained from KOSIS.

- dest_ginc_pc:

  Gross regional income per capita of destination region. Data obtained
  from KOSIS.

- dest_iinc_pc:

  Individual income per capita of destination region. Data obtained from
  KOSIS.

- dest_pconsum_pc:

  Personal consumption per capita of destination region. Data obtained
  from KOSIS.

## Source

Statistics Korea, Internal Migration Statistics. Data downloaded from
<https://kosis.kr/eng> in July 2021.

Robin Edwards, Maksym Bondarenko, Andrew J. Tatem and Alessandro
Sorichetta. Unconstrained subnational Population Weighted Density in
2000, 2005, 2010, 2015 and 2020 ( 100m resolution ). WorldPop,
University of Southampton, UK.

Source: Statistics Korea, Population Statistics Based on Resident
Registration. Data downloaded from <https://kosis.kr/eng> in July 2021.

Source: Statistics Korea, Regional GDP, Gross regional income and
Individual income. Data downloaded from <https://kosis.kr/eng> in
November 2023.

## Examples

``` r
korea_gravity
#> # A tibble: 2,601 × 20
#>    orig  dest    year   flow dist_cent dist_min dist_pw contig orig_pop dest_pop
#>    <chr> <chr>  <int>  <int>     <dbl>    <dbl>   <dbl> <lgl>     <dbl>    <dbl>
#>  1 Seoul Seoul   2012 1.07e6       0        0       0   TRUE       10.2   10.2  
#>  2 Seoul Busan   2012 2.14e4     319.     294.    324.  FALSE      10.2    3.54 
#>  3 Seoul Daegu   2012 1.38e4     241.     209.    236.  FALSE      10.2    2.51 
#>  4 Seoul Inche…  2012 3.22e4      44.1      0      25.0 TRUE       10.2    2.84 
#>  5 Seoul Gwang…  2012 1.18e4     266.     244.    265.  FALSE      10.2    1.47 
#>  6 Seoul Daeje…  2012 1.46e4     139.     109.    138.  FALSE      10.2    1.52 
#>  7 Seoul Ulsan   2012 6.80e3     300.     263.    307.  FALSE      10.2    1.15 
#>  8 Seoul Sejong  2012 1.02e3     112.      79.6   117.  FALSE      10.2    0.113
#>  9 Seoul Gyeon…  2012 2.54e5      17.9      0      13.2 TRUE       10.2   12.1  
#> 10 Seoul Gangw…  2012 2.13e4     116.      34.3   113.  FALSE      10.2    1.54 
#> # ℹ 2,591 more rows
#> # ℹ 10 more variables: orig_area <units>, dest_area <units>, orig_gdp_pc <dbl>,
#> #   orig_ginc_pc <dbl>, orig_iinc_pc <dbl>, orig_pconsum_pc <dbl>,
#> #   dest_gdp_pc <dbl>, dest_ginc_pc <dbl>, dest_iinc_pc <dbl>,
#> #   dest_pconsum_pc <dbl>
```
