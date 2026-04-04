# Dictionary to look up region geographies based on countries used in UN DESA International Migrant Stock.

Intended for use as a custom dictionary with the countrycode package,
where the existing UN region and area codes do not match those used by
UN DESA in the WPP, see
<https://github.com/vincentarelbundock/countrycode/issues/253>

## Usage

``` r
dict_ims
```

## Format

Data frame with 243 rows and 18 columns. One of first three columns
intended as input for `origin` in `countrycode`.

- name:

  Country name

- iso3c:

  ISO numeric code

- iso3n:

  ISO 3 letter code

Remaining columns intended as input for `destination` in `countrycode`.

- name_short:

  Short country name

- ims:

  Country in UN DESA International Migration Stock data. Some codes
  added for older political geographies to match World Bank data and
  older country units in IMS

- region:

  Geographic region of country (6)

- region_sub:

  Geographic sub region of country (22). Filled using `region` if none
  given in original data

- region_sdg:

  SDG region of country (8)

- region_sdg_sub:

  Sub SDG region of country (9). Filled using `region_sdg` if none given
  in original data

- region_wb:

  World Bank region

- un_develop:

  UN development group of country (3)

- wb_income:

  World Bank income group of country (3)

- wb_income_detail:

  Detailled World Bank income group of country (4)

- lldc:

  Indicator variable for Land-Locked Developing Countries (32)

- sids:

  Indicator variable for Small Island Developing States (58)

- region_as2014:

  Region grouping used for global chord diagram plots by Abel and Sander
  (2014)

- region_sab2014:

  Region grouping used for global chord diagram plots by Sander, Abel
  and Bauer (2014)

- region_a2018:

  Region grouping used for global chord diagram plots by Abel (2018)

- region_ac2022:

  Region grouping used for global chord diagram plots by Abel and Cohen
  (2022)

## Source

The aggregates_correspondence_table_2020_1.xlsx file of United Nations
Department of Economic and Social Affairs, Population Division (2020).
International Migrant Stock 2020.

## Examples

``` r
dict_ims
#> # A tibble: 243 × 18
#>    name      iso3c iso3n name_short ims   region region_sub region_sdg region_wb
#>    <chr>     <chr> <dbl> <chr>      <lgl> <chr>  <chr>      <chr>      <chr>    
#>  1 Burundi   BDI     108 Burundi    TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#>  2 Comoros   COM     174 Comoros    TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#>  3 Djibouti  DJI     262 Djibouti   TRUE  Africa Eastern A… Sub-Sahar… Middle E…
#>  4 Eritrea   ERI     232 Eritrea    TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#>  5 Ethiopia  ETH     231 Ethiopia   TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#>  6 Ethiopia… ETI     231 Ethiopia … FALSE Africa Eastern A… Sub-Sahar… Sub-Saha…
#>  7 Kenya     KEN     404 Kenya      TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#>  8 Madagasc… MDG     450 Madagascar TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#>  9 Malawi    MWI     454 Malawi     TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#> 10 Mauritius MUS     480 Mauritius  TRUE  Africa Eastern A… Sub-Sahar… Sub-Saha…
#> # ℹ 233 more rows
#> # ℹ 9 more variables: un_develop <chr>, wb_income <chr>,
#> #   wb_income_detail <chr>, lldc <lgl>, sids <lgl>, region_as2014 <chr>,
#> #   region_sab2014 <chr>, region_a2018 <chr>, region_ac2022 <chr>
if (FALSE) { # \dontrun{
library(tidyverse)
library(countrycode)
# download Abel and Cohen (2019) estimates
f <- read_csv("https://ndownloader.figshare.com/files/38016762", show_col_types = FALSE)
f

# use dictionary to get region to region flows
d <- f %>%
  mutate(
    orig = countrycode(
      sourcevar = orig, custom_dict = dict_ims,
      origin = "iso3c", destination = "region"),
    dest = countrycode(
      sourcevar = dest, custom_dict = dict_ims,
      origin = "iso3c", destination = "region")
  ) %>%
  group_by(year0, orig, dest) %>%
  summarise_all(sum)
d
} # }
```
