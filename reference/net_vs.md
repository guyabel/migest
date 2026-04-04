# Estimate net migration from vital statistics

Estimate net migration from vital statistics

## Usage

``` r
net_vs(
  .data,
  pop0_col = NULL,
  pop1_col = NULL,
  births_col = "births",
  deaths_col = "deaths"
)
```

## Arguments

- .data:

  A data frame with two rows with the total number of lifetime in- and
  out-migrants in separate columns. The first row contains totals at the
  first time point and second row at the second time point.

- pop0_col:

  Character string name of column containing name of initial
  populations. Default `"pop0"`.

- pop1_col:

  Character string name of column containing name of end populations.
  Default `"pop1"`.

- births_col:

  Character string name of column containing name of births over the
  period. Default `"births"`.

- deaths_col:

  Character string name of column containing name of deaths over the
  period. Default `"deaths"`.

## Value

A tibble with additional columns for the population change
(`pop_change`), the natural population increase (`natural_inc`) and the
net migration (`net`) over the period.

## References

Bogue, D. J., Hinze, K., & White, M. (1982). Techniques of Estimating
Net Migration. Community and Family Study Center. University of Chicago.

## Examples

``` r
library(dplyr)
d <- alabama_1970 %>%
  group_by(race, sex) %>%
  summarise(births = sum(pop_1960[1:2]),
            pop_1960 = sum(pop_1960) - births,
            pop_1970 = sum(pop_1970)) %>%
  ungroup()
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by race and sex.
#> ℹ Output is grouped by race.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(race, sex))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
d
#> # A tibble: 4 × 5
#>   race      sex    births pop_1960 pop_1970
#>   <chr>     <chr>   <dbl>    <dbl>    <dbl>
#> 1 non-white female 126886   515483   483882
#> 2 non-white male   131767   467648   426452
#> 3 white     female 224034  1159548  1298342
#> 4 white     male   236481  1124061  1235489

d %>%
  mutate(deaths = c(51449, 58845, 86880, 123220)) %>%
  net_vs(pop0_col = "pop_1960", pop1_col = "pop_1970")
#> # A tibble: 4 × 9
#>   race      sex   births pop_1960 pop_1970 deaths pop_change natural_inc     net
#>   <chr>     <chr>  <dbl>    <dbl>    <dbl>  <dbl>      <dbl>       <dbl>   <dbl>
#> 1 non-white fema… 126886   515483   483882  51449     -31601       75437 -107038
#> 2 non-white male  131767   467648   426452  58845     -41196       72922 -114118
#> 3 white     fema… 224034  1159548  1298342  86880     138794      137154    1640
#> 4 white     male  236481  1124061  1235489 123220     111428      113261   -1833
```
