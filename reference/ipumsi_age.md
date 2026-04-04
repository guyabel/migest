# Age specific migration and population counts from two IPUMSI samples

Age specific migration and population counts for Brazil 2000 and France
2006 IPUMS International samples. Attempt to recreate the unsmoothed
data used in the appendix of Bernard, Bell and Charles-Edwards (2014)

## Usage

``` r
ipumsi_age
```

## Format

Data frame with 202 rows and 4 columns:

- sample:

  IPUMS International sample - either BRA2000 or FRA2006

- age:

  Age on census data

- migrants:

  Number of migrants, defined by those who had changed usual place of
  residence to a different minor administrative region compared to usual
  place of residence five years prior to the census. Obtained by summing
  person weights for `migrate5` variable equal to any of code 12, 20 or
  30.

- population:

  Population of each age group, obtained by summing person weights
  `perwt` variable.

## Source

Minnesota Population Center. (2015). Integrated Public Use Microdata
Series, International: Version 6.4 Machine-readable database
<https://international.ipums.org/international/>

Bernard, A., Bell, M., & Charles-Edwards, E. (2014). Improved measures
for the cross-national comparison of age profiles of internal migration.
Population Studies, 68(2), 179–195.
