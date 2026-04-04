# Adjust migrant stock tables to have matching place of birth (origin) totals

This function is predominantly intended to be used within the ffs
routines in the migest package.

## Usage

``` r
match_birthplace_tot(m1, m2, method = "rescale", verbose = FALSE)
```

## Arguments

- m1:

  Matrix of migrant stock totals at time *t*. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*+1.

- m2:

  Matrix of migrant stock totals at time *t*+1. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*+1.

- method:

  Character string matching either `rescale`, `rescale-adjust-zero-fb`,
  `open` or `open-dr`. See details.

- verbose:

  Logical value to indicate the print the parameter estimates at each
  iteration of the rescale, as used in `ipf2`. By default `FALSE`.

## Value

Returns a `list` object with:

- m1_adj :

  Matrix of adjusted `m1` where rows (place of births) match `m2_adj`.

- m2_adj :

  Matrix of adjusted `m2` where rows (place of births) match `m1_adj`.

- in_mat :

  Matrix of estimated inflows into the system.

- out_mat :

  Matrix of estimated outflows from the system.

## Details

The `rescale` and `rescale-adjust-zero-fb` method ensure flow estimates
closely match the net migration totals implied by the changes in
population totals, births and deaths - as introduced in the Science
paper. The `rescale-adjust-zero-fb` can adjust for rare cases when row
total margins that are smaller than native born totals in countries
where there are no foreign born populations (e.g. South Sudan
1990-1995). The `open-dr` method allows for moves in and out of the
global system - as introduced in the Demographic Research paper. The
`open` method is a slight improvement over `open-dr` - the calculation
of the moves and in and out using more sensible weights.

## References

Abel and Cohen (2019) Bilateral international migration flow estimates
for 200 countries *Scientific Data* 6 (1), 1-13

Azose & Raftery (2019) Estimation of emigration, return migration, and
transit migration between all pairs of countries *Proceedings of the
National Academy of Sciences* 116 (1) 116-122

Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by
Gender between 1960 and 2015. *International Migration Review* 52 (3),
809–852.

Abel, G. J. and Sander, N. (2014). Quantifying Global International
Migration Flows. *Science*, 343 (6178) 1520-1522

## See also

[`ffs_demo`](ffs_demo.md)

## Author

Guy J. Abel
