# Methods for the Indirect Estimation of Bilateral Migration

The migest package contains a collection of R functions for indirect
methods to estimate bilateral migration flows in the presence of partial
or missing data. Methods might be relevant to other categorical data
situations on non-migration data, where for example, marginal totals are
known and only auxiliary bilateral data is available.

## Details

|          |         |
|----------|---------|
| Package: | migest  |
| Type:    | Package |
| License: | GPL-2   |

The estimation methods in this package can be grouped as 1) functions
for origin-destination matrices ([`cm2`](cm2.md) and [`ipf2`](ipf2.md))
and 2) functions for origin-destination matrices categorized by a
further set of characteristics, such as ethnicity, employment or health
status ([`cm3`](cm3.md), [`ipf3`](ipf3.md) and [`ipf3_qi`](ipf3_qi.md)).
Each of these routines are based on indirect estimation methods where
marginal totals are known, and a Poisson regression (log-linear) model
is assumed.

The [`ffs_diff`](ffs_diff.md), [`ffs_rates`](ffs_rates.md) and
[`ffs_demo`](ffs_demo.md) functions provide different methods to
estimate migration bilateral flows from changes in stocks, see Abel and
Cohen (2019) for a review of different methods. The demo files,
`demo(cfplot_reg2)`, `demo(cfplot_reg)` and `demo(cfplot_nat)`, produce
circular migration flow plots for migration estimates from Abel(2018)
and Abel and Sander (2014), which were derived using the
[`ffs_demo`](ffs_demo.md) function.

Github repo: <https://github.com/guyabel/migest>

## Author

Guy J. Abel

## References

Abel and Cohen (2019) Bilateral international migration flow estimates
for 200 countries *Scientific Data* 6 (1), 1-13

Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by
Gender between 1960 and 2015. *International Migration Review* 52 (3),
809–852.

Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place
of Birth. *Demographic Research* 28, (18) 505-546

Abel, G. J. (2005) *The Indirect Estimation of Elderly Migrant Flows in
England and Wales* (MS.c. Thesis). University of Southampton

Abel, G. J. and Sander, N. (2014). Quantifying Global International
Migration Flows. *Science*, 343 (6178) 1520-1522

Raymer, J., G. J. Abel, and P. W. F. Smith (2007). Combining census and
registration data to estimate detailed elderly migration flows in
England and Wales. *Journal of the Royal Statistical Society: Series A
(Statistics in Society)* 170 (4), 891–908.

Willekens, F. (1999). Modelling Approaches to the Indirect Estimation of
Migration Flows: From Entropy to EM. *Mathematical Population Studies* 7
(3), 239–78.
