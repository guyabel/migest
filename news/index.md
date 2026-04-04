# Changelog

## migest 2.0.6

CRAN release: 2026-04-04

- Stash all `net_matrix_*` functions in dev folder

## migest 2.0.5

CRAN release: 2025-07-03

- Added [`net_matrix_ipf()`](../reference/net_matrix_ipf.md),
  `net_matrix_optim()`, `net_matrix_lp()` and `net_matrix_entropy()`
  functions.
- Replaced `orig_col`, `dest_col` and `flow_col` argument names in,
  [`sum_unilat()`](../reference/sum_region.md),
  [`sum_region()`](../reference/sum_region.md) and
  [`sum_country()`](../reference/sum_region.md) with `orig`, `dest` and
  `flow`.
- Allow `orig`, `dest` and `flow` argument names in, `sum_bilateral`,
  [`sum_region()`](../reference/sum_region.md) and
  [`sum_country()`](../reference/sum_region.md) to be non-characters.
- Added [`sum_unilat()`](../reference/sum_region.md) alias of
  [`sum_region()`](../reference/sum_region.md) and
  [`sum_country()`](../reference/sum_region.md)
- Replaced `orig_col`, `dest_col` and `flow_col` argument names in all
  other functions
- Added `labels.pos.adjust = FALSE` in
  [`mig_chord()`](../reference/mig_chord.md)
- Default colour ordering in [`mig_chord()`](../reference/mig_chord.md)
  based on `union(.$orig, .$dest)`
- Include `na_rm` option in [`sum_region()`](../reference/sum_region.md)
  and [`sum_country()`](../reference/sum_region.md)

## migest 2.0.4

CRAN release: 2023-11-18

- Direct demo users to [`mig_chord()`](../reference/mig_chord.md)
  pkgdown site
- Fixed colour ordering in [`mig_chord()`](../reference/mig_chord.md)
  when `order` is set
- Added [`nb_non_zero()`](../reference/nb_non_zero.md) to deal with
  negative native born population estimates from World Bank data
- Changed function name of `rescale_nb()` to
  [`nb_scale_global()`](../reference/nb_scale_global.md)
- Changed function name of `match_pob_tot()` to
  [`match_birthplace_tot()`](../reference/match_birthplace_tot.md)
- Changed argument names in [`ffs_demo()`](../reference/ffs_demo.md),
  [`ffs_diff()`](../reference/ffs_diff.md) and
  [`ffs_rates()`](../reference/ffs_rates.md) to more intuitive values
  (`stock_start` instead of `s1`, `stock_end` instead of `s2`, `births`
  instead of `b`, `deaths` instead of `d` and `seed` instead of `m`
- Added code to create `dict_ims`
- Added countries based on older political geography to `dict_ims` and
  column in data frame to indicate not in UN IMS data.
- Added `korea_gravity`, combining `korea_dist`, `korea_pop`, and
  `korea_reg` and additional distance, contiguity, area and economic
  measures.
- Removed `korea_dist`, `korea_pop`, and `korea_reg`
- Updated examples in `index_*()` functions for changes in Korean data

## migest 2.0.3

CRAN release: 2022-10-28

- Dropped `sum_turnover()` and `sum_bilateral()`
- Fixed axis upper limits in [`mig_chord()`](../reference/mig_chord.md)
- Added `label_squeeze` option to
  [`mig_chord()`](../reference/mig_chord.md)
- Added `region_ac2022` and `region_wb` column to `dict_ims`
- Replaced `ipf` functions in [`ffs_demo()`](../reference/ffs_demo.md)
  and sub-functions with
  [`mipfp::Ipfp`](https://rdrr.io/pkg/mipfp/man/Ipfp.html)
- Added `return` option in [`ffs_demo()`](../reference/ffs_demo.md) to
  output only estimted flow array unless otherwise stated
- Added `name_short` column to `dict_ims`
- Simplified [`sum_od()`](../reference/sum_od.md) code to directly work
  with outputs from [`ffs_demo()`](../reference/ffs_demo.md) and provide
  margin sums

## migest 2.0.2

CRAN release: 2021-12-02

- Added method option to [`birth_mat()`](../reference/birth_mat.md)
- Added [`sum_bilat()`](../reference/sum_bilat.md) for `sum_bilateral()`
- Added [`sum_country()`](../reference/sum_region.md)

## migest 2.0.1

CRAN release: 2021-10-30

- Added a `NEWS.md` file to track changes to the package.
- Added hex logo
- Added pkgdown
- Github actions working
- Added [`str_wrap_n()`](../reference/str_wrap_n.md)
- Added [`mig_chord()`](../reference/mig_chord.md)
- Added `dict_ims`
- Replace `sum_turnover()` with
  [`sum_region()`](../reference/sum_region.md)
- Replace `counter()` with `sum_bilateral()`
