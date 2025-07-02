# migest 2.0.5

* Added `net_matrix_ipf()`, `net_matrix_optim()`, `net_matrix_lp()` and `net_matrix_entropy()` functions.
* Replaced `orig_col`, `dest_col` and `flow_col` argument names in, `sum_unilat()`, `sum_region()` and `sum_country()` with `orig`, `dest` and `flow`.
* Allow `orig`, `dest` and `flow` argument names in, `sum_bilateral`, `sum_region()` and `sum_country()` to be non-characters.
* Added `sum_unilat()` alias of `sum_region()` and `sum_country()`
* Replaced `orig_col`, `dest_col` and `flow_col` argument names in all other functions
* Added `labels.pos.adjust = FALSE` in `mig_chord()`
* Default colour ordering in `mig_chord()` based on `union(.$orig, .$dest)`
* Include `na_rm` option in `sum_region()` and `sum_country()`

# migest 2.0.4

* Direct demo users to `mig_chord()` pkgdown site
* Fixed colour ordering in `mig_chord()` when `order` is set
* Added `nb_non_zero()` to deal with negative native born population estimates from World Bank data
* Changed function name of `rescale_nb()` to `nb_scale_global()`
* Changed function name of `match_pob_tot()` to `match_birthplace_tot()`
* Changed argument names in `ffs_demo()`, `ffs_diff()` and `ffs_rates()` to more intuitive values (`stock_start` instead of `s1`, `stock_end` instead of `s2`, `births` instead of `b`, `deaths` instead of `d` and `seed` instead of `m`
* Added code to create `dict_ims`
* Added countries based on older political geography to `dict_ims` and column in data frame to indicate not in UN IMS data.
* Added `korea_gravity`, combining `korea_dist`, `korea_pop`, and `korea_reg` and additional distance, contiguity, area and economic measures. 
* Removed `korea_dist`, `korea_pop`, and `korea_reg`
* Updated examples in `index_*()` functions for changes in Korean data

# migest 2.0.3

* Dropped `sum_turnover()` and `sum_bilateral()`
* Fixed axis upper limits in `mig_chord()`
* Added `label_squeeze` option to `mig_chord()`
* Added `region_ac2022` and `region_wb` column to `dict_ims`
* Replaced `ipf` functions in `ffs_demo()` and sub-functions with `mipfp::Ipfp`
* Added `return` option in `ffs_demo()` to output only estimted flow array unless otherwise stated
* Added `name_short` column to `dict_ims`
* Simplified `sum_od()` code to directly work with outputs from `ffs_demo()` and provide margin sums


# migest 2.0.2

* Added method option to `birth_mat()`
* Added `sum_bilat()` for `sum_bilateral()`
* Added `sum_country()`

# migest 2.0.1

* Added a `NEWS.md` file to track changes to the package.
* Added hex logo
* Added pkgdown
* Github actions working
* Added `str_wrap_n()`
* Added `mig_chord()`
* Added `dict_ims`
* Replace `sum_turnover()` with `sum_region()`
* Replace `counter()` with `sum_bilateral()`
