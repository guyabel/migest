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
