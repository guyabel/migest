#' Alabama population totals in 1960 and 1970 by age, sex and race
#'
#' Population data for Alabama by age, sex and race in 1960 and 1970 .
#'
#' @format Data frame with 68 rows and 6 columns:
#' \describe{
#'   \item{age_1970}{Age group in 1970}
#'   \item{sex}{Sex from `male` or `female`}
#'   \item{race}{Race from `white` or `non-white`}
#'   \item{pop_1960}{Enumerated population in 1960. Number of births in first and second half of 1960s used for age groups `0-4` and `5-9`.}
#'   \item{pop_1970}{Enumerated population in 1970}
#'   \item{us_census_sr}{Census survival ratio based on US population}
#' }
#' @source Data scraped from Figure 2.3 and Table 1-3A of Bogue, D. J., Hinze, K., & White, M. (1982). Techniques of Estimating Net Migration. Community and Family Study Center. University of Chicago.
"alabama_1970"
