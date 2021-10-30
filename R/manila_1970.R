#' Manila female population 1970 by age
#'
#' Population data for Manila by age in 1960 and 1970
#'
#' @format Data frame with 13 rows and 5 columns:
#' \describe{
#'   \item{age_1970}{Age group in 1970}
#'   \item{pop_1960}{Enumerated population in 1960}
#'   \item{pop_1970}{Enumerated population in 1970}
#'   \item{phl_census_sr}{Census survival ratio derived from the national data.}
#' }
#' @source Scraped from Table 6 of United Nations Department of Economic and Social Affairs Population Division. (1992). Preparing Migration Data for Subnational Population Projections.
#' @examples
#' # match table 6 - perhaps small error in children net migration numbers in the published table?
#' net_sr(manila_1970, pop0_col = "pop_1960", pop1_col = "pop_1970", 
#'        survival_ratio_col = "phl_census_sr", net_children = TRUE)
"manila_1970"
