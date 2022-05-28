#' Dictionary to look up region geographies based on countries used in UN DESA International Migrant Stock.
#'
#' Intended for use as a custom dictionary with the countrycode package, where the existing UN region and area codes do not match those used by UN DESA in the WPP, see \url{https://github.com/vincentarelbundock/countrycode/issues/253}
#' 
#' @source The aggregates_correspondence_table_2020_1.xlsx file of United Nations Department of Economic and Social Affairs, Population Division (2020). International Migrant Stock 2020.
#' 
#' @format Data frame with 237 rows and 18 columns. One of first three columns intended as input for \code{origin} in \code{countrycode}.
#' \describe{
#'   \item{name}{Country name}
#'   \item{iso3c}{ISO numeric code}
#'   \item{iso3n}{ISO 3 letter code}
#' }
#' Remaining columns intended as input for \code{destination} in \code{countrycode}.
#' \describe{
#'   \item{name_short}{Short country name }
#'   \item{region}{Geographic region of country (6)}
#'   \item{region_sub}{Geographic sub region of country (22). Filled using \code{region} if none given in original data}
#'   \item{region_sdg}{SDG region of country (8)}
#'   \item{region_sdg_sub}{Sub SDG region of country (9). Filled using \code{region_sdg} if none given in original data}
#'   \item{un_develop}{UN development group of country (3)}
#'   \item{wb_income}{World Bank income group of country (3)}
#'   \item{wb_income_detail}{Detailled World Bank income group of country (4)}
#'   \item{lldc}{Indicator variable for Land-Locked Developing Countries (32)}
#'   \item{sids}{Indicator variable for Small Island Developing States (58)}
#'   \item{region_as2014}{Region grouping used for global chord diagram plots by Abel and Sander (2014)}
#'   \item{region_sab2014}{Region grouping used for global chord diagram plots by Sander, Abel and Bauer (2014)}
#'   \item{region_a2018}{Region grouping used for global chord diagram plots by Abel (2018)}
#'   \item{region_ac2022}{Region grouping used for global chord diagram plots by Abel and Cohen (2022)}
#'   \item{region_wb}{World Bank region}
#' }
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' library(countrycode)
#' # download Abel and Cohen (2019) estimates
#' f <- read_csv("https://ndownloader.figshare.com/files/26239945")
#' 
#' # use dictionary to get region to region flows
#' d <- f %>%
#'   mutate(
#'     orig = countrycode(
#'       sourcevar = orig, custom_dict = dict_ims,
#'       origin = "iso3c", destination = "region"),
#'     dest = countrycode(
#'       sourcevar = dest, custom_dict = dict_ims,
#'       origin = "iso3c", destination = "region")
#'   ) %>%
#'   group_by(year0, orig, dest) %>%
#'   summarise_all(sum)
#' d
#' }
"dict_ims"