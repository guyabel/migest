#' Age specific migration and population counts from two IPUMSI samples
#'
#' Age specific migration and population counts for Brazil 2000 and France 2006 IPUMS International samples. 
#' Attempt to recreate the unsmoothed data used in the appendix of Bernard, Bell and Charles-Edwards (2014)
#'
#' @format Data frame with 202 rows and 4 columns:
#' \describe{
#'   \item{sample}{IPUMS International sample - either BRA2000 or FRA2006}
#'   \item{age}{Age on census data}
#'   \item{migrants}{Number of migrants, defined by those who had changed usual place of residence to a different minor adminstrative region compared to usual place of residence five years prior to the cenuses. Obtained by summing person weights for `migrate5` variable equal to any of code 12, 20 or 30.}
#'   \item{population}{Population of each age group, obtained by summing person weights `perwt` variable. }
#' }
#' @source 
#' Minnesota Population Center. (2015). Integrated Public Use Microdata Series, International: Version 6.4 [Machine-readable database]. https://international.ipums.org/international/
#' Bernard, A., Bell, M., & Charles-Edwards, E. (2014). Improved measures for the cross-national comparison of age profiles of internal migration. Population Studies, 68(2), 179–195. https://doi.org/10.1080/00324728.2014.890243
"ipumsi_age"