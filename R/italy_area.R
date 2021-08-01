#' Single year age-specific origin destination migration flows between Italian NUTS1 areas
#'
#' Origin-destination migration flows from 7 years between 1970 and 2000 by five-year age groups
#'
#' @format Data frame with 3500 rows and 5 columns:
#' \describe{
#'   \item{orig}{Origin area (NUTS1 region)}
#'   \item{dest}{Destination area (NUTS1 region)}
#'   \item{year}{Year of flow}
#'   \item{age_grp}{Five-year age group}
#'   \item{flow}{Migration flow}
#' }
#' @source Provided by James Raymer. Originally from ISTAT. 2003. Rapporto annuale: La situazione nel Paese nel 2003. ISTAT, Rome. 
#' 
#' Data used in Raymer, J., Bonaguidi, A., & Valentini, A. (2006). Describing and projecting the age and spatial structures of interregional migration in Italy. Population, Space and Place, 12(5), 371–388. \url{https://doi.org/10.1002/psp.414}
"italy_area"