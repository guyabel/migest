#' US population totals in 1950 and 1960 by place of birth, age, sex and race
#'
#' Population data by place of birth, age, sex and race in 1950 and 1960 
#'
#' @format Data frame with 288 rows and 7 columns:
#' \describe{
#'   \item{birthplace}{Place of birth (US Census area)}
#'   \item{race}{Race from `white` or `non-white`}
#'   \item{sex}{Sex from `male` or `female`}
#'   \item{age_1950}{Age group in 1950}
#'   \item{age_1960}{Age group in 1960}
#'   \item{pop_1950}{Enumerated population in 1950}
#'   \item{pop_1960}{Enumerated population in 1960}
#' }
#' @source Data scraped from Hope T. Eldridge, Net Intercensal Migration for States and Geographic Divisions of the United States, 1950-1960: Methodological and Substantive Aspects. Analytical and Technical Report No.5 (Population, Studies Center,                                      University of Pennsylvania, Philadelphia, 1965), table D, pp. 183-191.
"usa_1960"