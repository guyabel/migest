#' Estimate net migration from vital statistics
#'
#' @param .data A data frame with two rows with the total number of lifetime in- and out-migrants in separate columns. The first row contains totals at the first time point and second row at the second time point. 
#' @param pop0_col Character string name of column containing name of initial populations. Default \code{"pop0"}.
#' @param pop1_col Character string name of column containing name of end populations. Default \code{"pop1"}.
#' @param births_col Character string name of column containing name of births over the period. Default \code{"births"}.
#' @param deaths_col Character string name of column containing name of deaths over the period. Default \code{"deaths"}.
#'
#' @return A tibble with additional columns for the population change (`pop_change`), the natural population increase (`natural_inc`) and the net migration (`net`) over the period.
#' @export
#' 
#' @references Bogue, D. J., Hinze, K., & White, M. (1982). Techniques of Estimating Net Migration. Community and Family Study Center. University of Chicago.
#'
#' @examples
#' library(dplyr)
#' d <- alabama_1970 %>%
#'   group_by(race, sex) %>%
#'   summarise(births = sum(pop_1960[1:2]),
#'             pop_1960 = sum(pop_1960) - births,
#'             pop_1970 = sum(pop_1970)) %>%
#'   ungroup()
#' d
#' 
#' d %>%
#'   mutate(deaths = c(51449, 58845, 86880, 123220)) %>%
#'   net_vs(pop0_col = "pop_1960", pop1_col = "pop_1970")
net_vs <- function(.data, pop0_col = NULL, pop1_col = NULL, 
                   births_col = "births", deaths_col = "deaths"){
  pop0 <- pop1 <- births <- deaths <- pop_change <- natural_inc <- NULL
  .data %>%
    dplyr::rename(pop0 := !!pop0_col,
                  pop1 := !!pop1_col,
                  births := !!births_col,
                  deaths := !!deaths_col) %>%
    dplyr::mutate(pop_change = pop1 - pop0,
                  natural_inc = births - deaths,
                  net = pop_change - natural_inc) %>%
    dplyr::rename(!!pop0_col := pop0,
                  !!pop1_col := pop1,
                  !!births_col := births,
                  !!deaths_col := deaths) %>%
    # dplyr::select(pop_change:net) %>%
    dplyr::as_tibble()
}