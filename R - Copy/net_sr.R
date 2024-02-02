#' Estimate net migration from survival ratios applied to lifetime migration data
#'
#' @description Using survival ratios to estimate net migration from lifetime migration data
#'
#' @param .data A data frame with two rows with the total number of lifetime in- and out-migrants in separate columns. The first row contains totals at the first time point and second row at the second time point. 
#' @param pop0_col Character string name of column containing name of initial populations. Default \code{"pop0"}.
#' @param pop1_col Character string name of column containing name of end populations. Default \code{"pop1"}.
#' @param survival_ratio_col Character string name of column containing survivor ratios. Default \code{"sr"}.
#' @param net_children Logical to indicate if to estimate net migration when no survival ratio exists. Default `FALSE`.
#' @param maternal_exposure Vector for maternal exposures to interval to be used to estimate net migration for each of the unknown children age groups. Length should correspond to the number of children age groups where net migration estimates are required.
#' @param maternal_age_id Row numbers to indicate which rows correspond to maternal age groups at the end of the period. 
#' @param maternal_col Name of maternal population column, required for the estimation of net migration of children. 
#'
#' @return Data frame with estimates of net migration
#' @export
#' 
#' @references Bogue, D. J., Hinze, K., & White, M. (1982). Techniques of Estimating Net Migration. Community and Family Study Center. University of Chicago.
#' 
#' @examples 
#' # results to match un manual 1984 (table 24)
#' net_sr(bombay_1951, pop0_col = "pop_1941", pop1_col = "pop_1951")
#'   
#' # results to match Bogue, Hinze and White (1982)
#' library(dplyr)
#' alabama_1970 %>%
#'   filter(race == "white", sex == "male") %>%
#'   select(-race, -sex) %>%
#'   group_by(age_1970) %>%
#'   net_sr(pop0_col = "pop_1960", pop1_col = "pop_1970", 
#'          survival_ratio_col = "us_census_sr")
#'          
#' # results to match UN manual 1992 (table 6)
#' net_sr(manila_1970, pop0_col = "pop_1960", pop1_col = "pop_1970", 
#'        survival_ratio_col = "phl_census_sr")
#'        
#' # with children net migration estimate
#' net_sr(manila_1970, pop0_col = "pop_1960", pop1_col = "pop_1970", 
#'        survival_ratio_col = "phl_census_sr", net_children = TRUE)
net_sr <- function(.data, 
                   pop0_col = "pop0",
                   pop1_col = "pop1",
                   survival_ratio_col = "sr",
                   net_children = FALSE,
                   maternal_exposure = c(0.25, 0.75),
                   maternal_age_id = 4:9,
                   maternal_col = pop1_col){
  # .data = manila_1970; pop0_col = "pop_1960"; pop1_col = "pop_1970"
  # survival_ratio_col = "phl_census_sr"; 
  # net_children = FALSE;
  # net_children = TRUE;
  # maternal_exposure = c(0.25, 0.75)
  # maternal_age_id = 4:9
  # maternal_col = pop1_col
  pop0 <- pop1 <- sr <- pop1_forward <- pop0_reverse <- NULL
  net_forward <- net_reverse <- net_average <- NULL
  d <- .data %>%
    dplyr::rename(pop0 := !!pop0_col,
                  pop1 := !!pop1_col,
                  sr := !!survival_ratio_col) %>%
    dplyr::mutate(pop1_forward = pop0 * sr,
                  net_forward = pop1 - pop1_forward,
                  pop0_reverse = pop1 * 1/sr,
                  net_reverse = pop0_reverse - pop0,
                  net_average = (net_forward + net_reverse)/2) %>%
    dplyr::relocate(-pop1_forward, -pop0_reverse)
  
  if(net_children){
    n <- length(maternal_exposure)
    mat_pop <- unlist(c(.data[,maternal_col]))
    m <- d %>%
      dplyr::select(net_forward, net_reverse, net_average)
    
    for(j in 1:3){
      xx <- NULL
      for(i in 1:n){
        x <- maternal_exposure[i] * 
          (mat_pop[i] / sum(mat_pop[maternal_age_id + i - 1])) *
          sum(m[maternal_age_id + i - 1, j])
        xx <- c(xx, x)
      }
      m[1:n, j] <- c(xx)
    }
    d <- d %>%
      dplyr::mutate(net_forward = m$net_forward,
                    net_reverse = m$net_reverse, 
                    net_average = m$net_average)
  }
  d <- d %>%
    tidyr::replace_na(list(net_forward = 0, net_reverse = 0, net_average = 0)) %>%
    dplyr::rename(!!pop0_col := pop0,
                  !!pop1_col := pop1,
                  !!survival_ratio_col := sr) %>%
    dplyr::as_tibble()  

  return(d)
}
