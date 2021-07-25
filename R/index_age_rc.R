#' Summary indices of age migration profile based on parameters from a Rogers and Castro schedule
#'
#' @param pars Named vector or parameters parameters from a Rogers and Castro schedule
#'
#' @return A tibble with at least five summary measures
#' @source Rogers, A., & Castro, L. J. (1981). Model Migration Schedules. In IIASA Research Report (Vol. 81, Issue RR-81-30). http://webarchive.iiasa.ac.at/Admin/PUB/Documents/RR-81-030.pdf
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' rc_model_fund %>%
#'   deframe() %>%
#'   index_age_rc()
index_age_rc <- function(pars = NULL){
  child_dependency <- NULL
  # parameter name groups
  comp1 <- c("a1", "alpha1")
  comp2 <- c("a2", "alpha2", "lambda2", "mu2")

  # check for specific parameter groups
  if (any(comp1 %in% names(pars))){
    stopifnot(all(comp1 %in% names(pars)))
  }
  if (any(comp2 %in% names(pars))){
    stopifnot(all(comp2 %in% names(pars)))
  }
  p <- pars
  tibble::tibble(
    peaking = p[stringr::str_detect(string = names(p), pattern = "mu2")],
    child_dependency = 
      p[stringr::str_starts(string = names(p), pattern = "a1")]/
      p[stringr::str_starts(string = names(p), pattern = "a2")],
    labor_dependency = 1/child_dependency,
    labor_asymmetry =
      p[stringr::str_detect(string = names(p), pattern = "lambda2")]/
      p[stringr::str_detect(string = names(p), pattern = "alpha2")],
    regularity = 
      p[stringr::str_detect(string = names(p), pattern = "alpha1")]/
      p[stringr::str_detect(string = names(p), pattern = "alpha2")]
  ) %>%
    tidyr::pivot_longer(cols = 1:ncol(.)) %>%
    dplyr::rename(measure = 1)
}

