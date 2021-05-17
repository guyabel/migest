#' Multiplicative component description of origin-destination migration flow tables
#'
#' Multiplicative component descriptions of *n*-dimension flow tables based on total reference coding system.
#' 
#' @param m \code{matrix} or \code{array} of migration flows
#'
#' @return \code{matrix} or \code{array} of multiplicative components of `m`. When output is an array the total for each table of origin-destination flows is used. 
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' n <- LETTERS[1:4]
#' m0 <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0), 
#'              nrow = 4, ncol = 4, dimnames = list(orig = n, dest = n), byrow = TRUE)
#' addmargins(m0)
#' multi_comp(m = m0)
multi_comp <- function(m){
  d0 <- m %>%
    stats::addmargins() %>%
    as.data.frame.table(responseName = "obs") %>%
    dplyr::as_tibble()
  
  f1 <- d0 %>%
    dplyr::select(-obs) %>%
    names() %>%
    paste(collapse = " + ") %>%
    paste("obs ~", .)
  
  d1 <- d0 %>%
    dplyr::filter_at(dplyr::vars(1:"obs"), dplyr::all_vars(. != "Sum"))
  
  m_ind <- stats::glm(formula = stats::as.formula(f1), data = d1, family = stats::poisson())
  
  d1 <- d1 %>% 
    dplyr::mutate(fit = m_ind$fitted.values, 
                  interact = obs/fit)
  
  f2 <- stringr::str_replace(string = f1, pattern = "obs", replacement = "interact")
  
  d2 <- 
    d0 %>%
    dplyr::select(-obs) %>%
    dplyr::left_join(d1) %>%
    dplyr::mutate(prop = c(stats::addmargins(m/sum(m))),
                  comp = ifelse(test = is.na(obs), yes = prop, no = interact), 
                  comp = ifelse(test = dplyr::row_number() == dplyr::n(), yes = sum(d1$obs), no = comp))
  
  f2 %>%
    stringr::str_replace(pattern = "interact", replacement = "comp") %>%
    stats::xtabs(data = d2)
}
