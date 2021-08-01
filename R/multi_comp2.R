#' Multiplicative component descriptions of origin-destination flow tables based on total reference coding system.
#' 
#' Multiplicative component descriptions of origin-destination flow tables based on total reference coding system.
#' 
#' @param m \code{matrix} of migration flows
#'
#' @return \code{matrix} of multiplicative components of `m`. When output is an array the total for each table of origin-destination flows is used. 
#' @importFrom magrittr "%>%"
#' @import utils
#' 
#' @export
#' 
#' @references Rogers, A., Willekens, F., Little, J., & Raymer, J. (2002). Describing migration spatial structure. Papers in Regional Science, 81(1), 29–48. https://doi.org/10.1007/s101100100090
#' 
#' @references Raymer, J., Bonaguidi, A., & Valentini, A. (2006). Describing and projecting the age and spatial structures of interregional migration in Italy. Population, Space and Place, 12(5), 371–388. https://doi.org/10.1002/psp.414
#'
#' @examples
#' r <- LETTERS[1:2]
#' m0 <- array(c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2),
#'             dimnames = list(orig = r, dest = r, type = c("ILL", "HEALTHY")))
#' addmargins(m0)
#' multi_comp2(m = m0)
multi_comp2 <- function(m){
  obs <- comp <- m_mc <- m_od <- d_mc <- NULL
  if(length(dim(m)) == 2)
    mm <- multi_comp(m = m)
  if(length(dim(m)) > 2){
    d0 <- m %>%
      as.data.frame.table(responseName = "obs") %>%
      dplyr::as_tibble()
    
    f0 <- d0 %>%
      dplyr::select(1:2) %>%
      names() %>%
      paste(collapse = " + ") %>%
      paste("obs ~", .)
    
    f1 <- d0 %>%
      dplyr::select(-obs) %>%
      names() %>%
      paste(collapse = " + ") %>%
      paste("Freq ~", .)
    
    mm <- d0 %>%
      dplyr::group_by_at(.vars = names(.)[-c(1:2, ncol(.))]) %>%
      tidyr::nest() %>%
      dplyr::mutate(m_od = purrr::map(.x = data, .f = function(x) stats::xtabs(formula = f0, data = x)),
                    m_mc = purrr::map(.x = m_od, .f = function(x) multi_comp2(x)),
                    d_mc = purrr::map(.x = m_mc, .f = function(x) as.data.frame.table(x))) %>%
      dplyr::select(-data, -m_od, -m_mc) %>%
      tidyr::unnest(cols = d_mc) %>%
      stats::xtabs(formula = f1)
  }
  return(mm)
}