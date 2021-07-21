#' Summary indices of migration intensity
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param pop_total Numeric value or vector for the total population. If a vector greater of length one is given, values will be summed. Can be a data frame, with column of populations labelled using `pop_col`
#' @param n Numeric value for the number of regions in `m`. If not given will be derived from the dimensions of `m`
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param pop_col Character string of the population column name (when \code{pop_total} is a data frame rather than a ]\code{vector})
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 2 summary measures where
#'   \item{cmp}{Crude migration probability from Bell et. al. (2002), sometimes known as crude migration intensity, e.g. Bernard (2017)}
#'   \item{courgeau_k}{Intensity measure of Courgeau (1973)}
#'   
#' @source Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247
#' Courgeau, D. (1973). Migrants et migrations. Population, 28(1), 95–129. https://doi.org/10.2307/1530972
#' Bernard, A., Rowe, F., Bell, M., Ueffing, P., Charles-Edwards, E., & Zhu, Y. (2017). Comparing internal migration across the countries of Latin America: A multidimensional approach. Plos One, 12(3), e0173895. https://doi.org/10.1371/journal.pone.0173895
#' 
#' @export
#'
#' @examples
#' # single year
#' index_intensity(
#'   m = subset(korea_reg, year == 2020),
#'   pop_total = subset(korea_pop, year == 2020),
#'   pop_col = "population"
#' )
#' 
#' # multiple years
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' korea_reg %>%
#'   nest(m = c(orig, dest, flow)) %>%
#'   left_join(korea_pop) %>%
#'   nest(p = c(region, population)) %>%
#'   mutate(i = map2(
#'     .x = m, .y = p,
#'     .f = ~index_intensity(
#'       m = .x, pop_total = .y, pop_col = "population", long = FALSE
#'     ))) %>%
#'   select(-m, -p) %>%
#'   unnest(i)
index_intensity <- function(m = NULL, 
                           pop_total = NULL, 
                           n = NULL,
                           long = TRUE, 
                           orig_col = "orig", dest_col = "dest", 
                           flow_col = "flow", pop_col = "pop"
){
  if(length(m) == 1)
    m <- matrix(m)
  # orig_col = "orig"; dest_col = "dest"; flow_col = "flow"; pop_col = "pop"; dist_col = "dist"
  orig <- dest <- flow <- pop <- cmp <- NULL
  d <- format_migration_tibble(
    m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col
  )$d

  if(is.vector(pop_total)){
    p <- tibble::tibble(pop = pop_total)
  }
  if(!is.vector(pop_total)){
    p <- pop_total %>%
      dplyr::rename(pop := !!pop_col)
  }
  
  if(is.null(n)){
    m_reg <- as.character(unique(c(d$orig, d$dest)))
    n <- length(m_reg)
  }

  d %>%
    {if(nrow(d) > 1) dplyr::filter(., orig != dest) else .} %>%
    dplyr::summarise(
      cmp = 100 * sum(flow)/sum(p$pop),
      courgeau_k = cmp/log(n^2)) %>%
    {if(long) tidyr::pivot_longer(data = ., cols = "cmp":"courgeau_k", names_to = "measure") else .}
}

