#' Summary indices of migration intensity
#'
#' @param mig_total Numeric value for the total number of migrations.
#' @param pop_total Numeric value for the total population. 
#' @param n Numeric value for the number of regions used in the definition of migration for `mig_total`.
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 2 summary measures where
#'   \item{cmp}{Crude migration probability from Bell et. al. (2002), sometimes known as crude migration intensity, e.g. Bernard (2017)}
#'   \item{courgeau_k}{Intensity measure of Courgeau (1973)}
#'   
#' @source Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247 
#' @source Courgeau, D. (1973). Migrants et migrations. Population, 28(1), 95–129. https://doi.org/10.2307/1530972
#' @source Bernard, A., Rowe, F., Bell, M., Ueffing, P., Charles-Edwards, E., & Zhu, Y. (2017). Comparing internal migration across the countries of Latin America: A multidimensional approach. Plos One, 12(3), e0173895. https://doi.org/10.1371/journal.pone.0173895
#' @md
#' @export
#'
#' @examples
#' # single year
#' library(dplyr)
#' m <- korea_reg %>%
#'   filter(year == 2020,
#'          orig != dest) %>%
#'   pull(flow) %>%
#'   sum()
#' m
#' p <- korea_pop %>%
#'   filter(year == 2020) %>%
#'   pull(population) %>%
#'   sum()
#' p
#' index_intensity(mig_total = m, pop_total = p, n = n_distinct(korea_pop$region))
#' 
#' # multiple years
#' mm <- korea_reg %>%
#'   group_by(year) %>%
#'   filter(orig != dest) %>%
#'   summarise(m = sum(flow))
#' mm
#' pp <- korea_pop %>%
#'   group_by(year) %>%
#'   summarise(p = sum(population))
#' pp
#' 
#' library(purrr)
#' library(tidyr)
#' mm %>%
#'   left_join(pp) %>%
#'   mutate(i = map2(.x = m, .y = p,
#'                   .f = ~index_intensity(mig_total = .x,
#'                                         pop_total = .y,
#'                                         n = n_distinct(korea_pop$region),
#'                                         long = FALSE))) %>%
#'   unnest(cols = i)
index_intensity <- function(mig_total = NULL,
                            pop_total = NULL,
                            n = NULL,
                            long = TRUE){
  cmp <- courgeau_k <- NULL
  tibble::tibble(cmp = 100 * mig_total/pop_total,
                 courgeau_k = cmp/log(n^2)) %>%
    {if(long) tidyr::pivot_longer(data = ., cols = 1:ncol(.), names_to = "measure") else .}
}

