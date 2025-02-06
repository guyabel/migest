#' Summary indices of migration impact
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig}, \code{dest} and \code{flow}.
#' @param p A data frame or named vector for the total population. When data frame, column of populations labelled using `pop` and region names labelled `reg`.
#' @param orig Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param pop Character string of the population column name
#' @param reg Character string of the region column name. Must match dimension names or values in origin and destination columns of `m`.
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 4 summary measures where
#'   \item{effectivness}{Migration effectiveness index (MEI) from Shryock et al. (1975). Values range between 0 and 100. High values indicate migration is an efficient mechanism of population redistribution, generating a large net migration. Conversely, low values denote that migration is closely balanced, leading to comparatively little redistribution.}
#'   \item{anmr}{Aggregate net migration rate from Bell et. al. (2002). The population weighted version of `mei`.}
#'   \item{perference}{Index of preference, given in UN DESA (1983). From Bachi (1957) and Shryock et al. (1975) - measures size of migration compared to expected flows based on unifrom migration. Can go from 0 to infinity}
#'   \item{velocity}{Index of velocity, given in UN DESA (1983). From Bogue, Shryock, Jr. & Hoermann (1957) - measures size of migration compared to expected flows based on population size alone. Can go from 0 to infinity}
#' @export
#'
#' @source Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247 \cr
#' @source Shryock, H. S., & Siegel, J. S. (1976). The Methods and Materials of Demography. (E. G. Stockwell (ed.); Condensed). Academic Press. \cr
#' @source United Nations Department of Economic and Social Affairs Population Division. (1970). Methods of measuring internal migration. United Nations Department of Economic and Social Affairs Population Division - 1970 - Methods of measuring internal migration \url{https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/files/documents/2020/Jan/manual_vi_methods_of_measuring_internal_migration.pdf}
#' @md
#'
#' @examples
#' # single year
#' library(dplyr)
#' m <- korea_gravity %>%
#'   filter(year == 2020,
#'          orig != dest) %>%
#'   select(orig, dest, flow)
#' m
#' p <- korea_gravity %>%
#'   filter(year == 2020) %>%
#'   distinct(dest, dest_pop)
#' p
#' index_impact(m = m, p = p, pop = "dest_pop", reg = "dest")
#'
#' # multiple years
#' library(tidyr)
#' library(purrr)
#'
#' korea_gravity %>%
#'   select(year, orig, dest, flow, dest_pop) %>%
#'   group_nest(year) %>%
#'   mutate(m = map(.x = data, .f = ~select(.x, orig, dest, flow)),
#'          p = map(.x = data, .f = ~distinct(.x, dest, dest_pop)),
#'          i = map2(.x = m, .y = p,
#'                   .f = ~index_impact(
#'                     m = .x, p = .y, pop = "dest_pop", reg = "dest", long = FALSE
#'                   ))) %>%
#'   select(-data, -m, -p) %>%
#'   unnest(i)
index_impact <- function(m, p,
                         pop = "pop",
                         reg = "region",
                         orig = "orig", dest = "dest", flow = "flow",
                         long = TRUE){
  if(!is.character(orig)){
    orig <- as.name(substitute(orig))
  }
  if(!is.character(dest)){
    dest <- as.name(substitute(dest))
  }
  if(!is.character(flow)){
    flow <- as.name(substitute(flow))
  }
  pop <- reg <- net <- turn <- NULL
  if(!is.matrix(m)){
    m <- mig_matrix(m = m, orig = orig, dest = dest, flow = flow)
    diag(m) <- 0
  }
  r <- unique(row.names(m), colnames(m))

  p <- p %>%
    dplyr::rename(pop := !!pop,
                  reg := !!reg) %>%
    dplyr::filter(reg %in% r) %>%
    dplyr::arrange(reg)

  m <- m[sort(r), sort(r)]


  m %>%
    sum_region() %>%
    dplyr::summarise(
      effectivness = 100 * sum(abs(net)) / sum(turn),
      anmr = 100 * 0.5 * sum(abs(net)) / sum(p$pop),
      preference = sum(m / (sum(m) * (p$pop/sum(p$pop)) %*% (t(p$pop/sum(p$pop))))),
      velocity = sum(m / (p$pop %*% t(p$pop)) * sum(p$pop))
    ) %>%
    {if(long) tidyr::pivot_longer(data = ., cols = 1:ncol(.), names_to = "measure") else .}
}
