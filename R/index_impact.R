#' Summary indices of migration impact
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param pop_total A data frame or named vector for the total population. When data frame, column of populations labelled using `pop_col` and region names labelled `reg_col`.
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param pop_col Character string of the population column name 
#' @param reg_col Character string of the region column name. Must match dimension names or values in origin and destination columns of `m`.
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 3 summary measures where
#'   \item{effectivness}{Migration effectiveness index (MEI) from Shryock et al. (1975). Values range between 0 and 100. High values indicate migration is an efficient mechanism of population redistribution, generating a large net migration. Conversely, low values denote that migration is closely balanced, leading to comparatively little redistribution.}
#'   \item{anmr}{Aggregate net migration rate from Bell et. al. (2002). The population weighted version of `mei`.}
#'   \item{perference}{Index of Preference from Bachi (1957) and Shryock et al. (1975) - measures size of migration compared to expected flows based on population size alone. Can go from 0 to infinity}
#' @export
#' 
#' @source  Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247
#' Shryock, H. S., & Siegel, J. S. (1976). The Methods and Materials of Demography. (E. G. Stockwell (ed.); Condensed). Academic Press.
#'
#' @examples
#' # single year
# index_impact(
#   m = subset(korea_reg, year == 2020),
#   pop_total = subset(korea_pop, year == 2020),
#   pop_col = "population"
# )
index_impact <- function(m, pop_total, 
                         pop_col = "pop", 
                         reg_col = "region",
                         orig_col = "orig", dest_col = "dest", flow_col = "flow",
                         long = TRUE){
  orig <- dest <- flow <- pop <- reg <- net <- turn <- NULL
  if(!is.matrix(m)){
    m <- format_migration_matrix(m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col)
    diag(m) <- 0
  }
  r <- unique(row.names(m), colnames(m))
  
  p <- pop_total %>%
    dplyr::rename(pop := !!pop_col, 
                  reg := !!reg_col) %>%
    dplyr::filter(reg %in% r) %>%
    dplyr::arrange(reg)
  
  m <- m[sort(r), sort(r)]

  
  m %>%
    sum_turnover() %>%
    dplyr::summarise(effectivness = 100 * sum(abs(net)) / sum(turn),
              anmr = 100 * 0.5 * sum(abs(net)) / sum(p$pop),
              preference = sum(m / (sum(m) * (p$pop/sum(p$pop)) %*% (t(p$pop/sum(p$pop))))),
              # velocity = sum(m / (p$pop %*% t(p$pop)) * sum(p$pop))
    ) %>%
    {if(long) tidyr::pivot_longer(data = ., cols = "effectivness":"velocity", names_to = "measure") else .}
}