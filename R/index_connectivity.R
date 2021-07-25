#' Summary indices of migration connectivity
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param gini_orig_all Logical to include gini index values for all origin regions. Default `FALSE`.
#' @param gini_dest_all Logical to include gini index values for all destination regions. Default `FALSE`.
#' @param gini_corrected Logical to use corrected denominator in Gini index of Bell (2002) or original of David A. Plane and Mulligan (1997)
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 12 summary measures:
#' \item{connectivity}{I_{mc} of Bell et. al. (2002) for the share of non-zero flows. A value of 0 means no connections (all zero flows) and 1 shows that all regions are connected by migrants.}
#' \item{inequality_equal}{I_{mi} of Bell et. al. (2002) based on a distributions of flows compared to equal distributions of expected flows . A value of 0 shows complete equality in flows and 1 shows maximum inequality.}
#' \item{inequality_sim}{I_{mi} of Bell et. al. (2002) based on a distributions of flows compared to distributions of expected flows from a Poisson regression independence fit `flow ~ orig +  dest`. A value of 0 shows complete equality in flows and 1 shows maximum inequality.}
#' \item{gini_total}{Overall concentration of migration from Bell (2002), corrected from Plane and Mulligan (1997). A value of 0 means no spatial focusing and 1 shows that all migrants are found in one single flow. Calculated using `migration.indices::migration.gini.total()`}
#' \item{gini_orig_standardized}{Relative extent to which the origin selections of out-migrations are spatially focused. A value of 0 means no spatial focusing and 1 shows maximum focusing. Adapted from `migration.indices::migration.gini.row.standardized()`.}
#' \item{gini_dest_standardized}{Relative extent to which the destination selections of in-migrations are spatially focused. A value of 0 means no spatial focusing and 1 shows maximum focusing. Adapted from `migration.indices::migration.gini.col.standardized()`.}
#' \item{mwg_orig}{Origin spatial focusing, from Bell et. al. (2002). Calculated using `migration.indices::migration.weighted.gini.out()`}
#' \item{mwg_dest}{Destination spatial focusing, from Bell et. al. (2002). Calculated using `migration.indices::migration.weighted.gini.in()`}
#' \item{mwg_mean}{Mean spatial focusing, from Bell et. al. (2002). Average of the origin and destination migration weighted Gini indices (`mwg_orig` and `mwg_dest`). A value of 0 means no spatial focusing and 1 shows that all migrants are found in one region. Calculated using `migration.indices::migration.weighted.gini.mean()`}
#' \item{cv}{Coefficient of variation from Rogers and Raymer (1998).}
#' \item{acv}{Aggregated system-wide coefficient of variation from Rogers and Sweeney (1998), using `migration.indices::migration.acv()`}
#'  
#' @source Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247 \cr
#' @source Rogers, A., & Raymer, J. (1998). The Spatial Focus of US Interstate Migration Flows. International Journal of Population Geography, 4(1), 63–80. https://doi.org/10.1002/(SICI)1099-1220(199803)4%3A1<63%3A%3AAID-IJPG87>3.0.CO%3B2-U  \cr
#' @source Rogers, A., & Sweeney, S. (1998). Measuring the Spatial Focus of Migration Patterns. Professional Geographer, 50(2), 232–242.  \cr
#' @source Plane, D., & Mulligan, G. F. (1997). Measuring spatial focusing in a migration system. Demography, 34(2), 251–262. \cr
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' korea_reg %>%
#'   filter(year == 2020) %>%
#'   index_connectivity()
index_connectivity <- function(m = NULL, #inequality_expected =  c("equal", "weighted"),
                               gini_orig_all = FALSE, gini_dest_all = FALSE,
                               gini_corrected = TRUE, 
                               orig_col = "orig", dest_col = "dest", 
                               flow_col = "flow",
                               long = TRUE){
  
  # gini_orig_all = FALSE; gini_dest_all = FALSE;  gini_corrected = TRUE
  orig <- dest <- flow <- NULL
  if(!is.matrix(m)){
    m <- format_migration_matrix(m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col)
    diag(m) <- 0
  }
  d0 <- format_migration_tibble(m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col)$d %>%
    dplyr::filter(orig != dest)
  m0 <- stats::glm(formula = round(flow) ~ orig + dest, data = d0, family = "poisson")
  f0 <- stats::fitted(m0)

  d <- tibble::tibble(
    connectivity = migration.indices::migration.connectivity(m),
    inequality_equal = 0.5 * sum(abs(d0$flow/sum(d0$flow) - mean(d0$flow)/sum(d0$flow))),
    inequality_sim = 0.5 * sum(abs(d0$flow/sum(d0$flow) - f0/sum(f0))),
    # not sure about migration.indices::migration.inequality - bell (2002) says should be between 0 and 1
    #inequality = migration.indices::migration.inequality(m, expected = inequality_expected),
    gini_total = migration.indices::migration.gini.total(m, corrected = gini_corrected),
    gini_orig_standardized = migration.indices::migration.gini.row.standardized(m)/100,
    gini_dest_standardized = migration.indices::migration.gini.col.standardized(m)/100,
    mwg_orig = migration.indices::migration.weighted.gini.out(m = m),
    mwg_dest = migration.indices::migration.weighted.gini.in(m = m),
    mwg_mean = migration.indices::migration.weighted.gini.mean(m = m),
    cv = sqrt(sum((m -mean(m))^2)/(length(m) * (length(m)- 1))/mean(m)),
    acv = migration.indices::migration.acv(m)
  ) %>%
    tidyr::pivot_longer(data = ., cols = 1:ncol(.))
  # g <-
  #   migration.gini(m, corrected = gini_corrected) %>%
  #   unlist() %>%
  #   enframe() %>%
  #   mutate(name = stringr::str_remove(string = name, pattern = "migration."),
  #          name = janitor::make_clean_names(name),
  #          name = stringr::str_replace(string = name, pattern = "row", replacement = "orig"),
  #          name = stringr::str_replace(string = name, pattern = "col", replacement = "dest")) %>%
  #   {if(gini_orig_all) . else dplyr::filter(., stringr::str_detect(string = name, pattern = "gini_in_", negate = TRUE))} %>%
  #   {if(gini_orig_all) . else dplyr::filter(., stringr::str_detect(string = name, pattern = "gini_out_", negate = TRUE))} %>%
  #   filter(str_detect(string = name, pattern = "exchange", negate = TRUE))
  
  d %>%
    # bind_rows(g) %>%
    {if(!long) tidyr::pivot_wider(data = ., names_to = "measure") else .}
}

