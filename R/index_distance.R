#' Summary indices of migration distance 
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param dist A \code{matrix} or data frame of origin-destination distances. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{dist_col}. Region names should match those in `m`.
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dist_col Character string of the distance column name (when \code{dist} is a data frame rather than a \code{matrix})
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 3 summary measures where
#'   \item{mean}{Mean migration distance from Bell et. al. (2002) - not discussed in text but given in Table 6}
#'   \item{median}{Mean migration distance from Bell et. al. (2002)}
#'   \item{decay}{Distance decay parameter obtained from a Poisson regression model (`flow ~ orig + dest + log(dist)`)}
#'   
#' @source #' Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247
#' @export
#'
#' @examples
#' # single year
#' index_distance(
#'   m = subset(korea_reg, year == 2020), 
#'   dist = korea_dist
#' )
#' 
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' # multiple years
#' korea_reg %>%
#'   nest(m = c(orig, dest, flow)) %>%
#'   mutate(dist = list(korea_dist)) %>%
#'   mutate(i = map2(.x = m, .y = dist, 
#'                   .f = ~index_distance(m = .x, dist = .y, long = FALSE))) %>%
#'   select(-m, -dist) %>%
#'   unnest(i)
index_distance <- function(m = NULL, 
                           dist = NULL,
                           long = TRUE, 
                           orig_col = "orig", dest_col = "dest", 
                           flow_col = "flow",
                           dist_col = "dist"
                           ){
  # orig_col = "orig"; dest_col = "dest"; flow_col = "flow"; pop_col = "pop"; dist_col = "dist"
  orig <- dest <- flow <- NULL
  ff <- format_migration_tibble(
    m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col
  )
  d0 <- ff$d
  g <- ff$g
  
  m_reg <- as.character(unique(c(d0$orig, d0$dest)))
  # dd <- format_migration_tibble(m = korea_dist, flow_col = "dist")$d
  d1 <- format_migration_tibble(m = dist, flow_col = "dist")$d
  d_reg <- as.character(unique(c(d1$orig, d1$dest)))
  if(!setequal(m_reg, d_reg))
    stop("different region names in m and dist")
  # glm(formula = round(flow) ~ orig + dest + log(dist), data = x, family = "poisson")$coefficients["log(dist)"]

  d0 %>%
    dplyr::filter(orig != dest) %>%
    dplyr::left_join(d1, by = c("orig", "dest")) %>%
    dplyr::summarise(
      mean = matrixStats::weightedMean(x = dist, w = flow),
      median = matrixStats::weightedMedian(x = dist, w = flow),
      decay = stats::glm(formula = round(flow) ~ orig + dest + log(dist), data = ., family = "poisson")$coefficients["log(dist)"]) %>%
    {if(long) tidyr::pivot_longer(data = ., cols = "mean":"decay", names_to = "measure") else .}
}
