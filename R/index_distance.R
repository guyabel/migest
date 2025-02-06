#' Summary indices of migration distance
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig}, \code{dest} and \code{flow}.
#' @param d A \code{matrix} or data frame of origin-destination distances. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig}, \code{dest} and \code{dist}. Region names should match those in `m`.
#' @param orig Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dist Character string of the distance column name (when \code{dist} is a data frame rather than a \code{matrix})
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 3 summary measures where
#'   \item{mean}{Mean migration distance from Bell et. al. (2002) - not discussed in text but given in Table 6}
#'   \item{median}{Mean migration distance from Bell et. al. (2002)}
#'   \item{decay}{Distance decay parameter obtained from a Poisson regression model (`flow ~ orig + dest + log(dist)`)}
#'
#' @source Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247
#' @md
#' @export
#'
#' @examples
#' # single year
#' index_distance(
#'   m = subset(korea_gravity, year == 2020),
#'   d = subset(korea_gravity, year == 2020),
#'   dist = "dist_cent"
#' )
#'
#' # multiple years
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' korea_gravity %>%
#'   select(year, orig, dest, flow, dist_cent) %>%
#'   group_nest(year) %>%
#'   mutate(i = map2(
#'     .x = data, .y = data,
#'     .f = ~index_distance(m = .x, d = .y, dist = "dist_cent", long = FALSE)
#'   )) %>%
#'   select(-data) %>%
#'   unnest(i)
index_distance <- function(m = NULL,
                           d = NULL,
                           orig = "orig", dest = "dest",
                           flow = "flow",
                           dist = "dist",
                           long = TRUE
                           ){
  # orig = "orig"; dest = "dest"; flow = "flow"; pop = "pop"; dist = "dist"
  if(!is.character(orig)){
    orig <- as.name(substitute(orig))
  }
  if(!is.character(dest)){
    dest <- as.name(substitute(dest))
  }
  if(!is.character(flow)){
    flow <- as.name(substitute(flow))
  }
  dist <- NULL

  f0 <- mig_tibble(
    m = m, orig = orig, dest = dest, flow = flow
  )
  d0 <- f0$d %>%
    dplyr::select(orig, dest, flow)
  g0 <- f0$g

  m_reg <- as.character(unique(c(d0$orig, d0$dest)))
  # dd <- mig_tibble(m = korea_dist, flow = "dist")$d
  f1 <- d %>%
    dplyr::rename(dist := !!dist) %>%
    mig_tibble(orig = orig, dest = dest)
  d1 <- f1$d %>%
    dplyr::select(orig, dest, dist)
  g1 <- f1$g

  d_reg <- as.character(unique(c(d1$orig, d1$dest)))
  if(!setequal(m_reg, d_reg))
    stop("different region names in m and d")
  # glm(formula = round(flow) ~ orig + dest + log(dist), data = x, family = "poisson")$coefficients["log(dist)"]

  d0 %>%
    dplyr::filter(orig != dest) %>%
    dplyr::left_join(d1, by = c("orig", "dest")) %>%
    dplyr::summarise(
      mean = matrixStats::weightedMean(x = dist, w = flow),
      median = matrixStats::weightedMedian(x = dist, w = flow),
      decay = stats::glm(formula = round(flow) ~ orig + dest + log(dist), data = ., family = "poisson")$coefficients["log(dist)"]) %>%
    {if(long) tidyr::pivot_longer(data = ., cols = 1:ncol(.), names_to = "measure") else .}
}
