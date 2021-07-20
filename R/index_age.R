#' Summary indices of migration age profile
#'
#' Summary measures of migration age profiles as proposed by Rogers (1975), Bell et. al. (2002), Bell and Muhidin (2009) and Bernard, Bell and Charles-Edwards (2014)
#' @param d Data frame of age specific migration intensities. If used, ensure the correct column names are passed to \code{age_col} and \code{mi_col}.
#' @param age Numeric vector of ages. Used if `d = NULL`.
#' @param mi Numeric vector of migration intensities corresponding to each value of `age`. Used if `d = NULL`.
#' @param age_min Numeric value for minimum age for peak calculations. Taken as 5 by default.
#' @param age_max Numeric value for maximum age for peak calculations. Taken as 65 by default.
#' @param breadth Numeric value for number of age groups around peak to be used in breadth_peak measure. Defualt of `5`.
#' @param age_col Character string of the age column name (when \code{d} is provided)
#' @param mi_col Character string of the migration intensities column name (when \code{d} is provided)
#' @param long Logical to return a long data frame with index values all in one column
#'
#' @return A tibble with 8 summary measures where
#'   \item{gmr}{Gross migraproduction rate of Rogers (1975)}
#'   \item{peak_mi}{Peak migration intensities, from Bell et. al. (2002)}
#'   \item{peak_age}{Corresponding age of `peak_mi`, from Bell et. al. (2002)}
#'   \item{peak_breadth}{Breadth of peak, from Bell and Muhidin (2009)}
#'   \item{peak_share}{Percentage share of peak breadth of all migration, from Bell and Muhidin (2009)}
#'   \item{murc}{Maximum upward rate of change of Bernard, Bell and Charles-Edwards (2014)}
#'   \item{mdrc}{Maximum downward rate of change of Bernard, Bell and Charles-Edwards (2014)}
#'   \item{asymmetry}{Asymmetry between the `murc` and `mudc`, from Bernard, Bell and Charles-Edwards (2014)}
#'   
#' @source 
#' Rogers, A. (1975). Introduction to Multiregional Mathematical Demography. Wiley.
#' Bell, M., Blake, M., Boyle, P., Duke-Williams, O., Rees, P. H., Stillwell, J., & Hugo, G. J. (2002). Cross-national comparison of internal migration: issues and measures. Journal of the Royal Statistical Society: Series A (Statistics in Society), 165(3), 435–464. https://doi.org/10.1111/1467-985X.00247
#' Bell, M., & Muhidin, S. (2009). Cross-National Comparisons of Internal Migration (Research Paper 2009/30; Human Development Reports).
#' Bernard, A., Bell, M., & Charles-Edwards, E. (2014). Improved measures for the cross-national comparison of age profiles of internal migration. Population Studies, 68(2), 179–195. https://doi.org/10.1080/00324728.2014.890243
#' @export
#'
#' @examples
#' library(dplyr)
#' ipumsi_age %>%
#'   filter(sample == "BRA2000") %>%
#'   mutate(mi = migrants/population) %>%
#'   index_age()
#'   
#' ipumsi_age %>%
#'   group_by(sample) %>%
#'   mutate(mi = migrants/population) %>%
#'   index_age(long = FALSE)
index_age <- function(d = NULL, age, mi, age_min = 5, age_max = 65, breadth = 5,
                      age_col = "age", mi_col = "mi", long = TRUE){
  # d <- intensity_at_peak <- age_at_peak <- breadth_at_peak <- murc <- mdrc <- NULL
  if(is.null(d)){
    d <- tibble::tibble(
      age = age,
      mi = mi,
    ) 
  }
  a <- m <- nmi <-age_study <- peak_age <- max_d <- min_d <- murc <- mdrc <- NULL
  d %>%
    dplyr::rename(a = age_col,
                  m = mi_col) %>%
    dplyr::mutate(m = 100*m,
                  nmi = m/sum(m),
                  d = c(0, diff(nmi)),
                  age_study = a > age_min & a < age_max) %>%
    tidyr::replace_na(list(d = 0)) %>%
    dplyr::summarise(
      gmr = sum(m)/100,
      # peak_nmi = nmi[nmi == max(nmi)],
      peak_mi = max(m[age_study]),
      peak_age = a[nmi == max(nmi)],
      peak_breadth = sum(m[(peak_age - breadth):(peak_age + breadth)]),
      peak_share = 100 * sum(nmi[(peak_age - breadth):(peak_age + breadth)]),
      max_d = max(d[age_study], na.rm = TRUE),
      min_d = min(d[age_study], na.rm = TRUE),
      murc = a[d == max_d],
      mdrc = a[d == min_d],
      asymmetry = murc/mdrc) %>%
    dplyr::select(-max_d, -min_d) %>%
    {if(long) tidyr::pivot_longer(data = ., cols = "gmr":"asymmetry", names_to = "measure") else .}
}


