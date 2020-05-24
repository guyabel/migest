#' Estimate Net Migration from Lifetime Migration Data
#'
#' @param .data A data frame with two rows with the total number of lifetime in- and out-migrants in seperate columns. The first row contains totals at the first time point and second row at the second time point. 
#' @param in_migrant Chracater string name of column containin in-migrant counts. Default `code{"in_migrants"}`.
#' @param out_migrant Chracater string name of column containin out-migrant counts. Default `code{"out_migrants"}`.
#' @param year Chracater string name of column containin time points. Default `code{"year"}`.
#' @param in_survival Survival probablity for foreign migrants in region
#' @param out_survival Survival probablity for native migrants outside of region
#'
#' @return
#' @export
#'
#' @examples
net_from_lifetime <- function(.data, in_migrant = "in_migrants", out_migrant = "out_migrants", year = "year",
                              in_survival, out_survival){
  .data %>%
    dplyr::rename(in_migrants = in_migrant,
                   out_migrants = out_migrant,
                   year = year) %>%
    dplyr::mutate(period = paste(year, collapse = "-"),
                  net_foreign = in_migrants[2] - in_survival * in_migrants[1],
                  net_native = out_migrants[1] * out_survival - out_migrants[2], 
                  net = net_foreign + net_native) %>%
    dplyr::select(period:net) %>%
    dplyr::distinct() %>%
    dplyr::as_tibble()
}
