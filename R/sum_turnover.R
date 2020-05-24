#' Extract Total In- and Out-Migration Totals from an Origin-Destination Migration Flow Matrix.
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame the first and second columns correspond to origin and destination respecivly. The third column contains the number of migraitons or migrants.
#' @param drop_diagonal Logical to indicate dropping of diagonal terms, where the origin and destination are the same, in the calcuation of totals. Default \code{TRUE}.
#' @param include_net Logical to indicate inclusion of a net migration total column for each region, in addition to the total in- and out-flows. Default \code{TRUE}. 
#'
#' @return A \code{tibble} with total in- and out-flows for each region. 
#' @export
#'
#' @examples
sum_turnover <- function(m, drop_diagonal = TRUE, include_net = TRUE){
  if(!is.matrix(m)){
    d <- m %>%
      dplyr::rename(orig = 1, 
                    dest = 2, 
                    migration = 3)
  }
  if(is.matrix(m)){
    d <- as.data.frame.table(x = m, responseName = "migration", stringsAsFactors = FALSE) %>%
      dplyr::rename(orig = 1, 
                    dest = 2)
  }
  if(drop_diagonal)
    d <- d %>%
      mutate(migration = ifelse(orig == dest, 0, migration))
  d <- d %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(orig) %>%
    dplyr::mutate(tot_out = sum(migration)) %>%
    dplyr::group_by(dest) %>%
    dplyr::mutate(tot_in = sum(migration)) %>%
    dplyr::filter(orig == dest) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = orig) %>%
    dplyr::select(region, tot_in, tot_out)
  if(include_net){
    d <- d %>%
      dplyr::mutate(tot_net = tot_in - tot_out)
  }
  return(d)
}
