#' Origin or Destination Sums for Origin-Destination Tidy Data
#'
#' Sums of inflows and out flows from tidy origin-destination data.
#' 
#' @param d data frame of origin-destination data. Must contain columns with names orig, dest and flow.
#' @param type string (either orig or dest) indicating if totals should be for origins (outflows) or destinations (inflows)
#'
#' @return \code{tibble} data frame 
#' @author Guy J. Abel
#' @export
#'
sum_tidy_total <- function(d = NULL, type = "orig"){
  if(type == "orig"){
    dd <- d %>%
      dplyr::select(orig, flow) %>%
      group_by(orig) %>%
      summarise(flow_out = sum(flow)) %>%
      rename(name = orig) %>%
      ungroup()
  }
  if(type == "dest"){
    dd <- d %>%
      select(dest, flow) %>%
      group_by(dest) %>%
      summarise(flow_in = sum(flow)) %>%
      rename(name = dest) %>%
      ungroup()
  }
  return(dd)
}
