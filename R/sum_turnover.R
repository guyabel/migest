#' Extract total in-, out- and net-migration totals from an origin-destination migration flow matrix or data frame.
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param drop_diagonal Logical to indicate dropping of diagonal terms, where the origin and destination are the same, in the calculation of totals. Default \code{TRUE}.
#' @param include_net Logical to indicate inclusion of a net migration total column for each region, in addition to the total in- and out-flows. Default \code{TRUE}. 
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param type Character string to indicate if flows are \code{internal} or \code{international} to indicate if to use \code{region}, \code{tot_in}, \code{tot_out} or \code{country}, \code{tot_imm} and \code{tot_emi} in output.
#'
#' @return A \code{tibble} with total in- and out-flows for each region. 
#' 
#' @export
#' @examples 
#' # matrix
#' dn <- LETTERS[1:4]
#' m <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
#'             nrow = 4, ncol = 4, dimnames = list(orig = dn, dest = dn), byrow = TRUE)
#' sum_turnover(m)
#' 
#' \dontrun{
#' # data frame (tidy) format
#' library(tidyverse)
#' 
#' # download Abel and Cohen (2019) estimates
#' f <- read_csv("https://ndownloader.figshare.com/files/26239945")
#' 
#' # turnover for single period
#' f %>% 
#'   filter(year0 == 1990) %>%
#'   sum_turnover(flow_col = "da_pb_closed", type = "international")
#' 
#' # turnover for all periods using group_by
#' f %>% 
#'   group_by(year0) %>%
#'   sum_turnover(flow_col = "da_pb_closed", type = "international")
#' }   
sum_turnover <- function(
  m, drop_diagonal = TRUE, include_net = TRUE,
  orig_col = "orig", dest_col = "dest", flow_col = "flow",
  type = "internal"){
  # m = d0; drop_diagonal = TRUE; include_net = TRUE
  # m <- xtabs(formula = da_pb_closed ~ orig + dest, data = d0, subset = year0 == 1990)
  # orig_col = "orig"; dest_col = "dest"; flow_col = "da_pb_closed"
  # flow_col = "flow"
  if(!type %in% c("internal", "international"))
    stop("type must be set to internal or international")
  if(!is.matrix(m)){
    d <- m %>%
      dplyr::rename(orig := !!orig_col,
                    dest := !!dest_col,
                    flow := !!flow_col)
    g <- dplyr::group_vars(d)
    if(length(g) == 0) 
      g <- NULL
  }
  if(is.matrix(m)){
    d <- as.data.frame.table(x = m, responseName = "flow", stringsAsFactors = FALSE) %>%
      dplyr::rename(orig := 1,
                    dest := 2) %>%
      dplyr::as_tibble()
    g <- NULL
  }
  if(drop_diagonal)
    d <- d %>%
      dplyr::mutate(flow = ifelse(orig == dest, 0, flow))
  
  d <- d %>%
    dplyr::as_tibble() %>%
    dplyr::group_by_at(c({{g}}, "orig")) %>%
    dplyr::mutate(tot_out = sum(flow)) %>%
    dplyr::group_by_at(c({{g}}, "dest")) %>%
    dplyr::mutate(tot_in = sum(flow)) %>%
    dplyr::filter(orig == dest) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at({{g}}) %>%
    dplyr::mutate(region = orig) %>%
    dplyr::select(region, tot_in, tot_out)
  if(include_net){
    d <- d %>%
      dplyr::mutate(tot_net = tot_in - tot_out)
  }
  if(type == "international"){
    d <- d %>%
      dplyr::rename(country = region, 
                    tot_imm = tot_in,
                    tot_emi = tot_out)
  }
  return(d)
}