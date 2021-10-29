#' Calculate total in-, out-, turnover and net-migration totals from an origin-destination migration flow matrix or data frame.
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param drop_diagonal Logical to indicate dropping of diagonal terms, where the origin and destination are the same, in the calculation of totals. Default \code{TRUE}.
#' @param include_net Logical to indicate inclusion of a net migration total column for each region, in addition to the total in- and out-flows. Default \code{TRUE}. 
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param type Character string to indicate if flows are \code{internal} or \code{international} to indicate if to use \code{region}, \code{tot_in_mig}, \code{tot_out_mig} or \code{country}, \code{tot_imm} and \code{tot_emi} in output.
#' @param international Logical to indicate if flows are international.
#' @param name_tot Logical to prefix column names with `"tot_"` to match older versions. Default `FALSE`.
#'
#' @return A \code{tibble} with total in-, out- and turnover of flows for each region. 
#' 
#' @export
#' @examples 
#' # matrix
#' r <- LETTERS[1:4]
#' m <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
#'             nrow = 4, ncol = 4, dimnames = list(orig = r, dest = r), byrow = TRUE)
#' m
#' sum_region(m)
#'   
#' # different labels
#' sum_region(m, international = TRUE)
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
#'   sum_region(flow_col = "da_pb_closed", type = "international")
#' 
#' # turnover for all periods using group_by
#' f %>% 
#'   group_by(year0) %>%
#'   sum_region(flow_col = "da_pb_closed", type = "international")
#' }   
sum_region <- function(
  m, drop_diagonal = TRUE, include_net = TRUE, 
  orig_col = "orig", dest_col = "dest", flow_col = "flow",
  type = "internal", international = FALSE, name_tot = FALSE){
  # m = d0; drop_diagonal = FALSE; include_net = TRUE
  # m <- xtabs(formula = da_pb_closed ~ orig + dest, data = d0, subset = year0 == 1990)
  # orig_col = "orig"; dest_col = "dest"; flow_col = "da_pb_closed"
  # flow_col = "flow"
  orig <- dest <- flow <- region <- tot_in_mig <- tot_out_mig <- NULL
  if(!type %in% c("internal", "international"))
    stop("type must be set to internal or international")
  
  # fmt <- migest:::format_migration_tibble(
  fmt <- format_migration_tibble(
    m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col
  )
  d <- fmt$d
  g <- fmt$g
  
  if(drop_diagonal)
    d <- d %>%
      dplyr::mutate(flow = ifelse(orig == dest, 0, flow))
  
  d1 <- d %>%
    dplyr::as_tibble() %>%
    dplyr::group_by_at(c({{g}}, "orig")) %>%
    dplyr::summarise(tot_out_mig = sum(flow), .groups = "keep") %>%
    dplyr::rename(region = orig) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at({{g}})
    
  d2 <- d %>%
    dplyr::group_by_at(c({{g}}, "dest")) %>%
    dplyr::summarise(tot_in_mig = sum(flow), .groups = "keep") %>%
    dplyr::rename(region = dest) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at({{g}}) 
  
  d <- d1 %>%
    dplyr::full_join(d2, by = c(g, "region")) %>%
    tidyr::replace_na(list(tot_out_mig = 0, tot_in_mig = 0)) %>%
    dplyr::mutate(tot_turn = tot_in_mig + tot_out_mig)
  
  if(include_net){
    d <- d %>%
      dplyr::mutate(tot_net = tot_in_mig - tot_out_mig)
  }
  if(type == "international" | international == TRUE){
    d <- d %>%
      dplyr::rename(country = region, 
                    tot_imm = tot_in_mig,
                    tot_emi = tot_out_mig)
  }
  if(!name_tot){
    d <- d %>%
      stats::setNames(stringr::str_remove(names(.), pattern = "tot_"))
  }
  return(d)
}
# library(tidyverse)
# library(migest)
# d0 <- expand_grid(
#   orig = LETTERS[1:4], 
#   dest = LETTERS[2:4]) %>%
#   mutate(flow = 1:12)
# 
# 
# d0 <- expand_grid(
#   orig = LETTERS[1:4], 
#   dest = LETTERS[1:4]) %>%
#   mutate(flow = 1:16)
# sum_region(d0)
# 
# m0 <- xtabs(formula = flow ~ orig + dest, data = d0)
# addmargins(m0)
# sum_region(d0, drop_diagonal = FALSE)
