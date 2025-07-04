#' Unilateral summaries of in-, out-, turnover and net-migration totals from an origin-destination migration flow matrix or data frame.
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig}, \code{dest} and \code{flow}.
#' @param drop_diagonal Logical to indicate dropping of diagonal terms, where the origin and destination are the same, in the calculation of totals. Default \code{TRUE}.
#' @param orig Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param include_net Logical to indicate inclusion of a net migration total column for each region, in addition to the total in- and out-flows. Default \code{TRUE}.
#' @param international Logical to indicate if flows are international.
#' @param na_rm Logical to indicate if to remove NA values in \code{m} when calculating in and out migration flow totals. Default set to \code{TRUE}.
#'
#' @return A \code{tibble} with total in-, out- and turnover of flows for each region.
#'
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
#' \dontrun{
#' # data frame (tidy) format
#' library(tidyverse)
#'
#' # download Abel and Cohen (2019) estimates
#' f <- read_csv("https://ndownloader.figshare.com/files/38016762", show_col_types = FALSE)
#' f
#'
#' # single period
#' f %>%
#'   filter(year0 == 1990) %>%
#'   sum_country(flow = "da_pb_closed")
#'
#' # all periods using group_by
#' f %>%
#'   group_by(year0) %>%
#'   sum_country(flow = "da_pb_closed")
#' }
sum_region <- function(
  m, drop_diagonal = TRUE,
  orig = "orig", dest = "dest", flow = "flow",
  international = FALSE, include_net = TRUE,
  na_rm = TRUE){
  # m = d0; drop_diagonal = FALSE; include_net = TRUE; na_rm = TRUE; international = TRUE
  # m <- xtabs(formula = da_pb_closed ~ orig + dest, data = d0, subset = year0 == 1990)
  # orig = "orig"; dest = "dest"; flow = "da_pb_closed"
  # flow = "flow"
  if(!is.character(orig)){
    orig <- as.name(substitute(orig))
  }
  if(!is.character(dest)){
    dest <- as.name(substitute(dest))
  }
  if(!is.character(flow)){
    flow <- as.name(substitute(flow))
  }
  region <- tot_in_mig <- tot_out_mig <- NULL
  # fmt <- migest:::mig_tibble(
  fmt <- mig_tibble(
    m = m, orig = orig, dest = dest, flow = flow
  )
  d <- fmt$d
  g <- fmt$g
  # print(d)

  if(drop_diagonal)
    d <- d %>%
      dplyr::mutate(flow = ifelse(orig == dest, 0, flow))

  d1 <- d %>%
    dplyr::as_tibble() %>%
    dplyr::group_by_at(c({{g}}, "orig")) %>%
    dplyr::summarise(tot_out_mig = sum(flow, na.rm = na_rm), .groups = "keep") %>%
    dplyr::rename(region = orig) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at({{g}})

  d2 <- d %>%
    dplyr::group_by_at(c({{g}}, "dest")) %>%
    dplyr::summarise(tot_in_mig = sum(flow, na.rm = na_rm), .groups = "keep") %>%
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
  if(international == TRUE){
    d <- d %>%
      dplyr::rename(country = region,
                    tot_imm = tot_in_mig,
                    tot_emi = tot_out_mig)
  }
  d <- d %>%
    stats::setNames(stringr::str_remove(names(.), pattern = "tot_"))
  return(d)
}

#' Alias for sum_region() for international data
#' @rdname sum_region
#' @export
sum_country <- function(m, drop_diagonal = TRUE,
                        orig = "orig", dest = "dest", flow = "flow",
                       include_net = TRUE, international = TRUE, na_rm = TRUE){
  sum_region(m = m, drop_diagonal = drop_diagonal,
             orig = orig, dest = dest, flow = flow,
             include_net = include_net, international = international, na_rm = na_rm)
}

#' Alias for sum_region() with more general naming
#' @rdname sum_region
#' @export
sum_unilat <- function(m, drop_diagonal = TRUE,
                        orig = "orig", dest = "dest", flow = "flow",
                       include_net = TRUE, international = TRUE, na_rm = TRUE){
  sum_region(m = m, drop_diagonal = drop_diagonal,
             orig = orig, dest = dest, flow = flow,
             include_net = include_net, international = international, na_rm = na_rm)
}

#' Alias for sum_unilat() with more explicit naming
#' @rdname sum_region
#' @export
sum_unilateral <- function(m, drop_diagonal = TRUE,
                       orig = "orig", dest = "dest", flow = "flow",
                       include_net = TRUE, international = TRUE, na_rm = TRUE){
  sum_unilat(m = m, drop_diagonal = drop_diagonal,
             orig = orig, dest = dest, flow = flow,
             include_net = include_net, international = international, na_rm = na_rm)
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
