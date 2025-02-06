#' Summary of bilateral flows, counter-flow and net migration flow
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig}, \code{dest} and \code{flow}.
#' @param label Character string for the prefix of the calculated columns. Can take values \code{flow} or \code{stream}
#' @param orig Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})

#' @return A \code{tibble} with columns for orig, destination, corridor, flow, counter-flow and net flow in each bilateral pair.
#' @export
#'
#' @examples
#' # using matrix
#' r <- LETTERS[1:4]
#' m <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0),
#'             nrow = 4, ncol = 4, dimnames = list(orig = r, dest = r), byrow = TRUE)
#' m
#' sum_bilat(m)
#'
#' # using data frame
#' library(dplyr)
#' library(tidyr)
#' d <- expand_grid(orig = r, dest = r, sex = c("female", "male")) %>%
#'   mutate(flow = sample(x = 1:100, size = 32))
#' d
#'
#' # orig-dest summary of sex-specific flows
#' d %>%
#'   group_by(sex) %>%
#'   sum_bilat()
#'
#' # use group_by to distinguish orig-dest tables
#' d %>%
#'   group_by(sex) %>%
#'   sum_bilat()
sum_bilat <- function(m, label = "flow",
                      orig = "orig", dest = "dest", flow = "flow"){
  # orig = "orig"; dest = "dest"; flow = "flow"
  if(!is.character(orig)){
    orig <- as.name(substitute(orig))
  }
  if(!is.character(dest)){
    dest <- as.name(substitute(dest))
  }
  if(!is.character(flow)){
    flow <- as.name(substitute(flow))
  }

  if(!label %in% c("flow", "stream"))
    stop("label must be set to stream or flow")
  corridor <- pair <- counter_flow <- interchange <-  net_flow <- NULL
  if(!is.matrix(m)){
    d <- m %>%
      dplyr::rename(orig := !!orig,
                    dest := !!dest,
                    flow := !!flow)
    g <- dplyr::group_vars(d)
    if(length(g) == 0)
      g <- NULL
  }
  if(is.matrix(m)){
    d <- as.data.frame.table(x = m, responseName = "flow", stringsAsFactors = FALSE) %>%
      dplyr::rename(orig = 1,
                    dest = 2)
    g <- NULL
  }
  d <- d %>%
    dplyr::as_tibble() %>%
    dplyr::filter(orig != dest) %>%
    dplyr::mutate(corridor = paste(orig, dest, sep = " -> ")) %>%
    dplyr::mutate(orig = as.character(orig),
                  dest = as.character(dest)) %>%
    dplyr::mutate(pair = ifelse(orig < dest, paste(orig, dest, sep = " - "), paste(dest, orig, sep = " - "))) %>%
    dplyr::group_by_at(c({{g}}, "pair")) %>%
    dplyr::mutate(counter_flow = rev(flow),
                  net_flow = flow - counter_flow,
                  interchange = flow + counter_flow) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at({{g}}) %>%
    dplyr::relocate(orig, dest, corridor, pair)
  if(label == "steam"){
    d <- d %>%
      stats::setNames(stringr::str_replace(names(.),
                                           pattern = "flow",
                                           replacement = "stream"))
  }
  return(d)
}

