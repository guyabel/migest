#' Helper function to format migration input
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig}, \code{dest} and \code{flow}.
#' @param orig Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#'
#' @return Formatted tibble
#'
mig_tibble <- function(m, orig = "orig", dest = "dest", flow = "flow"){
  if(!is.character(orig)){
    orig <- as.name(substitute(orig))
  }
  if(!is.character(dest)){
    dest <- as.name(substitute(dest))
  }
  if(!is.character(flow)){
    flow <- as.name(substitute(flow))
  }
  # orig <- dest <- flow <- NULL
  if(!is.matrix(m)){
    d <- m %>%
      dplyr::as_tibble() %>%
      dplyr::rename(orig := !!orig,
                    dest := !!dest,
                    flow := !!flow)
    g <- dplyr::group_vars(m)
    if(length(g) != 0)
      d <- dplyr::group_by_at(d, g)
    if(length(g) == 0)
      g <- NULL
  }
  if(is.matrix(m)){
    d <- as.data.frame.table(x = m, responseName = flow, stringsAsFactors = FALSE) %>%
      dplyr::rename(orig := 1,
                    dest := 2) %>%
      dplyr::as_tibble()
    g <- NULL
  }
  list(d = d, g = g)
}
