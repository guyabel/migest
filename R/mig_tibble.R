#' Helper function to format migration input
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#'
#' @return Formatted tibble
#' 
mig_tibble <- function(m, orig_col = "orig", dest_col = "dest", flow_col = "flow"){
  orig <- dest <- flow <- NULL
  if(!is.matrix(m)){
    d <- m %>%
      dplyr::as_tibble() %>%
      dplyr::rename(orig := !!orig_col,
                    dest := !!dest_col,
                    flow := !!flow_col)
    g <- dplyr::group_vars(m)
    if(length(g) != 0)
      d <- dplyr::group_by_at(d, g)
    if(length(g) == 0)
      g <- NULL
  }
  if(is.matrix(m)){
    d <- as.data.frame.table(x = m, responseName = flow_col, stringsAsFactors = FALSE) %>%
      dplyr::rename(orig := 1,
                    dest := 2) %>%
      dplyr::as_tibble()
    g <- NULL
  }
  list(d = d, g = g)
}
