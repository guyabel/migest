#' Helper function to format migration input
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig}, \code{dest} and \code{flow}.
#' @param array Logical on return of array of all dimensions or origin-destination matrix (summed over all other dimensions)
#' @param orig Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#'
#' @return Formatted matrix
#'
mig_matrix <- function(m, array = TRUE, orig = "orig", dest = "dest", flow = "flow"){
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
    m <- m %>%
      dplyr::as_tibble() %>%
      dplyr::rename(orig := !!orig,
                    dest := !!dest,
                    flow := !!flow) %>%
      dplyr::relocate(flow, orig, dest)
    u <- apply(X = m, MARGIN = 2, FUN = dplyr::n_distinct)
    ii <- which(u == 1)
    m <- dplyr::select(m, -dplyr::all_of(names(ii)))
    if(!array){
      m <- m %>%
        stats::xtabs(formula = flow ~ orig + dest, data = .)
    }
    if(array){
      m <- m %>%
        stats::xtabs(formula = flow ~ ., data = .)
    }
  }
  m
}
