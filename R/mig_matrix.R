#' Helper function to format migration input
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param array Logical on return of array of all dimensions or origin-destination matrix (summed over all other dimensions)
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#'
#' @return Formatted matrix
#' 
mig_matrix <- function(m, array = TRUE, orig_col = "orig", dest_col = "dest", flow_col = "flow"){
  orig <- dest <- flow <- NULL
  if(!is.matrix(m)){
    m <- m %>%
      dplyr::as_tibble() %>%
      dplyr::rename(orig := !!orig_col,
                    dest := !!dest_col,
                    flow := !!flow_col) %>%
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
