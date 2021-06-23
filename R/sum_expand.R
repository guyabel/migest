#' Expand flow data to include aggregate sums for origin and destination meta-regions
#' 
#' @description Expand matrix of data frame of migration data to include aggregate sums for corresponding origin and destination meta regions.
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param return_matrix Logical to return a matrix. Default `TRUE`
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param orig_area_col Character string of the origin area column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_area_col Character string of the destination area column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param orig_area Vector of labels for the origin areas of each row of \code{m}.
#' @param dest_area Vector of labels for the destination areas of each row of \code{m}.
#'
#' @return A \code{tibble} or \code{matrix} with additional row and columns (for matrices) for aggregate sums for origin and destination meta-regions
#' @export
#'
#' @examples
#' ##
#' ## from data frame
#' ##
#' library(tidyverse)
#' d <- block_matrix(x = 1:25, b = c(2,3,4,2,1)) %>%
#'   as.data.frame.table(responseName = "flow") %>%
#'   as_tibble() %>%
#'   rename(orig = 1, 
#'          dest = 2) %>%
#'   mutate(orig_area = str_sub(string = orig, end = 1),
#'          dest_area = str_sub(string = dest, end = 1))
#' d
#' 
#' sum_expand(d)
#' 
#' # return a matrix
#' sum_expand(d, return_matrix = FALSE)
#' 
#' ##
#' ## from matrix
#' ##
#' m <- block_matrix(x = 1:16, b = c(2,3,4,2))
#' m
#' # requires a vector of origin and destination areas
#' a <- rep(LETTERS[1:4], times = c(2,3,4,2))
#' a
#' sum_expand(m = m, orig_area = a, dest_area = a)
sum_expand <- function(m, return_matrix = TRUE,
                       orig_col = "orig", dest_col = "dest", flow_col = "flow",
                       orig_area_col = "orig_area", dest_area_col = "dest_area", 
                       orig_area = NULL, dest_area = NULL){
  if(!is.matrix(m)){
    d <- m %>%
      dplyr::rename(orig := !!orig_col,
                    dest := !!dest_col,
                    flow := !!flow_col,
                    orig_area := !!orig_area_col, 
                    dest_area := !!dest_area_col)
    g <- dplyr::group_vars(d)
    if(length(g) == 0) 
      g <- NULL
  }
  if(is.matrix(m)){
    if(length(orig_area) != nrow(m))
      stop("length of orig_area needs to match the number of rows in m")
    if(length(dest_area) != ncol(m))
      stop("length of dest_area needs to match the number of columns in m")
    d <- as.data.frame.table(x = m, responseName = "flow", 
                             stringsAsFactors = FALSE) %>%
      dplyr::rename(orig := 1,
                    dest := 2) %>%
      dplyr::as_tibble() %>%
      dplyr::bind_cols(expand_grid(dest_area = dest_area, orig_area = orig_area)) %>%
      dplyr::relocate(-dest_area)
    g <- NULL
  }
  x0 <- d %>%
    tidyr::pivot_longer(cols = c(orig, orig_area), 
                 names_to = "orig_type", values_to = "orig") %>%
    tidyr::pivot_longer(cols = c(dest, dest_area), 
                 names_to = "dest_type", values_to = "dest") %>%
    dplyr::group_by_at(c({{g}}, "orig", "dest")) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop") %>%
    dplyr::ungroup()
    
  if(return_matrix){
    x0 <- stats::xtabs(formula = flow ~ orig + dest, data = x0)
  }
  return(x0)
}

