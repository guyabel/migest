#' Sum bilateral data to include aggregate bilateral totals for origin and destination meta areas
#' 
#' @description Expand matrix of data frame of migration data to include aggregate sums for corresponding origin and destination meta regions.
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param return_matrix Logical to return a matrix. Default \code{FALSE}.
#' @param guess_order Logical to return a matrix or data frame ordered by origin and destination with area names at the end of each block. Default \code{TRUE}. If \code{FALSE} returns matrix or data frame based on alphabetical order of origin and destinations.
#' @param area_first Order area sums to be placed before the origin and destination values. Default \code{TRUE}
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
#' ## from matrix
#' ##
#' m <- block_matrix(x = 1:16, b = c(2,3,4,2))
#' m
#' 
#' # requires a vector of origin and destination areas
#' a <- rep(LETTERS[1:4], times = c(2,3,4,2))
#' a
#' sum_expand(m = m, orig_area = a, dest_area = a)
#' 
#' # place area sums after regions
#' sum_expand(m = m, orig_area = a, dest_area = a, area_first = FALSE)
#'
#' ##
#' ## from large data frame
#' ##
#' \dontrun{
#' library(tidyverse)
#' library(countrycode)
#' 
#' # download Abel and Cohen (2019) estimates
#' f <- read_csv("https://ndownloader.figshare.com/files/26239945", show_col_types = FALSE)
#' f
#' 
#' # 1990-1995 flow estimates
#' f %>%
#'   filter(year0 == 1990) %>%
#'   mutate(
#'     orig_area = countrycode(sourcevar = orig, custom_dict = dict_ims,
#'                             origin = "iso3c", destination = "region"),
#'     dest_area = countrycode(sourcevar = dest, custom_dict = dict_ims,
#'                             origin = "iso3c", destination = "region")
#'   ) %>%
#'   sum_expand(flow_col = "da_pb_closed", return_matrix = FALSE)
#' 
#' # by group (period)
#' f %>%
#'   mutate(
#'     orig_area = countrycode(sourcevar = orig, custom_dict = dict_ims,
#'                             origin = "iso3c", destination = "region"),
#'     dest_area = countrycode(sourcevar = dest, custom_dict = dict_ims,
#'                             origin = "iso3c", destination = "region")
#'   ) %>%
#'   group_by(year0) %>%
#'   sum_expand(flow_col = "da_pb_closed", return_matrix = FALSE)
#' }
sum_expand <- function(m, return_matrix = FALSE, guess_order = TRUE, area_first = TRUE,
                       orig_col = "orig", dest_col = "dest", flow_col = "flow",
                       orig_area_col = "orig_area", dest_area_col = "dest_area", 
                       orig_area = NULL, dest_area = NULL){
  # orig_col = "orig"; dest_col = "dest"; flow_col = "da_pb_closed"
  # orig_area_col = "orig_area"; dest_area_col = "dest_area"
  # orig_area_col = "orig_reg"; dest_area_col = "dest_reg"
  # orig_area = a; dest_area = a
  # flow_col = "flow"
  # return_matrix = FALSE; guess_order = TRUE; area_first = TRUE
  orig <- dest <- flow <- region <- area <- value <- NULL
  if(!is.matrix(m)){
    dd <- mig_tibble(
      m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col
    )
    d <- dd$d
    g <- dd$g
    
    d <- d %>%
      dplyr::rename(orig_area := !!orig_area_col, 
                    dest_area := !!dest_area_col)
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
      dplyr::bind_cols(
        tidyr::expand_grid(dest_area = dest_area, 
                           orig_area = orig_area)
      ) %>%
      dplyr::relocate(-dest_area)
    g <- NULL
  }
  if(guess_order){
    c0 <- d %>%
      dplyr::ungroup() %>%
      dplyr::select(orig, orig_area) %>%
      dplyr::distinct() %>%
      dplyr::rename(region = 1,
                    area = 2)
    
    c1 <- d %>%
      dplyr::ungroup() %>%
      dplyr::select(dest, dest_area) %>%
      dplyr::distinct() %>%
      dplyr::rename(region = 1,
                    area = 2)
    
    c2 <- c0 %>%
      dplyr::full_join(c1, by = c("region", "area")) %>%
      dplyr::mutate(area = forcats::fct_inorder(area)) %>%
      dplyr::arrange(area)
    
    c3 <- c2 %>%
      dplyr::mutate(region = as.character(region), 
                    area = as.character(area)) %>%
      dplyr::group_by(area) %>%
      dplyr::summarise(
        value = ifelse(
          test = area_first, 
          yes = paste0(c(unique(area), (region)), collapse = "#"), 
          no =  paste0(c((region), unique(area)), collapse = "#")
        ), .groups = "drop") %>%
      dplyr::mutate(area = factor(area, levels = levels(c2$area))) %>%
      dplyr::arrange(area) %>%
      dplyr::pull(value) %>%
      paste(collapse = "#") %>%
      stringr::str_split(pattern = "#") %>%
      .[[1]]
  }
  x0 <- d %>%
    tidyr::pivot_longer(cols = c(orig, orig_area), 
                 names_to = "orig_type", values_to = "orig") %>%
    tidyr::pivot_longer(cols = c(dest, dest_area), 
                 names_to = "dest_type", values_to = "dest") %>%
    dplyr::mutate(orig = forcats::fct_inorder(orig), 
                  dest = forcats::fct_inorder(dest)) %>%
    dplyr::group_by_at(c({{g}}, "orig", "dest")) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at({{g}})
  
  if(guess_order){
    x0 <- x0 %>%
      dplyr::mutate(orig = factor(orig, levels = c3), 
                    dest = factor(dest, levels = c3)) %>%
      dplyr::arrange(orig, dest)
  }
    
  if(return_matrix){
    x0 <- stats::xtabs(formula = flow ~ orig + dest, data = x0)
    attr(x0, "class") <- NULL
    attr(x0, "call") <- NULL
  }
  
  if(guess_order & !return_matrix){
    x0 <- x0 %>%
      dplyr::mutate(orig = as.character(orig), 
                    dest = as.character(dest))
  }
  return(x0)
}