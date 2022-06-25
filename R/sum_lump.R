#' Sum and lump together small flows into a "other" category
#'
#' @description Lump together regions/countries if their flows are below a given threshold.
#'
#' @param m A \code{matrix} or data frame of origin-destination flows. For \code{matrix} the first and second dimensions correspond to origin and destination respectively. For a data frame ensure the correct column names are passed to \code{orig_col}, \code{dest_col} and \code{flow_col}.
#' @param threshold Numeric value used to determine small flows, origins or destinations that will be grouped (lumped) together. 
#' @param lump Character string to indicate where to apply the threshold. Choose from the \code{flow} values, \code{in} migration region and/or \code{out} migration region.
#' @param other_level Character string for the origin and/or destination label for the lumped values below the \code{threshold}. Default \code{"other"}.
#' @param complete Logical value to return a \code{tibble} with complete the origin-destination combinations
#' @param fill Numeric value for to fill small cells below the \code{threshold} when \code{complete = TRUE}. Default of zero.
#' @param return_matrix Logical to return a matrix. Default \code{FALSE}.
#' @param orig_col Character string of the origin column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param dest_col Character string of the destination column name (when \code{m} is a data frame rather than a \code{matrix})
#' @param flow_col Character string of the flow column name (when \code{m} is a data frame rather than a \code{matrix})
#'
#' @return A \code{tibble} with an additional \code{other} origins and/or destinations region based on the grouping together of small values below the \code{threshold} argument and the \code{lump} argument to indicate on where to apply the threshold. 
#' 
#' @details The \code{lump} argument can take values \code{flow} or \code{bilat} to apply the threshold to the data values for between region migration, \code{in} or \code{imm} to apply the threshold to the incoming region region and \code{out} or \code{emi} to apply the threshold to outgoing region region.
#' @export
#'
#' @examples
#' r <- LETTERS[1:4]
#' m <- matrix(data = c(0, 100, 30, 10, 50, 0, 50, 5, 10, 40, 0, 40, 20, 25, 20, 0),
#'             nrow = 4, ncol = 4, dimnames = list(orig = r, dest = r), byrow = TRUE)
#' m
#' 
#' # threshold on in and out region
#' sum_lump(m, threshold = 100, lump = c("in", "out"))
#' 
#' # threshold on flows (default)
#' sum_lump(m, threshold = 40)
#' 
#' # return a matrix (only possible when input is a matrix and
#' # complete = TRUE) with small values replaced by zeros
#' sum_lump(m, threshold = 50, complete = TRUE)
#' 
#' # return a data frame with small values replaced with zero
#' sum_lump(m, threshold = 80, complete = TRUE, return_matrix = FALSE)
#' 
#' \dontrun{
#' # data frame (tidy) format
#' library(tidyverse)
#' 
#' # download Abel and Cohen (2019) estimates
#' f <- read_csv("https://ndownloader.figshare.com/files/26239945", show_col_types = FALSE)
#' f
#' 
#' # large 1990-1995 flow estimates
#' f %>%
#'   filter(year0 == 1990) %>%
#'   sum_lump(flow_col = "da_pb_closed", threshold = 1e5)
#' 
#' # large flow estimates for each year
#' f %>%
#'   group_by(year0) %>%
#'   sum_lump(flow_col = "da_pb_closed", threshold = 1e5)
#' }
sum_lump <- function(m, threshold = 1, lump = "flow",
                     other_level = "other",
                     complete = FALSE, fill = 0, return_matrix = TRUE,
                     orig_col = "orig", dest_col = "dest", flow_col = "flow"){
  # m = m0filter(f, year0 == 1990) 
  # threshold = 1e3; lump = c("in", "out");
  # lump = "flow"
  # other_level = "other"; complete = FALSE; fill = 0
  # orig_col = "orig"; dest_col = "dest"; flow_col = "da_pb_closed"
  # flow_col = "flow"
  orig <- dest <- flow <- region <- in_mig <- out_mig <- NULL
  if(!all(lump %in% c("flow", "bilat", "in", "imm", "emi", "out", "turnover", "turn")))
    stop("lump is not recognised")
  
  #m 
  # dd <- migest:::mig_tibble(
  dd <- mig_tibble(
    m = m, orig_col = orig_col, dest_col = dest_col, flow_col = flow_col
  )
  d <- dd$d
  g <- dd$g

  imm_lump <- emi_lump <- flow_lump <- turn_imm_lump <- turn_emi_lump <- NULL
  turn <- orig_new <- dest_new <- NULL
  if(any(lump %in% c("in", "imm"))){
    imm_lump <- d %>%
      sum_region() %>%
      dplyr::filter(in_mig < threshold) %>%
      dplyr::select(dplyr::all_of(g), region) %>%
      dplyr::rename(dest = region) %>%
      dplyr::mutate(dest_new = other_level)
  }
  if(any(lump %in% c("out", "emi"))){
    emi_lump <- d %>%
      sum_region() %>%
      dplyr::filter(out_mig < threshold) %>%
      dplyr::select(dplyr::all_of(g), region) %>%
      dplyr::rename(orig = region) %>%
      dplyr::mutate(orig_new = other_level)
  }
  if(any(lump %in% c("turn", "turnover"))){
    turn_emi_lump <- d %>%
      sum_region() %>%
      dplyr::filter(turn < threshold) %>%
      dplyr::select(dplyr::all_of(g), region) %>%
      dplyr::rename(orig = region) %>%
      dplyr::mutate(orig_new = other_level)
    
    turn_imm_lump <- turn_emi_lump %>%
      dplyr::rename(dest = orig, 
                    dest_new = orig_new)
  }
  
  # not really tested these 
  x0 <- NULL
  if(length(imm_lump)!=0){
    x0 <- d %>%
      dplyr::left_join(imm_lump) %>%
      dplyr::mutate(dest_new = ifelse(is.na(dest_new), dest, dest_new)) %>%
      dplyr::mutate(dest = dest_new) %>%
      dplyr::select(-dest_new)
  }
  if(length(emi_lump)!=0){
    if(is.null(x0))
      x0 <- d
    x0 <- x0 %>%
      dplyr::left_join(emi_lump) %>%
      dplyr::mutate(orig_new = ifelse(is.na(orig_new), orig, orig_new)) %>%
      dplyr::mutate(orig = orig_new) %>%
      dplyr::select(-orig_new)
  }
  
  # not thoroughly tested these 
  if(length(turn_emi_lump)!=0){
    if(is.null(x0))
      x0 <- d
    x0 <- x0 %>%
      dplyr::left_join(turn_emi_lump) %>%
      dplyr::mutate(orig_new = ifelse(is.na(orig_new), orig, orig_new)) %>%
      dplyr::mutate(orig = orig_new) %>%
      dplyr::select(-orig_new) %>%
      dplyr::left_join(turn_imm_lump) %>%
      dplyr::mutate(dest_new = ifelse(is.na(dest_new), dest, dest_new)) %>%
      dplyr::mutate(dest = dest_new) %>%
      dplyr::select(-dest_new)
  }
  
  if(any(lump %in% c("flow", "bilat")))
    flow_lump <- TRUE
  if(is.null(x0))
    x0 <- d
  x1 <- x0 %>%
    if(is.null(flow_lump)) . else dplyr::mutate(., orig = ifelse(flow < threshold, other_level, orig)) %>%
    if(is.null(flow_lump)) . else dplyr::mutate(., dest = ifelse(flow < threshold, other_level, dest))

  x2 <- x1 %>%
    dplyr::group_by_at(c({{g}}, "orig", "dest")) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at({{g}}) %>%
    if(complete) tidyr::complete(., orig = c(unique(d$orig), "other"),
                                    dest = c(unique(d$dest), "other"),
                                    fill = list(flow = fill)) else .
  
  if(complete & is.matrix(m) & return_matrix){
    x2 <- stats::xtabs(formula = flow ~ orig + dest, data = x2)
  }
  return(x2)
}
