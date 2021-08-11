#' Wrap character string to fit a target number of lines
#'
#' Inserts line breaks for spaces, where the position of the line breaks are chosen to provide the most balanced length of each line.
#'
#' @param string Character string to be broken up
#' @param n Number of lines to break the string over
#'
#' @details Function is intended for a small number of line breaks. The \code{n} argument is not allowed to be greater than 8 as all combinations of possible line breaks are explored.
#'
#' When there a number of possible solutions that provide equally balanced number of characters in each line, the function returns the character string where the number of spaces are distributed most evenly.
#' @return The original \code{string} with line breaks inserted at optimal positions.
#' @export
#'
#' @examples
#' str_wrap_n(string = "a bb ccc dddd eeee ffffff", n = 2)
#' str_wrap_n(string = "a bb ccc dddd eeee ffffff", n = 4)
#' str_wrap_n(string = "a bb ccc dddd eeee ffffff", n = 8)
#' str_wrap_n(string = c("a bb", "a bb ccc"), n = 2)
str_wrap_n <- function(string = NULL, n = 2){
  if(length(string) == 1 & length(n) == 1)
    f <- str_wrap_n_single(string = string, n = n)
  if(length(string) > 1){
    s <- nn <- x <- NULL
    f <- tibble::tibble(
      s = string,
      nn = n) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(x = purrr::map2(
        .x = s, .y = nn,
        .f = ~str_wrap_n_single(string = s, n = n))) %>%
      dplyr::pull(x) %>%
      unlist()
  }
  return(f)
}
