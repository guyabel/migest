#' Single line wrap for string
#'
#' @param string string from \code{str_wrap_n}
#' @param n n from from \code{str_wrap_n}
#'
#' @return String with line breaks
str_wrap_n_single <- function(string = NULL, n = 2){
  # words
  w <- stringr::str_split(string = string, pattern = " ")[[1]]

  if(n > length(w)){
    n <- length(w)
    message("Asking for more lines than words")
  }
  if(n > 8){
    stop("n is too large to look at every combination")
  }
  if(n == 1)
    return(string)
  i <- dd <- s <- nn <- var_n <- var_s <- value <- words <- NULL
  b <- utils::combn(x = 2:length(w)-1, m = n - 1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(i = 1:dplyr::n()) %>%
    tidyr::pivot_longer(cols = -i) %>%
    tidyr::nest(dd = c(name, value)) %>%
    dplyr::mutate(s = purrr::map(.x = dd, .f = ~dplyr::pull(.x, value)),
                  nn = purrr::map(.x = s, .f = ~nchars_wrap(b = .x, w = w))) %>%
    tidyr::unnest_wider(col = nn) %>%
    dplyr::mutate(var_n = purrr::map_dbl(.x = n, .f = ~stats::var(x = .x)),
                  var_s = purrr::map_dbl(.x = words, .f = ~stats::var(x = .x))) %>%
    dplyr::filter(var_n == min(var_n)) %>%
    dplyr::filter(var_s == min(var_s)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(s)

  f <- NULL
  for(i in 2:length(w)-1){
    bb <- " "
    if(i %in% b[[1]])
      bb <- "\n"
    f <- paste0(f, w[i], bb)
  }
  f <- paste(c(f, w[length(w)]), collapse = "")
  return(f)
}
