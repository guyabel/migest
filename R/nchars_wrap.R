#' Count the number of characters per line
#'
#' @param b Numeric vector for the position of line breaks between the words in \code{w}
#' @param w Character string vector of words
#'
#' @return List with vectors for number of characters per line and the number of words per line
nchars_wrap <- function(b, w){
  n <- y <- NULL
  for(i in 0:length(b) + 1){
    s <- ifelse(i == 1, 1, b[i-1]+1)
    f <- ifelse(i == (length(b) + 1), length(w), b[i])
    y <- c(y, length(s:f))
    n <- c(n, sum(nchar(w[s:f])) +  length(s:f) - 1)
  }
  return(list(n = n, words = y))
}
