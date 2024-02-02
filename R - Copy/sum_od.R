#' Extract a classic origin-destination migration flow matrix.
#'
#' Extract a classic origin-destination migration flow matrix from a more detailed dis-aggregation of flows stored in an (\code{array}).
#' Primarily intended to work with output from \code{\link{ffs_demo}}.
#' @param x Array of origin-destination matrices, where the first and second dimensions correspond to origin and destination respectively. Higher dimension(s) refer to additional migrant characteristic(s).
#' @param zero_diag Logical to indicate if to set diagonal terms to zero. Default \code{TRUE}.
#' @param add_margins Logical to indicate if to add row and column for immigration and emigration totals. Default \code{TRUE}
#'
#' @return
#' Matrix from summing over the first and second dimension. Set diagonals to zero.
#' 
#' Returns a \code{matrix} object of origin-destination flows
#' @seealso \code{\link{ffs_demo}}
#' 
#' @export
sum_od <- function(x = NULL, zero_diag = TRUE, add_margins = TRUE){
  f <- apply(X = x, MARGIN = c(1,2), FUN = sum)
  if(zero_diag)
    diag(f) <- 0
  if(add_margins)
    f <- stats::addmargins(f)
  return(f)
}
