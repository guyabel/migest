#' Sum over a selected block in a block matrix
#'
#' Returns of a sum of a block within a \code{matrix}. This function is predominantly intended to be used within the \code{\link{ipf2_block}} routine.
#'
#' @param block Numeric value of block to summed. To be matched against the matrix in \code{block_id}.
#' @param m Matrix of all blocks combined.
#' @param block_id Matrix of the same dimensions of \code{m} used to identify blocks.
#'
#' @return Returns a numeric value of the sum of a single block.
#' @author Guy J. Abel
#' @noRd
#' @seealso \code{\link{block_matrix}}, \code{\link{stripe_matrix}}, \code{\link{ipf2_block}}
#'
#' @export
#'
#' @examples
#' m <- matrix(data = 100:220, nrow = 11, ncol = 11)
#' b <- block_matrix(x = 1:16, b = c(2, 3, 4, 2))
#' block_sum(block = 1, m = m, block_id = b)
#' block_sum(block = 4, m = m, block_id = b)
#' block_sum(block = 16, m = m, block_id = b)
block_sum <- function(block = NULL, m = NULL, block_id = NULL){
  if(any(dim(m)!=dim(block_id)))
    stop("dimensions of m and block_id must be equal")
  b <- m[block_id == block]
  sum(b)
}

