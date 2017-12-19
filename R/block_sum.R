#' Sum of Selected Block in a Block Matrix
#'
#' Returns of a sum of a block within a \code{matrix}. This function is predominantly intended to be used within the \code{\link{ipf2.b}} routine.
#'
#' @param block Numeric value of block to summed. To be matched against the matrix in \code{block_id}.
#' @param m Matrix of all blocks combined.
#' @param block_id Matrix of the same dimensions of \code{m} used to identify blocks.
#'
#' @return Returns a numeric value of the sum of a single block.
#' @author Guy J. Abel
#' @seealso \code{\link{block_matrix}}, \code{\link{stripe_matrix}}, \code{\link{ipf2_block}}
#' 
#' @export
#'
#' @examples
#' m <- matrix(100:220, nrow = 11, ncol = 11)
#' b <- block_matrix(1:16, c(2,3,4,2))
#' block_sum(1, m, b)
#' block_sum(4, m, b)
#' block_sum(16, m, b)
block_sum <- function(block = NULL, m = NULL, block_id = NULL){
  if(any(dim(m)!=dim(bid)))
    stop("dimensions of m and bid must be equal")
  b <- m[block_id == block]
  sum(b)
}


