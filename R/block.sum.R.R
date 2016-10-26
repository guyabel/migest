block.sum <- function(block = NULL, m = NULL, bid = NULL){
  if(any(dim(m)!=dim(bid)))
    stop("dimensions of m and bid must be equal")
  b <- m[bid == block]
  sum(b)
}

# m <- matrix(100:220, nrow = 11, ncol = 11)
# b <- block.matrix(1:16, c(2,3,4,2))
# block.sum(1, m, b)
# block.sum(4, m, b)
# block.sum(16, m, b)

