block.matrix <- function(x = NULL, b = NULL, byrow = FALSE, dimnames = NULL){
  n <- length(b)
  bb <- rep(1:4, times = b)
  dn <- NULL
  if(!is.null(dimnames)){
    dn <- rep(LETTERS[1:4], times = b)
    dd <- unlist(sapply(b, seq, from = 1))
    dn <- paste0(dn, dd)
    dn <- list(dn, dn)
  }
  xx <- matrix(NA, nrow = sum(b), ncol = sum(b), dimnames = dn)
  k <- 1
  if(byrow == TRUE){
    for(i in 1:n){
      for(j in 1:n){
        xx[i==bb, j==bb] <- x[k]
        k <- k+1
      }
    }
  }
  if(byrow == FALSE){
    for(j in 1:n){
      for(i in 1:n){
        xx[i==bb, j==bb] <- x[k]
        k <- k+1
      }
    }
  }
  return(xx)
}
# y <- block.matrix(x = 1:16, b = c(2,3,4,2), dimnames = LETTERS[1:4])
