#' Title
#'
#' @param ntot 
#' @param m 
#' @param tol 
#' @param maxit 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
ipf.net <-
  function(ntot = NULL,
           m = NULL,
           tol = 1e-05,
           maxit = 500,
           verbose = TRUE) {
    R <- unique(c(dim(m), length(ntot)))
    if (length(R) != 1)
      stop("The m matrix must be square and with the same dimension as the net totals (ntot).")
    if (sum(ntot))
      message("Convergence will not be obtained as ntot does not sum to zero.")
    dn <- dimnames(m)[[1]]
    
    #set up offset
    if (is.null(m)) {
      m <- matrix(1, nrow = R, ncol = R)
    }
    if (is.null(dimnames(m))) {
      dimnames(m) <- list(orig = dn, dest = dn)
    }
    
    mu <- m
    it <- 0
    d_max <- tol * 2
    while (d_max > tol & it < maxit) {
      for(i in 1:R){
        p <- net.param(region = i, m = mu, ntot = ntot[i])
        mu <- net.scale(mu, region = i, alpha = p[p>0])
        mu
      }
      it <- it + 1
      d <- c(net.sum(mu) - ntot)
      d_max <- max(abs(d))
      if (verbose == TRUE)
        cat(c(it, d_max), "\n")
    }
    return(list(mu = mu, it = it, tol = d_max))
  }

y <- ipf.net(ntot = c(30, 40, -15, -55), m = m)
y$mu
net.sum(y$mu)