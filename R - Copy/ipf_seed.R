#' Quickly create IPF seed
#'
#' This function is predominantly intended to be used within the ipf routines in the migest package.
#'
#' @param m Matrix, Array or NULL to build seed. If NULL seed will be 1 for all elements.
#' @param R Number of rows, columns and possibly n_dimensions for seed matrix or array.
#' @param n_dim Numeric integer for the number of n_dimensions - 2 for matrix, 3 or more for an array
#' @param dn Vector of character strings for n_dimension names
#'
#' @return An \code{array} or \code{matrix}
#' @author Guy J. Abel
#' 
ipf_seed <- function(m = NULL, R = NULL, n_dim = NULL, dn = NULL){
  if (is.null(m) & is.null(R))
    stop("Must provide either m or R")
  if (!is.null(m) & is.null(R))
    R <- unique(dim(m))
  if (is.null(m) & is.null(n_dim))
    stop("Must provide either m or n_dim")
  if (!is.null(R) & !is.null(dn) & length(dn) != R)
    stop("Length of dn must match R")
  if (is.null(m))
    m <- 1
  if (!is.null(n_dim))
    m <- array(data = m, dim = rep(R, n_dim))
  if (is.null(dimnames(m)) & !is.null(dn)) {
    for(i in 1:n_dim){
      dimnames(m)[i] <- list(dn)
    }
    names(dimnames(m)) <- c("orig", "dest", "pob")[1:n_dim]
  }
  return(m)
}
