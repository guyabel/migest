#' Title
#'
#' @param a 
#' @param b 
#' @param c 
#'
#' @return
#' @export
#'
#' @examples
#' ntot <- c(30, 40, -5, -65)
#' quadratic.eqn(a = itot, b = -ntot, c = -etot)
quadratic_eqn <- function(a,b,c){
  delta <- function(a,b,c){
    b^2-4*a*c
  }
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    return(c(x_1,x_2))
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
    return(x)
  }
  else {"There are no real roots."} # third case D<0
}