#' Solve Quadratic Equation
#' 
#' General function to solve classic quadratic equation:
#' \deqn{ a x^2 + b x + c = 0 }
#'
#' @param a Numeric value for quadratic term of x.
#' @param b Numeric value for multiplicative term of x.
#' @param c Numeric value for constant term.
#'
#' @return Vector of two values corresponding to the roots for the quadratic equation.
#' @author Guy J. Abel
#' @source Adapted from https://rpubs.com/kikihatzistavrou/80124
#' @export
#'
#' @examples
#' quadratic_eqn(a = 2, b = 4, c = -6)
quadratic_eqn <- function(a,b,c){
  delta <- function(a,b,c){
    b^2-4*a*c
  }
  if(delta(a,b,c) > 0){ 
    x1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    return(c(x1,x2))
  }
  else if(delta(a,b,c) == 0){ 
    x = -b/(2*a)
    return(x)
  }
  else {"There are no real roots."} 
}
