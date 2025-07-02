#' Solutions from the quadratic equation
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
#'
quadratic_eqn <- function(a, b, c) {
  disc <- b^2 - 4 * a * c
  if (!is.finite(disc) || !is.finite(a) || a == 0) {
    return(numeric(0))  # Return empty vector on invalid input
  }

  if (disc > 0) {
    x1 <- (-b + sqrt(disc)) / (2 * a)
    x2 <- (-b - sqrt(disc)) / (2 * a)
    return(c(x1, x2))
  } else if (disc == 0) {
    x <- -b / (2 * a)
    return(x)
  } else {
    return(numeric(0))  # No real roots
  }
}
