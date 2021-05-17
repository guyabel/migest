#' Fundamental parameters for Rogers-Castro migration schedule
#'
#' Set of fundamental parameters for the Rogers-Castro migration age schedule, as suggested in Rogers and Castro (1981).
#'
#' @source Rogers, A., and L. J. Castro. (1981). Model Migration Schedules. \emph{IIASA Research Report 81} RR-81-30
#' @format A \code{list} of the parameters required by the \code{\link{rc9}} function:
#'  \deqn{ a_{1} = 0.02 }
#'  \deqn{ \alpha_{1} = 0.1 }
#'  \deqn{ a_{2} = 0.06 }
#'  \deqn{ \alpha_{2} = 0.1 }
#'  \deqn{ \mu_{2} = 20 }
#'  \deqn{ \lambda_{2} = 0.4 }
#'  \deqn{ c = 0.003 }
#' @examples
#' # check format
#' str(rc9.fund)
#' 
#' # single age groups
#' x <- 1:100
#' m <- rc9(x, param = rc9.fund)
#' plot(x, m, type="l")
#' 
#' # alter to see the effect of mu2
#' p1 <- rc9.fund
#' p1$mu2 <- 30
#' m1 <- rc9(x, param = p1)
#' plot(x, m, type="l")
#' lines(x, m1, lty=2)
"rc9.fund"