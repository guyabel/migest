#' Generate a Rogers-Castro nine-parameter migration schedule
#' 
#' Provides the Rogers-Castro schedule,
#' \deqn{ M(x) = a_{1} \exp[-\alpha_{1}x] + a_{2} \exp [ \alpha_{2}(x-\mu_{2})- \exp [ \lambda_{2}(x-\mu_{2}) ] ] +c}
#' for a given set of parameters and ages.
#' @param x Vector of numbers 
#' @param param List with names matching the parameters in the age schedule
#' @param scaled Scale estimates to sum to one across all ages, x.
#'
#' @return
#' Returns the M(x) values from the Rogers-Castro schedule of age specific migration rate. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The \code{param} argument must be a list with correct names for each parameter. See for example the \code{\link{rc9.fund}} object for an example of the naming convention.
#' @references 
#' Rogers, A., and L. J. Castro. (1981). Model Migration Schedules. \emph{IIASA Research Report 81} RR-81-30
#' @author Guy J. Abel
#' @seealso \code{\link{rc9.fund}}
#' @export
#'
#' @examples
#' # single age groups
#' x <- 1:100
#' m <- rc9(x, param = rc9.fund)
#' plot(x, m, type="l")
#' 
#' # 5 year age groups
#' m <- rc9(x, param = rc9.fund)
#' plot(x, m, type="l")
# rc9.fund<-list(a1=0.02, alpha1=0.1, a2=0.06, alpha2=0.1, mu2=20, lambda2=0.4, c=0.003)
rc9 <- function(x, param = NULL, scaled=TRUE){
  if(!is.list(param))
  stop("param must be a list")
  if(any(!(names(param) %in% c("a1","alpha1","a2","alpha2","mu2","lambda2","c"))))
    stop("param must be a list with correct names, see for example rc9.fund")

  m <- param[["a1"]] * exp(-param[["alpha1"]] * x) +
    param[["a2"]] * exp(-param[["alpha2"]] * (x - param[["mu2"]]) - exp(-param[["lambda2"]]*(x - param[["mu2"]]))) +
    param[["c"]]

  if(scaled==TRUE)
    m <- m/sum(m)
  m
  
}
