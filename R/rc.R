#' Generate a Rogers-Castro migration schedules
#'
#' @param parameters A data frame with two columns. First column must be named `param` and second `value`
#' @param x Vector of ages
#' @param n Numeric value for number of parameters in the age schedule. By default takes the number of rows in `param`.
#' @param scaled Scale estimates to sum to one across all ages (`x`).
#'
#' @return Returns the M(x) values from the Rogers-Castro schedule of age specific migration rate. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. 
#' The \code{param} argument must be a data frame with two columns names `param` and `value`. The names in the `param` column must be one of `a1`, `alpha1`, `a2`, `alpha2`, `mu2`, `lambda2`, `a3`, `alpha3`, `mu3`, `lambda3`, `a4`, `lambda4` and `c`.
#' @references Rogers, A., and L. J. Castro. (1981). Model Migration Schedules. \emph{IIASA Research Report 81} RR-81-30
#' @author Guy J. Abel
#' @export
#'
#' @examples
#' # single year age group, seven parameters
#' m <- rc(param = model_rc_fun)
#' plot(1:100, m, type = "l")
#' 
#' # un model schedules
#' library(dplyr)
#' library(purrr)
#' library(tidyr)
#' library(stringr)
#' library(ggplot2)
#' model_rc_un %>%
#'   group_by(sex, schedule, schedule_abb) %>%
#'   nest() %>%
#'   mutate(m = map(.x = data, .f = ~rc(parameters = .x)),
#'          x = list(1:100)) %>%
#'   unnest(c(m, x)) %>%
#'   mutate(schedule = str_wrap(schedule, width = 20)) %>%
#'   ggplot(mapping = aes(x = x, y = m, colour = schedule)) +
#'   geom_line() +
#'   facet_wrap(facets = "sex", ncol = 1)
#' 
#' # 5 year age groups
#' x <- seq(2.5, 97.5, 5)
#' m <- rc(x, param = model_rc_fun)
#' plot(x, m, type="l")
#' 
#' # 11 parameters based on US mean values in Table 8 of Rogers and Castro (1981)
#' tibble::tribble(
#'   ~param, ~value,
#'   "a1",  0.021,
#'   "alpha1",  0.075,
#'   "a2",   0.06,
#'   "alpha2",  0.118,
#'   "mu2",  20.14,
#'   "lambda2",  0.569,
#'   "a3",  0.002,
#'   "alpha3",   0.43,
#'   "mu3",   81.8,
#'   "lambda3",  0.119,
#'   "c",  0.002
#' ) %>%
#'   rc() %>%
#'   plot(x = 1:100, y = ., type = "l")
rc <- function(parameters = NULL, x = 1:100, n = nrow(parameters), scaled=TRUE){
  if(names(parameters)[1] != "param")
    stop("parameters must have two columns, and the first name must be param")
  if(names(parameters)[2] != "value")
    stop("parameters must have two columns, and the first name must be value")
  pp <- c("a1","alpha1",
          "a2","alpha2","mu2","lambda2",
          "a3","alpha3","mu3","lambda3",
          "a4","lambda4",
          "c")
  if(!any(parameters$param %in% pp))
    stop("one or more of parameter names in param column are not recognised, see help file")
  
  param <- value <- NULL
  p <- parameters %>%
    dplyr::slice(1:n) %>%
    dplyr::select(param, value) %>%
    tibble::deframe()
  
  if(n == 7){
    m <- p[["a1"]] * exp(-p[["alpha1"]] * x) +
      p[["a2"]] * exp(-p[["alpha2"]] * (x - p[["mu2"]]) - exp(-p[["lambda2"]]*(x - p[["mu2"]]))) +
      p[["c"]]
  }
  
  
  if(n == 9){
    m <- p[["a1"]] * exp(-p[["alpha1"]] * x) +
      p[["a2"]] * exp(-p[["alpha2"]] * (x - p[["mu2"]]) - exp(-p[["lambda2"]]*(x - p[["mu2"]]))) +
      p[["a3"]] * exp(p[["alpha3"]] * x) +
      p[["c"]]
  }
  
  if(n == 11){
    m <- p[["a1"]] * exp(-p[["alpha1"]] * x) +
      p[["a2"]] * exp(-p[["alpha2"]] * (x - p[["mu2"]]) - exp(-p[["lambda2"]]*(x - p[["mu2"]]))) +
      p[["a3"]] * exp(-p[["alpha3"]] * (x - p[["mu3"]]) - exp(-p[["lambda3"]]*(x - p[["mu3"]]))) +
      p[["c"]]
  }
  
  
  
  if(n == 13){
    m <- p[["a1"]] * exp(-p[["alpha1"]] * x) +
      p[["a2"]] * exp(-p[["alpha2"]] * (x - p[["mu2"]]) - exp(-p[["lambda2"]]*(x - p[["mu2"]]))) +
      p[["a3"]] * exp(-p[["alpha3"]] * (x - p[["mu3"]]) - exp(-p[["lambda3"]]*(x - p[["mu3"]]))) +
      p[["a4"]] * exp(p[["lambda4"]] * x) +
      p[["c"]]
  }
  
  if(scaled==TRUE)
    m <- m/sum(m)
  m
}
