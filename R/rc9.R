##
##rogers castro curve
##
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
