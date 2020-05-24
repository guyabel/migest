#' Multiplicative Component Description of Origin-Destination Migration Flow Tables
#'
#' Multiplicative component descriptions of *n*-dimension flow tables based on total reference coding system.
#' 
#' @param m \code{matrix} or \code{array} of migration flows
#'
#' @return \code{matrix} or \code{array} of multiplicative components of `m`. When output is an array the total for each table of origin-destination flows is used. 
#' @export
#'
#' @examples
#' m0 <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0), 
#'              nrow = 4, ncol = 4, dimnames = list(orig = n, dest = n), byrow = TRUE)
#' addmargins(m0)
#' multi_comp(m = m0)
multi_comp <- function(m){
  if(length(dim(m)) == 2)
    mm <- multi_comp0(m = m)
  if(length(dim(m)) > 2){
    d0 <- m %>%
      as.data.frame.table(row.names = obs) %>%
      as_tibble()
    
    f0 <- d0 %>%
      select(1:2) %>%
      names() %>%
      paste(collapse = " + ") %>%
      paste("obs ~", .)
    
    f1 <- d0 %>%
      select(-obs) %>%
      names() %>%
      paste(collapse = " + ") %>%
      paste("Freq ~", .)
    
    mm <- d0 %>%
      group_by_at(names(.)[-c(1:2, ncol(.))]) %>%
      nest() %>%
      mutate(m_od = map(data, function(x) xtabs(formula = f0, data = x)),
             m_mc = map(m_od, function(x) multi_comp0(x)),
             d_mc = map(m_mc, function(x) as.data.frame.table(x))) %>%
      select(-data, -m_od, -m_mc) %>%
      unnest(cols = d_mc) %>%
      xtabs(formula = f1)
  }
  return(mm)
}

#' Multiplicative Component Description of Origin-Destination Migration Flow Tables
#'
#' Multiplicative component descriptions of *n*-dimension flow tables based on total reference coding system.
#' 
#' @param m \code{matrix} or \code{array} of migration flows
#'
#' @return \code{matrix} or \code{array} of multiplicative components of `m`. When output is an array the total for each table of origin-destination flows is used. 
#' @export
#'
#' @examples
#' m0 <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0), 
#'              nrow = 4, ncol = 4, dimnames = list(orig = n, dest = n), byrow = TRUE)
#' addmargins(m0)
#' multi_comp(m = m0)
multi_comp0 <- function(m){
  d0 <- m %>%
    addmargins() %>%
    as.data.frame.table(responseName = "obs") %>%
    as_tibble()
  
  f1 <- d0 %>%
    select(-obs) %>%
    names() %>%
    paste(collapse = " + ") %>%
    paste("obs ~", .)
  
  d1 <- d0 %>%
    filter_at(vars(1:"obs"), all_vars(. != "Sum"))
  
  m_ind <- glm(formula = as.formula(f1), data = d1, family = poisson())
  
  d1 <- d1 %>% 
    mutate(fit = m_ind$fitted.values, 
           interact = obs/fit)
  
  f2 <- str_replace(string = f1, pattern = "obs", replacement = "interact")
  
  d2 <- 
    xtabs(formula = f2, data = d1) %>%
    as.data.frame.table() %>%
    as_tibble() %>%
    left_join(d1) %>%
    mutate(prop = c(addmargins(m/sum(m))),
           comp = ifelse(is.na(obs), prop, interact), 
           comp = ifelse(row_number() == n(), sum(d1$obs), comp))
  
  str_replace(string = f2, pattern = "interact", replacement = "comp") %>%
    xtabs(data = d2)
}
