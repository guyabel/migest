library(mipfp)
library(magrittr)

##
## frans data
##
m0 <- matrix(data = c(5, 2, 1, 7), nrow = 2, ncol = 2, 
             dimnames = list(orig = LETTERS[1:2], dest = LETTERS[1:2]))
addmargins(m0)

m1 <- Ipfp(seed = m0, 
          target.list = list(1,2), 
          target.data = list(c(18,20), c(16,22)))
m1$x.hat %>%
  addmargins() %>%
  round(., 2)

##
## other methods (each give different results)
##
m <- c("ipfp", "ml", "chi2", "lsq")
m2 <- vector("list", 4) 
names(m2) <- m

m2$ipfp <- m1
for(i in 2:4){
  m2[[i]] <- Estimate(seed = m0, 
                      target.list = list(1, 2), 
                      target.data = list(c(18, 20), c(16, 22)),
                      method = m[i],
                      compute.cov = TRUE)
}

m2$ipfp$x.hat %>%
  addmargins() %>%
  round(., 3)

m2$ml$x.hat %>%
  addmargins() %>%
  round(., 3)

m2$chi2$x.hat %>%
  addmargins() %>%
  round(., 3)

m2$lsq$x.hat %>%
  addmargins() %>%
  round(., 3)

for(i in 1:4){
  m2[[i]]$x.hat.se <- vcov(object = m2[[i]])$x.hat.se
  m2[[i]]$p.hat.se <- vcov(object = m2[[i]])$p.hat.se
}
GetConfInt(m2$ipfp)
GetConfInt(m2$ml)
GetConfInt(m2$chi2) #only positive lower bound
GetConfInt(m2$lsq)



##
## msc data situation. gives same result as cm3
##
m0 <- array(data = c(5, 1, 2, 7, 4, 2, 5, 9), dim = c(2, 2, 2),
            dimnames = list(orig = LETTERS[1:2], 
                            dest = LETTERS[1:2], 
                            type = c("ILL", "HEALTHY")))

m <- Ipfp(seed = m0, 
          target.list = list(1,2), 
          target.data = list(c(18,20)*2,c(16,22)*2))
addmargins(m$x.hat)


##
## place of birth data situation
##
dn <- LETTERS[1:4]
P1 <- matrix(c(1000, 100,  10,   0,
               55,   555,  50,   5,
               80,    40, 800 , 40,
               20,    25,  20, 200),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(pob = dn, por = dn))
P2 <- matrix(c(950, 100,  60,   0,
                80, 505,  75,   5,
                90,  30, 800,  40,
                40,  45,   0, 180),
             nrow = 4, ncol = 4, byrow = TRUE,
             dimnames = list(pob = dn, por = dn))

m0 <- array(data = 1, dim = c(4, 4, 4),
            dimnames = list(orig = LETTERS[1:4], 
                            dest = LETTERS[1:4], 
                            pob = LETTERS[1:4]))

m <- Ipfp(seed = m0, 
          target.list = list(c(1, 3), c(2, 3)), 
          target.data = list(c(t(P1)), c(t(P2))))
addmargins(m$x.hat)


##
## place of birth data situation with known diagonals 
## (can just take them out, set as structural zero and then add back in)... 
##
## ... did more later (see further down) before adding to ffs_demo
##
m0 <- array(data = 1, dim = c(4, 4, 4),
            dimnames = list(orig = LETTERS[1:4], 
                            dest = LETTERS[1:4], 
                            pob = LETTERS[1:4]))

diag_index <- function(x){
  i <- dim(x)[1]
  d <- seq(from = 1, to = i^2, length.out = i)
  dd <- d
  if(!is.na(dim(x)[3]) & dim(x)[3]>1){
    for(j in 1:(dim(x)[3]-1))
      dd <- c(dd, max(dd) + d)
  }
  return(dd)
}
m1 <- replace(x = m0, list = diag_index(m0), values = 0)

ffs_diag_max <- function(s1 = P1, s2 = P2){
  d <- cbind(c(s1), c(s2))
  m <- apply(X = d, MARGIN = 1, FUN = min)
  return(m)
}

ffs_margin_min <- function(s1 = P1, s2 = P2){
  s3 <- s1
  s3[,] <- ffs_diag_max(s1, s2)
  return(list(s1 - t(s3),
              s2 - t(s3)))
}
ffs_margin_min(P1, P2)[[1]]

mm <- ffs_margin_min(P1, P2)
m <- Ipfp(seed = m1, 
          target.list = list(c(1, 3), c(2, 3)), 
          target.data = list(c(t(mm[[1]])), c(t(mm[[2]]))))
addmargins(m$x.hat)
m2 <- replace(x = m$x.hat, list = diag_index(m$x.hat), values = ffs_diag_max(s1 = P1, s2 = P2))
addmargins(m2)







library(purrr)
replace_diag <- function(x){
  diag(x) <- 0; 
  return(x)
} 


alply(.data = m0, .margins = 3, .fun = replace_diag)
aaply(.data = m0, .margins = 3, .fun = replace(., list = d, values = 0))
m1 <- array_tree(m0, 3) %>%
  map(replace_diag) %>%
  list_to_array()
  
  count()
  plyr::list_to_vector()

  # modify_at(diag, 3, 1)
  # map(diag)
  map({'diag<-'1})







GetConfInt(list.est = m, alpha = 0.05)

m_cov <- IpfpCov(estimate = m$x.hat, seed = m0, target.list = list(1,2))
n <- sum(m$x.hat)
# ... lower bound
m_lb <- 
m_ub <- c(m$x.hat) + 1.96 * sqrt(n * diag(m_cov))

m_lb <- m_ub <- m0
m_lb[] <- c(m$x.hat) - 1.96 * sqrt(n * diag(m_cov))
m_ub[] <- c(m$x.hat) + 1.96 * sqrt(n * diag(m_cov))
addmargins(m_lb)
addmargins(m$x.hat)
addmargins(m_ub)

# ... upperbound
ci.ub <- Array2Vector(res$x.hat) + 1.96 * sqrt(n * diag(res.cov))


m2 <- Estimate(seed = m0, 
               target.list = list(1,2), 
               target.data = list(c(18,20),c(16,22)),
               method = "chi2")
addmargins(m1$x.hat)
Estimate()


# set-up an initial 3-way table of dimension (2 x 2 x 2)
seed <- Vector2Array(c(80, 60, 20, 20, 40, 35, 35, 30), dim = c(c(2, 2, 2)))

# building target margins
margins12 <- c(2000, 1000, 1500, 1800)
margins12.array <- Vector2Array(margins12, dim=c(2, 2))
margins3 <- c(4000,2300)
margins3.array <- Vector2Array(margins3, dim = 2) 
target.list <- list(c(1, 2), 3)
target.data <- list(margins12.array, margins3.array)

# estimating the new contingency table using the ml method
results.ml <- ObtainModelEstimates(seed, target.list, target.data, 
                                   compute.cov = TRUE)
print(results.ml)
addmargins(results.ml$x.hat)



###
### more playing around with quasi indpendence
###

library(mipfp)

# independence
Ipfp(
  seed = matrix(1, 4, 4), 
  target.list = list(1, 2), 
  target.data = list(c(100, 10, 10, 0), 
                     c(70, 30, 10, 10))
)

# quasi independence
Ipfp(
  seed = matrix(data = c(70, 1, 1, 1, 
                         1, 10, 1, 1, 
                         1, 1, 10, 1, 
                         1, 1, 1, 0), nrow = 4, ncol = 4), 
  target.list = list(c(1, 2), 1, 2), 
  target.data = list(c(70, NA, NA, NA, 
                       NA, 10, NA, NA, 
                       NA, NA, 10, NA, 
                       NA, NA, NA, 0),
                     c(100, 10, 10, 0),
                     c(70, 30, 10, 10)),
  na.target = TRUE, tol = 0.0001, print = FALSE
)

# quasi independence - if seed is all 1 makes no difference
Ipfp(
  seed = matrix(1, nrow = 4, ncol = 4),
  target.list = list(c(1, 2), 1, 2), 
  target.data = list(c(70, NA, NA, NA, 
                       NA, 10, NA, NA, 
                       NA, NA, 10, NA, 
                       NA, NA, NA, 0),
                     c(100, 10, 10, 0),
                     c(70, 30, 10, 10)),
  na.target = TRUE, tol = 0.0001, print = FALSE
)


# different, more challenging matrix
Ipfp(
  seed = matrix(1, nrow = 4, ncol = 4),
  target.list = list(c(1, 2), 1, 2), 
  target.data = list(c(20, NA, NA, NA, 
                       NA, 55, NA, NA, 
                       NA, NA, 10, NA, 
                       NA, NA, NA, 10),
                     c(20, 55, 25, 10),
                     c(25, 60, 10, 15)),
  na.target = TRUE, tol = 0.0001, print = FALSE
) %>%
  suppressWarnings()

# better than currrent ipf3_qi
orig            A       B      C       D death outside
A       20.1743  0.0000 0.0000  0.0000     0       0
B        0.0000 55.1739 0.0000  0.0000     0       0
C        4.6737  5.5911 9.9548  4.8870     0       0
D        0.0000  0.0000 0.0000 10.0452     0       0

# try zero diagonals to see if improve
s <- matrix(1, nrow = 4, ncol = 4)
diag(s) <- 0
row_tot <- c(20, 55, 25, 10) - c(20, 55, 10, 10)
col_tot <- c(25, 60, 10, 15) - c(20, 55, 10, 10)

e0 <- Ipfp(
  seed = s,
  target.list = list(1, 2), 
  target.data = list(row_tot, col_tot)
)
e0$x.hat + diag(c(20, 55, 10, 10))

