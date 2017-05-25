#' Estimation of Bilateral Migrant Flows from Bilteral Migrant Stocks
#' 
#' Estimates migrant transitions flows between two sequential migrant stock tables.
#' @param P1 Matrix of migrant stock totals at time \emph{t}. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t} 
#' @param P2 Matrix of migrant stock totals at time \emph{t}+1. Rows in the matrix correspond to place of birth and columns to place of residence at time \emph{t}+1.
#' @param d Vector of the number of deaths between time \emph{t} and \emph{t}+1 in each region.
#' @param b Vector of the number of births between time \emph{t} and \emph{t}+1 in each region.
#' @param m Matrix of auxiliary data. By default set to 1 for all origin-destination combinations.
#' @param method Method used to adjust row margin totals of \code{P1} and \code{P2} to equal. By default \code{method="stocks"}. Can also take values \code{method="outside"} or \code{method="deaths"}. See details for explanation on each method.
#' @param b.mat Matrix containing the number of births during the period in each birthplace (rows) by place of residence (columns) combination created by the user. By default, this argument is \code{NULL}, and hence within the function a \code{b.mat} is formed as a diagonal matrix of \code{b} (i.e. all births happen in their respective place of residence at the end of the period, there are no infant migrants.)
#' @param d.mat Matrix containing the number of deaths during the period in each birthplace (rows) by place of residence (columns) combination created by the user. By default, this argument is \code{NULL}, and hence within the function a \code{d.mat} is formed as a proportional allocation of \code{d} over all populations (i.e. the mortality rate in each place of birth sub-group (native born and all foreign born stocks) is the same.)
#' @param b.deduct Method used to deduct births. By default \code{b.deduct="native.gt0"} deducts births from diagonals of stock table (i.e. the native born populations) with the exception of regions where this would lead to a negative adjusted population. In these select regions, births are spread over all population stocks (both native and foreign) thus avoiding potential negative flows. Can also take \code{b.deduct="native.only"} in which all births are deducted from the diagonals of stock table (i.e. native born populations only).
#' @param ... Additional arguments passes to \code{\link{ipf3.qi}}.
#'
#' @return
#' Estimates migrant transitions flows between two sequential migrant stock tables as shown in Abel (2013), when \code{method="outside"}. The length of \code{b} and \code{d} must equal the number of rows in \code{P1} and number of columns in \code{P2}.
#' 
#' Setting \code{method="stocks"} estimates migration flows using an alternative demographic accounting method to adjusted stock tables to match the row totals of the stock table after demographic accounting. Setting \code{method="deaths"} uses the calculation of the deaths by place of residence table to match the row totals of the stock table after demographic accounting. Note, when \code{b} and \code{d} are equal, the same estimated flows from stocks are obtained regardless of the \code{method} argument. Both of these options maintain the net migration flow implied by the population, birth and death data. I still need to write up these methods. 
#' 
#' Setting \code{b.deduct="native.gt0"} allows estimates to correct for cases where the number of births far exceeds the change in the native born population. Such cases potentially occur where place of birth stock data imply a change in native born population which directly conflict with changes in demographic data unless there is mass migration of all new borns.
#' 
#' Returns a \code{list} object with:
#' \item{mu }{Array of indirect estimates of origin-destination matrices by place of birth.}
#' \item{it }{Iteration count.}
#' \item{tol }{Tolerance level at final iteration.}
#' \item{y }{Array of indirect estimates of origin-destination matrices by place of birth with additional rows and columns for births, deaths and moves to other regions.}
#' @references 
#' Abel, G. J. (2016). Estimates of Global Bilateral Migration Flows by Gender between 1960 and 2015. \emph{Vienna Institute of Demography Working Papers} 2/2016.
#' 
#' Abel, G. J. and Sander, N. (2014). Quantifying Global International Migration Flows. \emph{Science}, 343 (6178) 1520-1522
#' 
#' Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place of Birth. \emph{Demographic Research} 28, (18) 505-546
#' @author Guy J. Abel
#' @seealso \code{\link{ipf3.qi}}, \code{\link{fm}}
#' @export
#'
#' @examples
#' ## create P1 and P2 stock tables
#' dn <- LETTERS[1:4]
#' P1 <- matrix(c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200), 4, 4,
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' P2 <- matrix(c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180), 4, 4,
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' # display with row and col totals
#' addmargins(P1)
#' addmargins(P2)
#' 
#' # no births and deaths
#' b <- rep(0, 4)
#' d <- rep(0, 4)
#' 
#' y <- ffs(P1, P2, d, b)
#' # display with row, col and table totals
#' round(addmargins(y$mu), 1)
#' # display with row and col totals
#' round(fm(y$mu), 1)
#' 
#' ## alternative offset term
#' dis <- array(c(1, 2, 3, 4, 2, 1, 5, 6, 3, 4, 1, 7, 4, 6, 7, 1), c(4, 4, 1))
#' y <- ffs(P1, P2, d, b, dis)
#' # display with row, col and table totals
#' round(addmargins(y$mu), 1)
#' # display with row and col totals
#' round(fm(y$mu), 1)
#' 
#' ## alternative P2 and changes in population from natural increase
#' P2 <- matrix(c(1060, 60, 10, 10, 45, 540, 40, 0, 70, 75, 770, 70, 30, 30, 20, 230), 4, 4, 
#'              dimnames = list(pob = dn, por = dn), byrow = TRUE)
#' # display with row and col totals
#' addmargins(P2)
#' b <- c(80, 20, 40, 60)
#' d <- c(70, 30, 50, 10)
#' 
#' y <- ffs(P1, P2, d, b, method="outside")
#' # display with row, col and table totals
#' round(addmargins(y$mu), 1)
#' # display with row and col totals
#' round(fm(y$mu), 1)
#P1=P1;P2=P2;d=d;b=b.adj;m=m
#P1 = s0; P2 = s1; d = df7$d; b = df7$b.adj; m = dm; method = "stocks"
#method="stocks"; d.mat=NULL;b.mat=NULL;b.deduct="native.gt0"
#m=NULL;
ffs <- function(P1,
           P2,
           d,
           b,
           m = NULL,
           method = "stocks",
           b.mat = NULL,
           d.mat = NULL,
           b.deduct = "native.gt0",
           ...) {
    if (!(method %in% c("outside", "stocks", "deaths")) |
        length(method) != 1)
      stop("method must be one of outside, stocks or deaths")
    if (!(b.deduct %in% c("native.only", "native.gt0")) |
        length(b.deduct) != 1)
      stop("method must be one of outside, stocks or deaths")
    
    R <- unique(c(dim(P1), dim(P2)))
    if (length(R) != 1)
      stop("P1 and P2 matrices must be square and with the same dimensions.")
    dn <- dimnames(P1)[[1]]
    
    #set up offset
    if (length(dim(m)) == 2) {
      m <- array(c(m), c(R, R, R))
    }
    if (is.null(m)) {
      m <- array(1, c(R, R, R))
    }
    if (is.null(dimnames(m))) {
      dimnames(m) <- list(orig = dn,
                          dest = dn,
                          pob = dn)
    }

    if (method == "stocks") {
      if (round(sum(P2 - P1), 1) != round(sum(b - d), 1)) {
        message("sum(P2-P1): ", sum(P2 - P1))
        message("sum(b-d):   ", sum(b - d))
        stop("difference in stock tables must be equal to sum of all births - sum of all deaths")
      }
    }
    
    #set up migration matrix with births/deaths and others rows and columns
    y <- m
    y <- array(0, dim(m) + c(2, 2, 0))
    dimnames(y) <-
      list(
        o = c(dimnames(m)[[1]], "b", "O"),
        d = c(dimnames(m)[[1]], "d", "O"),
        b = dimnames(m)[[3]]
      )
    y[, , ] <- 0
    
    #step 1-2a take off deaths
    message("Adjust for Deaths...")
    if (is.null(d.mat))
      d.mat <- ipf2(ctot = d, m = P1)$mu
    if (method == "deaths") {
      d.mat <- ipf2(ctot = d,
                    rtot = rowSums(P1) + b - rowSums(P2),
                    m = P1)$mu
    }
    y[1:R, R + 1, ] <- t(d.mat)
    P1.adj <- P1 - d.mat
    
    #step 1-2b take off births
    message("Adjust for Births...")
    if (is.null(b.mat))
      b.mat <- diag(b)
    y[R + 1, 1:R, ] <- b.mat
    P2.adj <- P2 - b.mat
    #adjust for negative nb sums after birth deduction...spread births to all population where needed (new to v1.6)
    if (b.deduct == "native.gt0") {
      ii <- diag(P2.adj < 0)
      if (sum(ii) > 0) {
        b.mat[, ii] <- ipf2(ctot = b, m = P2)$mu[, ii]
        P2.adj <- P2 - b.mat
      }
    }
    
    #step 3-4a take off moves in from external or adjust P1.adj rows
    message("Remaining Adjustments (P1)...")
    dif <- rowSums(P1.adj) - rowSums(P2.adj)
    if (method == "outside" | method == "deaths") {
      #following is in versions <1.3. is wrong. those leaving contolled for in P1, like those who die
      #this (in the #) is labelled in.mat but should be out.mat (where the dif>0). should have offset P1.adj, where they leave from, not P2.adj
      #in.mat<-t(ipf2(ctot=pmax(dif,0),m=t(P2.adj))$mu)
      #P1.adj<-P1.adj-in.mat
      #y[R+2,1:R,]<-t(in.mat)
      out.mat <- t(ipf2(ctot = pmax(dif, 0), m = t(P1.adj))$mu)
      P1.adj <- P1.adj - out.mat
      y[1:R, R + 2, ] <- t(out.mat)
    }
    if (method == "stocks") {
      #was not reaching convergence of near zero difference in maxdiff in <v1.5, i.e. either row or col totals in estimates were not matching arguments.
      P1.adj <-
        ipf2(
          rtot = rowSums(P1.adj) - dif / 2,
          ctot = colSums(P1.adj),
          m = P1.adj,
          maxit = 100000,
          tol = 0.1
        )$mu
    }
    
    #step 3-4b take off moves out from external or adjust P2.adj rows
    message("Remaining Adjustments (P2)...")
    if (method == "outside" | method == "deaths") {
      #following is in versions <1.3. is wrong. those arriving contolled for in P2, like those who are born
      #this (in the #) is labelled out.mat but should be in.mat (where the dif<0). should have offset P2.adj, where they arrive too, not P1.adj
      #out.mat<-t(ipf2(ctot=pmax(-dif,0),m=t(P1.adj))$mu)
      #P2.adj<-P2.adj-out.mat
      #y[1:R,R+2,]<-t(out.mat)
      in.mat <- t(ipf2(ctot = pmax(-dif, 0), m = t(P2.adj))$mu)
      P2.adj <- P2.adj - in.mat
      y[R + 2, 1:R, ] <- t(in.mat)
    }
    if (method == "stocks") {
      #was not reaching convergence of near zero difference in maxdiff in <v1.5, i.e. either row or col totals in estimates were not matching arguments.
      P2.adj <-
        ipf2(
          rtot = rowSums(P2.adj) + dif / 2,
          ctot = colSums(P2.adj),
          m = P2.adj,
          maxit = 100000,
          tol = 0.1
        )$mu
    }
    
    #step 5 calculate
    message("Calculate Flows...")
    #ipf<-ipf3.qi(rtot=t(P1.adj),ctot=P2.adj,m=m)
    ipf <- ipf3.qi(rtot = t(P1.adj),
                   ctot = P2.adj,
                   m = m,
                   ...)
    y[1:R, 1:R, ] <- ipf$mu
    return(c(ipf, list(y = y)))
  }
