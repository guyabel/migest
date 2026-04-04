# Estimation of bilateral migrant flows from bilateral migrant stocks using demographic accounting approaches

Estimates migrant transitions flows between two sequential migrant stock
tables. Replaces old `ffs`.

## Usage

``` r
ffs_demo(
  stock_start = NULL,
  stock_end = NULL,
  births = NULL,
  deaths = NULL,
  seed = NULL,
  stayer_assumption = TRUE,
  match_global = "before-demo-adjust",
  match_birthplace_tot_method = "rescale",
  birth_method = "native",
  birth_non_negative = TRUE,
  death_method = "proportion",
  verbose = FALSE,
  return = "flow"
)
```

## Arguments

- stock_start:

  Matrix of migrant stock totals at time *t*. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*. Previously had argument name `m1`.

- stock_end:

  Matrix of migrant stock totals at time *t*+1. Rows in the matrix
  correspond to place of birth and columns to place of residence at time
  *t*+1. Previously had argument name `m2`.

- births:

  Vector of the number of births between time *t* and *t*+1 in each
  region. Previously had argument name `b_por`.

- deaths:

  Vector of the number of deaths between time *t* and *t*+1 in each
  region. Previously had argument name `d_por`.

- seed:

  Matrix of auxiliary data. By default set to 1 for all
  origin-destination combinations. Previously had argument name `m`.

- stayer_assumption:

  Logical value to indicate whether to use a quasi-independent or
  independent IPFP to estimate flows. By default uses quasi-independent,
  i.e. is set to `TRUE` and estimates the minimum migration. When set to
  `FALSE` estimates flows under the independent model as used as part of
  Azose and Raftery (2019).

- match_global:

  Character string used to indicate whether to balance the change in
  stocks totals with the changes in births and deaths. Only applied when
  `match_birthplace_tot_method` is either `rescale` or
  `rescale-adjust-zero-fb`. By default uses `after-demo-adjust` rather
  than `before-demo-adjust` which I think minimises risk of negative
  values.

- match_birthplace_tot_method:

  Character string passed to `method` argument in `match_birthplace_tot`
  to ensure place of birth margins in stock tables match.

- birth_method:

  Character string passed to `method` argument in `birth_mat`.

- birth_non_negative:

  Logical value passed to `non_negative` argument in `birth_mat`.

- death_method:

  Character string passed to `method` argument in `death_mat`.

- verbose:

  Logical value to show progress of the estimation procedure. By default
  `FALSE`.

- return:

  Character string used to indicate whether to return the array of
  estimated flows when set to `flow` (default), array of demographic
  accounts when set to `account` or the demographic account, list of
  input settings and the origin-destination matrix when set to `classic`

## Value

Estimates migrant transitions flows between two sequential migrant stock
tables using various methods. See the example section for possible
variations on estimation methods.

Detail of returned object varies depending on the setting used in the
`return` argument.

## References

Abel and Cohen (2019) Bilateral international migration flow estimates
for 200 countries *Scientific Data* 6 (1), 1-13

Azose & Raftery (2019) Estimation of emigration, return migration, and
transit migration between all pairs of countries *Proceedings of the
National Academy of Sciences* 116 (1) 116-122

Abel, G. J. (2018). Estimates of Global Bilateral Migration Flows by
Gender between 1960 and 2015. *International Migration Review* 52 (3),
809–852.

Abel, G. J. and Sander, N. (2014). Quantifying Global International
Migration Flows. *Science*, 343 (6178) 1520-1522

Abel, G. J. (2013). Estimating Global Migration Flow Tables Using Place
of Birth. *Demographic Research* 28, (18) 505-546

## See also

[`ffs_diff`](ffs_diff.md), [`ffs_rates`](ffs_rates.md)

## Author

Guy J. Abel

## Examples

``` r
##
## without births and deaths over period
##
# data as in demographic research and science paper papers
s1 <- matrix(data = c(1000, 100, 10, 0, 55, 555, 50, 5, 80, 40, 800, 40, 20, 25, 20, 200),
             nrow = 4, ncol = 4, byrow = TRUE)
s2 <- matrix(data = c(950, 100, 60, 0, 80, 505, 75, 5, 90, 30, 800, 40, 40, 45, 0, 180),
             nrow = 4, ncol = 4, byrow = TRUE)
b <- d <- rep(0, 4)
r <- LETTERS[1:4]
dimnames(s1) <- dimnames(s2) <- list(birth =  r, dest = r)
names(b) <- names(d) <- r
addmargins(s1)
#>      dest
#> birth    A   B   C   D  Sum
#>   A   1000 100  10   0 1110
#>   B     55 555  50   5  665
#>   C     80  40 800  40  960
#>   D     20  25  20 200  265
#>   Sum 1155 720 880 245 3000
addmargins(s2)
#>      dest
#> birth    A   B   C   D  Sum
#>   A    950 100  60   0 1110
#>   B     80 505  75   5  665
#>   C     90  30 800  40  960
#>   D     40  45   0 180  265
#>   Sum 1160 680 935 225 3000
b
#> A B C D 
#> 0 0 0 0 
d
#> A B C D 
#> 0 0 0 0 

# demographic research and science paper example
e0 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
e0
#> , , pob = A
#> 
#>     dest
#> orig   A   B  C D
#>    A 950   0 50 0
#>    B   0 100  0 0
#>    C   0   0 10 0
#>    D   0   0  0 0
#> 
#> , , pob = B
#> 
#>     dest
#> orig  A   B  C D
#>    A 55   0  0 0
#>    B 25 505 25 0
#>    C  0   0 50 0
#>    D  0   0  0 5
#> 
#> , , pob = C
#> 
#>     dest
#> orig  A  B   C  D
#>    A 80  0   0  0
#>    B 10 30   0  0
#>    C  0  0 800  0
#>    D  0  0   0 40
#> 
#> , , pob = D
#> 
#>     dest
#> orig  A  B C   D
#>    A 20  0 0   0
#>    B  0 25 0   0
#>    C 10 10 0   0
#>    D 10 10 0 180
#> 
sum_od(e0)
#>      dest
#> orig   A  B  C D Sum
#>   A    0  0 50 0  50
#>   B   35  0 25 0  60
#>   C   10 10  0 0  20
#>   D   10 10  0 0  20
#>   Sum 55 20 75 0 150

# international migration review paper example
s1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
s2[,] <- c(70, 25, 10, 40, 30, 60, 55, 45, 10, 10, 140, 0, 10, 15, 50, 180)
addmargins(s1)
#>      dest
#> birth   A   B   C   D Sum
#>   A   100  10  10   0 120
#>   B    20  55  25  10 110
#>   C    10  40 140  65 255
#>   D    20  25  20 200 265
#>   Sum 150 130 195 275 750
addmargins(s2)
#>      dest
#> birth   A   B   C   D Sum
#>   A    70  30  10  10 120
#>   B    25  60  10  15 110
#>   C    10  55 140  50 255
#>   D    40  45   0 180 265
#>   Sum 145 190 160 255 750

e1 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
sum_od(e1)
#>      dest
#> orig   A  B C  D Sum
#>   A    0 20 0 10  30
#>   B    0  0 0  0   0
#>   C   15 15 0  5  35
#>   D   10 25 0  0  35
#>   Sum 25 60 0 15 100

# international migration review supp. material example
# distance matrix
dd <- matrix(data = c(0, 5, 50, 500, 5, 0, 45, 495, 50, 45, 0, 450, 500, 495, 450, 0),
             nrow = 4, ncol = 4, byrow = TRUE)
dimnames(dd) <- list(orig = r, dest = r)
dd
#>     dest
#> orig   A   B   C   D
#>    A   0   5  50 500
#>    B   5   0  45 495
#>    C  50  45   0 450
#>    D 500 495 450   0
e2 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d, seed = dd)
sum_od(e2)
#>      dest
#> orig   A  B C  D Sum
#>   A    0 20 0 10  30
#>   B    0  0 0  0   0
#>   C   15 15 0  5  35
#>   D   10 25 0  0  35
#>   Sum 25 60 0 15 100

##
## with births and deaths over period
##
# demographic research paper example (with births and deaths)
s1[,] <- c(1000, 55, 80, 20, 100, 555, 40, 25, 10, 50, 800, 20, 0, 5, 40, 200)
s2[,] <- c(1060, 45, 70, 30, 60, 540, 75, 30, 10, 40, 770, 20, 10, 0, 70, 230)
b[] <- c(80, 20, 40, 60)
d[] <- c(70, 30, 50, 10)
e3 <- ffs_demo(stock_start = s1, stock_end = s2, 
               births = b, deaths = d, 
               match_birthplace_tot_method = "open-dr")
sum_od(e3)
#>      dest
#> orig         A         B         C         D       Sum
#>   A    0.00000  8.502866 0.0000000  3.695332  12.19820
#>   B   26.42296  0.000000 0.4234563  9.855274  36.70169
#>   C    0.00000 34.183358 0.0000000 25.077888  59.26125
#>   D   11.46761 10.933241 1.3928875  0.000000  23.79374
#>   Sum 37.89057 53.619464 1.8163437 38.628494 131.95487
# makes more sense to use this method
e4 <- ffs_demo(stock_start = s1, stock_end = s2, 
               births = b, deaths = d, 
               match_birthplace_tot_method = "open")
sum_od(e4)
#>      dest
#> orig         A         B         C         D       Sum
#>   A    0.00000  8.897732 0.0000000  3.852711  12.75044
#>   B   26.81098  0.000000 0.4296748 10.000000  37.24065
#>   C    0.00000 35.677466 0.0000000 26.145924  61.82339
#>   D   11.62571 11.251175 1.4120903  0.000000  24.28898
#>   Sum 38.43669 55.826373 1.8417650 39.998635 136.10346

# science paper  supp. material example
b[] <- c(80, 20, 60, 60)
e5 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
sum_od(e5)
#>      dest
#> orig             A         B         C         D        Sum
#>   A    0.000000000  3.328563 0.0000000  3.242990   6.571552
#>   B   34.120952437  0.000000 0.5838738  9.809336  44.514162
#>   C    0.004365738 27.127753 0.0000000 25.711331  52.843450
#>   D   12.446119999  9.058478 2.2595262  0.000000  23.764124
#>   Sum 46.571438174 39.514794 2.8434001 38.763657 127.693289

# international migration review supp. material example (with births and deaths)
s1[,] <- c(100, 20, 10, 20, 10, 55, 40, 25, 10, 25, 140, 20, 0, 10, 65, 200)
s2[,] <- c(75, 20, 30, 30, 25, 45, 40, 30, 5, 30, 150, 20, 0, 15, 60, 230)
b[] <- c(10, 50, 25, 60)
d[] <- c(30, 10, 40, 10)
e6 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
#> Too many births in region B.
#> 
#>                        ... Subtracted births proportionally for this region. Might want to check the input data.
sum_od(e6)
#>      dest
#> orig         A        B         C        D       Sum
#>   A    0.00000 8.126216  0.000000 0.000000  8.126216
#>   B   15.45535 0.000000 18.081832 5.712864 39.250050
#>   C    0.00000 1.125679  0.000000 0.000000  1.125679
#>   D   17.66941 0.000000  8.043363 0.000000 25.712770
#>   Sum 33.12476 9.251895 26.125195 5.712864 74.214715

# scientific data 2019 paper
s1[] <- c(100, 80, 30, 60, 10, 180, 10, 70, 10, 10, 140, 10, 0, 90, 40, 160)
s2[] <- c(95, 75, 55, 35, 5, 225, 0, 25, 15, 5, 115, 25, 5, 55, 50, 215)
b[] <- c(0, 0, 0, 0)
d[] <- c(0, 0, 0, 0)
e7 <- ffs_demo(stock_start = s1, stock_end = s2, births = b, deaths = d)
sum_od(e7)
#>      dest
#> orig          A  B         C         D Sum
#>   A    0.000000  5  7.857143 22.142857  35
#>   B    7.142857  0 12.142857 40.714286  60
#>   C   17.857143  5  0.000000  7.142857  30
#>   D    0.000000 35  0.000000  0.000000  35
#>   Sum 25.000000 45 20.000000 70.000000 160
```
