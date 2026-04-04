# Multiplicative component description of origin-destination migration flow tables

Multiplicative component descriptions of *n*-dimension flow tables based
on total reference coding system.

## Usage

``` r
multi_comp(m)
```

## Arguments

- m:

  `matrix` or `array` of migration flows

## Value

`matrix` or `array` of multiplicative components of `m`. When output is
an array the total for each table of origin-destination flows is used.

## References

Rogers, A., Willekens, F., Little, J., & Raymer, J. (2002). Describing
migration spatial structure. Papers in Regional Science, 81(1), 29–48.
https://doi.org/10.1007/s101100100090

Raymer, J., Bonaguidi, A., & Valentini, A. (2006). Describing and
projecting the age and spatial structures of interregional migration in
Italy. Population, Space and Place, 12(5), 371–388.
https://doi.org/10.1002/psp.414

## Examples

``` r
r <- LETTERS[1:4]
m0 <- matrix(data = c(0, 100, 30, 70, 50, 0, 45, 5, 60, 35, 0, 40, 20, 25, 20, 0), 
             nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(orig = r, dest = r))
addmargins(m0)
#>      dest
#> orig    A   B  C   D Sum
#>   A     0 100 30  70 200
#>   B    50   0 45   5 100
#>   C    60  35  0  40 135
#>   D    20  25 20   0  65
#>   Sum 130 160 95 115 500
multi_comp(m = m0)
#>      dest
#> orig            A           B           C           D         Sum
#>   A     0.0000000   1.5625000   0.7894737   1.5217391   0.4000000
#>   B     1.9230769   0.0000000   2.3684211   0.2173913   0.2000000
#>   C     1.7094017   0.8101852   0.0000000   1.2882448   0.2700000
#>   D     1.1834320   1.2019231   1.6194332   0.0000000   0.1300000
#>   Sum   0.2600000   0.3200000   0.1900000   0.2300000 500.0000000

# data frame
library(dplyr)
italy_area %>%
  filter(year == 2000) %>%
  multi_comp() %>%
  round(digits = 3)
#> , , age_grp = 0-4
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.401      0.859      0.909      2.370      0.010
#>   Islands         0.970      0.000      1.181      1.513      0.681      0.012
#>   North-East      1.053      1.916      0.000      1.179      2.501      0.010
#>   North-West      0.877      2.490      0.887      0.000      2.023      0.014
#>   South           1.409      0.531      1.184      1.102      0.000      0.025
#>   Sum             0.016      0.007      0.017      0.018      0.014      0.072
#> 
#> , , age_grp = 5-9
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.589      0.779      0.762      2.243      0.007
#>   Islands         1.166      0.000      1.393      1.707      0.562      0.010
#>   North-East      0.840      1.932      0.000      0.936      2.085      0.006
#>   North-West      0.877      2.714      0.844      0.000      1.963      0.010
#>   South           1.387      0.507      1.283      1.151      0.000      0.018
#>   Sum             0.011      0.005      0.012      0.012      0.009      0.050
#> 
#> , , age_grp = 10-14
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.570      0.738      0.667      1.978      0.005
#>   Islands         1.333      0.000      1.572      1.791      0.463      0.008
#>   North-East      0.861      1.834      0.000      0.840      1.805      0.004
#>   North-West      0.793      2.694      0.826      0.000      1.959      0.007
#>   South           1.424      0.411      1.332      1.226      0.000      0.014
#>   Sum             0.009      0.004      0.010      0.009      0.006      0.037
#> 
#> , , age_grp = 15-19
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.358      0.732      0.668      1.673      0.005
#>   Islands         1.261      0.000      1.617      2.109      0.417      0.009
#>   North-East      0.677      1.769      0.000      0.847      1.697      0.004
#>   North-West      0.629      2.606      0.818      0.000      1.803      0.007
#>   South           1.449      0.347      1.449      1.340      0.000      0.016
#>   Sum             0.009      0.004      0.011      0.011      0.006      0.041
#> 
#> , , age_grp = 20-24
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.044      0.852      0.759      1.552      0.014
#>   Islands         0.983      0.000      1.490      1.948      0.436      0.025
#>   North-East      0.593      1.530      0.000      0.852      1.808      0.012
#>   North-West      0.533      1.880      0.726      0.000      1.449      0.018
#>   South           1.419      0.425      1.788      1.624      0.000      0.055
#>   Sum             0.025      0.009      0.036      0.037      0.017      0.124
#> 
#> , , age_grp = 25-29
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.092      0.992      0.939      2.093      0.027
#>   Islands         0.915      0.000      1.221      1.599      0.544      0.034
#>   North-East      0.910      1.420      0.000      1.161      1.829      0.023
#>   North-West      0.795      1.652      0.947      0.000      1.719      0.034
#>   South           1.473      0.482      1.457      1.373      0.000      0.079
#>   Sum             0.044      0.014      0.053      0.054      0.032      0.197
#> 
#> , , age_grp = 30-34
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.211      1.088      1.159      2.136      0.025
#>   Islands         0.915      0.000      1.053      1.390      0.526      0.025
#>   North-East      1.143      1.384      0.000      1.362      1.837      0.021
#>   North-West      0.945      1.857      1.091      0.000      1.756      0.031
#>   South           1.544      0.445      1.205      1.244      0.000      0.059
#>   Sum             0.039      0.012      0.040      0.043      0.027      0.160
#> 
#> , , age_grp = 35-39
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.439      1.175      1.245      2.126      0.016
#>   Islands         0.956      0.000      1.073      1.372      0.407      0.015
#>   North-East      1.278      1.396      0.000      1.484      1.719      0.013
#>   North-West      1.158      2.026      1.229      0.000      1.753      0.020
#>   South           1.465      0.424      1.089      1.085      0.000      0.032
#>   Sum             0.024      0.008      0.024      0.025      0.015      0.096
#> 
#> , , age_grp = 40-44
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.547      1.283      1.266      2.200      0.009
#>   Islands         1.001      0.000      1.090      1.445      0.367      0.008
#>   North-East      1.322      1.563      0.000      1.417      1.626      0.007
#>   North-West      1.234      2.353      1.261      0.000      1.885      0.012
#>   South           1.317      0.354      1.044      1.001      0.000      0.017
#>   Sum             0.013      0.005      0.014      0.014      0.009      0.054
#> 
#> , , age_grp = 45-49
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.638      1.130      1.204      2.331      0.005
#>   Islands         1.076      0.000      1.100      1.372      0.400      0.005
#>   North-East      1.406      1.701      0.000      1.501      1.607      0.005
#>   North-West      1.320      2.600      1.354      0.000      2.007      0.008
#>   South           1.286      0.408      0.919      0.912      0.000      0.010
#>   Sum             0.008      0.003      0.008      0.008      0.006      0.033
#> 
#> , , age_grp = 50-54
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.887      1.064      1.110      2.579      0.005
#>   Islands         0.997      0.000      0.861      1.226      0.361      0.004
#>   North-East      1.449      1.709      0.000      1.505      1.541      0.004
#>   North-West      1.519      3.174      1.595      0.000      2.391      0.008
#>   South           1.267      0.366      0.738      0.831      0.000      0.007
#>   Sum             0.007      0.003      0.006      0.006      0.005      0.028
#> 
#> , , age_grp = 55-59
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      2.263      1.084      1.029      2.894      0.004
#>   Islands         0.845      0.000      0.643      1.027      0.343      0.003
#>   North-East      1.448      1.641      0.000      1.455      1.391      0.003
#>   North-West      1.724      3.929      1.892      0.000      2.921      0.008
#>   South           1.160      0.398      0.544      0.722      0.000      0.005
#>   Sum             0.006      0.003      0.005      0.005      0.005      0.023
#> 
#> , , age_grp = 60-64
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      2.271      1.067      1.084      3.282      0.004
#>   Islands         0.767      0.000      0.397      0.933      0.414      0.002
#>   North-East      1.331      1.473      0.000      1.578      1.500      0.003
#>   North-West      1.633      4.038      1.938      0.000      3.047      0.008
#>   South           1.245      0.395      0.444      0.734      0.000      0.005
#>   Sum             0.005      0.003      0.004      0.004      0.005      0.022
#> 
#> , , age_grp = 65-69
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      2.383      1.159      1.030      3.451      0.003
#>   Islands         0.827      0.000      0.435      0.876      0.385      0.002
#>   North-East      1.222      1.237      0.000      1.629      1.439      0.002
#>   North-West      1.518      3.324      1.891      0.000      2.933      0.005
#>   South           1.340      0.479      0.419      0.874      0.000      0.004
#>   Sum             0.004      0.002      0.003      0.004      0.004      0.017
#> 
#> , , age_grp = 70-74
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      2.409      0.999      1.353      3.269      0.003
#>   Islands         0.723      0.000      0.381      1.253      0.385      0.002
#>   North-East      1.301      1.200      0.000      1.765      1.113      0.002
#>   North-West      1.421      2.608      1.719      0.000      2.445      0.004
#>   South           1.451      0.432      0.460      1.065      0.000      0.004
#>   Sum             0.004      0.001      0.003      0.004      0.003      0.014
#> 
#> , , age_grp = 75-79
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.926      1.174      1.311      2.957      0.003
#>   Islands         0.819      0.000      0.352      1.352      0.431      0.002
#>   North-East      1.395      0.840      0.000      2.114      0.929      0.002
#>   North-West      1.327      2.463      1.810      0.000      1.963      0.003
#>   South           1.450      0.437      0.488      1.173      0.000      0.004
#>   Sum             0.003      0.001      0.003      0.004      0.002      0.013
#> 
#> , , age_grp = 80-84
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.846      1.070      1.503      2.636      0.002
#>   Islands         0.804      0.000      0.428      1.295      0.519      0.001
#>   North-East      1.466      0.631      0.000      2.117      0.986      0.001
#>   North-West      1.232      2.001      1.825      0.000      1.826      0.002
#>   South           1.571      0.408      0.493      1.258      0.000      0.003
#>   Sum             0.002      0.001      0.002      0.002      0.001      0.008
#> 
#> , , age_grp = 85-89
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.545      1.509      1.606      2.575      0.001
#>   Islands         0.739      0.000      0.383      1.345      0.414      0.001
#>   North-East      1.766      1.254      0.000      2.809      0.913      0.001
#>   North-West      1.090      1.667      1.944      0.000      1.395      0.002
#>   South           1.410      0.301      0.415      1.240      0.000      0.002
#>   Sum             0.002      0.000      0.001      0.002      0.001      0.007
#> 
#> , , age_grp = 90-94
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      1.319      1.211      1.906      2.277      0.001
#>   Islands         0.809      0.000      0.418      1.033      0.359      0.000
#>   North-East      1.469      1.083      0.000      2.835      0.660      0.000
#>   North-West      1.494      1.635      2.216      0.000      1.778      0.001
#>   South           1.452      0.250      0.387      1.142      0.000      0.001
#>   Sum             0.001      0.000      0.001      0.001      0.000      0.003
#> 
#> , , age_grp = 95+
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      0.886      1.076      1.504      2.207      0.000
#>   Islands         0.847      0.000      0.521      0.835      0.523      0.000
#>   North-East      1.383      1.750      0.000      2.340      0.698      0.000
#>   North-West      1.707      2.593      2.149      0.000      2.263      0.000
#>   South           1.394      0.485      0.523      0.965      0.000      0.000
#>   Sum             0.000      0.000      0.000      0.000      0.000      0.001
#> 
#> , , age_grp = Sum
#> 
#>             dest
#> orig             Center    Islands North-East North-West      South        Sum
#>   Center          0.000      0.017      0.037      0.039      0.054      0.148
#>   Islands         0.038      0.000      0.048      0.067      0.013      0.166
#>   North-East      0.030      0.016      0.000      0.041      0.037      0.125
#>   North-West      0.045      0.037      0.056      0.000      0.063      0.202
#>   South           0.120      0.013      0.111      0.115      0.000      0.360
#>   Sum             0.233      0.084      0.253      0.263      0.168 277436.000
#> 
```
