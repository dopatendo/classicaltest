
<!-- README.md is generated from README.Rmd. Please edit that file -->

# classicaltest

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/classicaltest)](https://CRAN.R-project.org/package=classicaltest)
![GitHub R package
version](https://img.shields.io/github/r-package/v/dopatendo/classicaltest)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![Static
Badge](https://img.shields.io/badge/dependencies-none-brightgreen)
[![](https://img.shields.io/badge/doi-10.32614/CRAN.package.classicaltest-green.svg)](https://doi.org/10.32614/CRAN.package.classicaltest)
<!-- ![![](http://cranlogs.r-pkg.org/badges/grand-total/classicaltest?color=blue)](https://cran.r-project.org/package=classicaltest)-->
<!-- badges: end -->

## Installation

You can install the stable version of `classicaltest` directly from
CRAN:

``` r
install.packages("classicaltest")
```

Or, if you wish to install the development version of `classicaltest`:

``` r
remotes::install_github("dopatendo/classicaltest")
```

## Included data

`classicaltest` includes simulated data for two common cases for CTT:

- `polydata`: 1000 cases with 20 polytomous items.
- `dichodata`: 1000 cases with 20 multiple choice items. Associated with
  their respective keys in `dichokey`.

## Scoring - `correct()`

For multiple choice items we would need to determine if responses were
correct or not. For this we use `correct()`, for which we state the
multiple choice data frame or matrix, the key of correct responses, and
which value should be assigned for omitted responses.

For keeping NAs as NAs:

``` r
data(dichodata)
data(dichokey)


head(dichodata)[,1:5]
#>   item01 item02 item03 item04 item05
#> 1      D      A      B      A      B
#> 2      A      A      A   <NA>      C
#> 3   <NA>      C      A      C      D
#> 4      A      C   <NA>      D      B
#> 5      A      C      B      C      D
#> 6      A      B      C      B      B
corr <- correct(x = dichodata, key = dichokey, navalue = NA)
head(corr)[,1:5]
#>   item01 item02 item03 item04 item05
#> 1      0      0      1      0      0
#> 2      1      0      0     NA      1
#> 3     NA      0      0      0      0
#> 4      1      0     NA      0      0
#> 5      1      0      1      0      0
#> 6      1      0      0      1      0
```

For scoring NAs as 0s:

``` r
head(dichodata)[,1:5]
#>   item01 item02 item03 item04 item05
#> 1      D      A      B      A      B
#> 2      A      A      A   <NA>      C
#> 3   <NA>      C      A      C      D
#> 4      A      C   <NA>      D      B
#> 5      A      C      B      C      D
#> 6      A      B      C      B      B
corr2 <- correct(x = dichodata, key = dichokey, navalue = 0)
head(corr2)[,1:5]
#>   item01 item02 item03 item04 item05
#> 1      0      0      1      0      0
#> 2      1      0      0      0      1
#> 3      0      0      0      0      0
#> 4      1      0      0      0      0
#> 5      1      0      1      0      0
#> 6      1      0      0      1      0
```

## Person statistics - `ctperson()`

Using the corrected data, we can calculate person statistics using
`ctperson()`.

For dichotomous data, this will produce a data frame with the sum
scores, the number of items administered, the number of items answered,
the proportion of correct items, the proportion of correct administered
items, and the proportion of correct answered items.

``` r

res.person <- ctperson(x = corr)
head(res.person)
#>   sum.score it.administered it.answered prcorr.total prcorr.admin prcorr.answr
#> 1        10              20          18         0.50         0.50    0.5555556
#> 2        15              20          19         0.75         0.75    0.7894737
#> 3         9              20          18         0.45         0.45    0.5000000
#> 4         5              20          15         0.25         0.25    0.3333333
#> 5        10              20          19         0.50         0.50    0.5263158
#> 6        12              20          20         0.60         0.60    0.6000000
```

And for polytomous data, the proportion of correct items will be
replaced by the item means:

``` r
data(polydata)
res.person.poly <- ctperson(x = polydata)
head(res.person.poly)
#>   sum.score it.administered it.answered mean.total mean.admin mean.answr
#> 1        46              20          18       2.30       2.30   2.555556
#> 2        53              20          19       2.65       2.65   2.789474
#> 3        47              20          16       2.35       2.35   2.937500
#> 4        35              20          18       1.75       1.75   1.944444
#> 5        28              20          18       1.40       1.40   1.555556
#> 6        40              20          18       2.00       2.00   2.222222
```

Finally, if not all items were administrated to all persons, we can
include this information via the argument `administered`:

``` r
# Random administered matrix
set.seed(1919)
admin <- sample(x = 0:1, size = nrow(corr)*ncol(corr), 
                replace = TRUE, prob = c(.05,.95))
admin <- matrix(data = as.logical(admin),nrow = nrow(corr))
head(admin)[,1:5]
#>      [,1]  [,2]  [,3] [,4] [,5]
#> [1,] TRUE  TRUE  TRUE TRUE TRUE
#> [2,] TRUE  TRUE  TRUE TRUE TRUE
#> [3,] TRUE  TRUE FALSE TRUE TRUE
#> [4,] TRUE  TRUE  TRUE TRUE TRUE
#> [5,] TRUE FALSE  TRUE TRUE TRUE
#> [6,] TRUE  TRUE  TRUE TRUE TRUE

# Person statistics
res.person <- ctperson(x = corr, administered = admin)
head(res.person)
#>   sum.score it.administered it.answered prcorr.total prcorr.admin prcorr.answr
#> 1        10              20          18         0.50    0.5000000    0.5555556
#> 2        14              19          18         0.70    0.7368421    0.7777778
#> 3         9              19          17         0.45    0.4736842    0.5294118
#> 4         5              18          13         0.25    0.2777778    0.3846154
#> 5         9              17          16         0.45    0.5294118    0.5625000
#> 6        12              20          20         0.60    0.6000000    0.6000000
```

## Item analysis - `ctitem()`

For item analysis, we can use `ctitem()`. This function has six
arguments:

- `x`: A data frame or a matrix with unscored data for dichotomous
  items, or numeric data for polytomous items.
- `key` (optional): A vector with the item keys. If `NULL`, polytomous
  items will be assumed.
- `categories` (optional): A vector indicating all possible categories.
  If `NULL`, this vector will be created with all the non-NA values
  present in x. If not `NULL` only included categories will be
  considered as valid answers.
- `wt` (optional): A numeric vector of total weights.
- `listwise`: A logical value indicating if only complete data should be
  considered.

## Readme in progress for

`classicaltest()`, `ctitem()`, `dichotomize()`, and `pointbiserial()`.
