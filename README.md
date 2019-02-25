[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/finalfit)](https://cran.r-project.org/package=finalfit)
[![CRAN_Status_Badge](https://cranlogs.r-pkg.org/badges/finalfit)](https://cran.r-project.org/package=finalfit)
[![TravisCRAN_Status_Badge](https://travis-ci.com/ewenharrison/finalfit.svg?branch=master)](https://travis-ci.com/ewenharrison/finalfit)
[![Build status](https://ci.appveyor.com/api/projects/status/3wpgw2rs6vn1lsrn?svg=true)](https://ci.appveyor.com/project/ewenharrison/finalfit)
[![Coverage status](https://codecov.io/gh/ewenharrison/finalfit/branch/master/graph/badge.svg)](https://codecov.io/github/ewenharrison/finalfit?branch=master)

finalfit <img src="man/figures/finalfit_130_150.png" align="right" />
==============================


The `finalfit` package provides functions that help you quickly create elegant final results tables and plots when modelling in R. These can easily be exported as Word documents, PDFs, or html files. 

Its design follows Hadley Wickham's [tidy tool manifesto](http://tidyverse.tidyverse.org/articles/manifesto.html).

In addition, it provides functions for identifying and handling missing data, together with a number of functions to bootstrap simulate regression model results. 

## Installation

You can install `finalfit` from CRAN:

``` r
install.packages("finalfit")
```

It is recommended that this package is used together with `dplyr` which can be installed via:

``` r
install.packages("dplyr")
```

## Documentation

The package documentation is maintained independently at [finalfit.org](http://finalfit.org/). 

## Example

See getting started and the "All tables" vignette for extensive examples.  

### Crosstable / table 1

``` r
# Crosstable 
explanatory = c("age.factor", "sex.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s %>%
  summary_factorlist(dependent, explanatory, 
  p=TRUE, add_dependent_label=TRUE) -> t2
knitr::kable(t2, row.names=FALSE, align=c("l", "l", "r", "r", "r"))
```

<a href="https://www.datasurg.net/wp-content/uploads/2018/05/table2.jpg"><img src="https://www.datasurg.net/wp-content/uploads/2018/05/table2.jpg" alt="" width="600" class="aligncenter" /></a>

### Regression table

``` r
explanatory = c("age.factor", "sex.factor", 
  "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory, metrics=TRUE) -> t7
knitr::kable(t7[[1]], row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))
knitr::kable(t7[[2]], row.names=FALSE, col.names="")
```

When exported to PDF:

<a href="https://www.datasurg.net/wp-content/uploads/2018/05/table7a.jpg"><img src="https://www.datasurg.net/wp-content/uploads/2018/05/table7a.jpg" alt="" width="700" /></a>

<a href="https://www.datasurg.net/wp-content/uploads/2018/05/table7b.jpg"><img src="https://www.datasurg.net/wp-content/uploads/2018/05/table7b.jpg" alt="" width="700"/></a>