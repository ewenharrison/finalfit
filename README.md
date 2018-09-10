[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/finalfit)](https://cran.r-project.org/package=finalfit)
[![CRAN_Status_Badge](https://cranlogs.r-pkg.org/badges/finalfit)](https://cran.r-project.org/package=finalfit)
[![TravisCRAN_Status_Badge](https://travis-ci.com/ewenharrison/finalfit.svg?branch=master)](https://travis-ci.com/ewenharrison/finalfit)


finalfit <img src="man/figures/finalfit_130_150.png" align="right" />
==============================


The `finalfit` package provides functions that help you quickly create elegant final results tables and plots when modelling in R. These can easily be exported as Word documents, PDFs, or html files. 

Its design follows Hadley Wickham's [tidy tool manifesto](http://tidyverse.tidyverse.org/articles/manifesto.html).

In addition, it provides functions for identifying and handling missing data, together with a number of functions to bootstrap simulate regression model results. 

## Installation and Documentation

You can install `finalfit` from CRAN or github with:

``` r
install.packages("finalfit")
# install.packages("devtools")
devtools::install_github("ewenharrison/finalfit")
```

It is recommended that this package is used together with `dplyr` which can be installed via:

``` r
install.packages("dplyr")
```

## Vignettes

[Finalfit basics](https://CRAN.R-project.org/package=finalfit/vignettes/finalfit_basics.html)

[Bootstrapping for model prediction](http://www.datasurg.net/2018/07/12/finalfit-now-includes-bootstrap-simulation-for-model-prediction/)

[Exporting results to Word, PDF and html with R Markdown](http://www.datasurg.net/2018/05/22/finalfit-knitr-and-r-markdown-for-quick-results/)
