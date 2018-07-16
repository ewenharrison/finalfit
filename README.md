[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/finalfit)](https://cran.r-project.org/package=finalfit)
[![CRAN_Status_Badge](https://cranlogs.r-pkg.org/badges/finalfit](https://cran.r-project.org/package=finalfit)


finalfit
==========

The `finalfit` package provides functions that help you create elegant final results tables and charts when modelling. 
Its design follows Hadley Wickham's [tidy tool manifesto](http://tidyverse.tidyverse.org/articles/manifesto.html).

Installation and Documentation
------------------------------

You can install `finalfit` from github with:

``` r
# install.packages("devtools")
devtools::install_github("ewenharrison/finalfit")
```

It is recommended that this package is used together with `dplyr` which can be installed via:

``` r
install.packages("dplyr")
```

Main Features
-------------

### 1. Summarise variables/factors by a categorical variable

`summary_factorlist()` is a simple wrapper used to summarise any number of variables by a single categorical variable. 
This is usually "Table 1" of a study report. 

``` r
library(finalfit)
library(dplyr)

# Load example dataset, modified version of survival::colon
data(colon_s)

# Table 1 - Patient demographics ----
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary_factorlist(dependent, explanatory, p=TRUE)

```

`summary_factorlist()` is also commonly used to summarise any number of variables by an *outcome variable* (say dead yes/no).  

``` r
# Table 2 - 5 yr mortality ----
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  summary_factorlist(dependent, explanatory)
```

### 2. Summarise regression model results in final table format

The second main feature is the ability to create final tables for lineary `lm()`, logistic `glm()`, hierarchical logistic `lme4::glmer()` and Cox proprotional hazard `survival::coxph()` regression models.

The `finalfit()` "all-in-one" function takes a single dependent variable with a vector of explanatory variable names 
(continuous or categorical variables) to produce a final table for publication including summary statistics, 
univariable and multivariable regression analyses. The first columns are those produced by 
`summary_factorist()`. 

`glm(depdendent ~ explanatory, family="binomial")`

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory)
```

Where a multivariable model contains a subset of the variables specified in the full univariable set, this can be specified. 

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory, explanatory_multi)
```

Random effects

`lme4::glmer(dependent ~ explanatory + (1 | random_effect), family="binomial")`

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
random_effect = "hospital"
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory, explanatory_multi, random_effect)
```

`metrics=TRUE` provides common model metrics. 

``` r
colon_s %>%
  finalfit(dependent, explanatory, explanatory_multi,  metrics=TRUE)
```

Cox proportional hazards 

`survival::coxph(dependent ~ explanatory)`

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"

colon_s %>% 
	finalfit(dependent, explanatory)
```

Rather than going all-in-one, any number of subset models can be manually added on to a `summary_factorlist()` table using `finalfit_merge()`. This is particularly useful when models take a long-time to run or are complicated. 

Note requirement for `fit_id=TRUE`. `fit2df` is a function extracting most common models to a dataframe. 


``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
random_effect = "hospital"
dependent = 'mort_5yr'

# Separate tables
colon_s %>%
  summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example.summary

colon_s %>%
  glmuni(dependent, explanatory) %>%
  fit2df(estimate_suffix=" (univariable)") -> example.univariable

colon_s %>%
  glmmulti(dependent, explanatory) %>%
  fit2df(estimate_suffix=" (multivariable)") -> example.multivariable


colon_s %>%
  glmmixed(dependent, explanatory, random_effect) %>%
  fit2df(estimate_suffix=" (multilevel)") -> example.multilevel

# Pipe together
example.summary %>% 
  finalfit_merge(example.univariable) %>% 
  finalfit_merge(example.multivariable) %>% 
  finalfit_merge(example.multilevel) %>% 
  select(-c(fit_id, index)) -> example.final
example.final

```

Cox Proportional Hazards example with separate tables merged together.

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
dependent = "Surv(time, status)"

# Separate tables
colon_s %>%
	summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example2.summary

colon_s %>%
	coxphuni(dependent, explanatory) %>%
	fit2df(estimate_suffix=" (univariable)") -> example2.univariable

colon_s %>%
  coxphmulti(dependent, explanatory_multi) %>%
  fit2df(estimate_suffix=" (multivariable)") -> example2.multivariable

# Pipe together
example2.summary %>% 
	finalfit_merge(example2.univariable) %>% 
	finalfit_merge(example2.multivariable) %>% 
	select(-c(fit_id, index)) -> example2.final
example2.final
```

### 3. Summarise regression model results in plot

Models can be summarized with odds ratio/hazard ratio plots using `or_plot` or `hr_plot`. 

``` r
# OR plot
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory)
# Previously fitted models (`glmmulti`) can be provided directly to `glmfit`  
  
# HR plot
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
  hr_plot(dependent, explanatory, dependent_label = "Survival")
# Previously fitted models (`coxphmulti`) can be provided directly using `coxfit`
```

Our own particular `Rstan` models are supported and will be documented in the future. Broadly, if you are running (hierarchical) logistic regression models in [Stan](http://mc-stan.org/users/interfaces/rstan) with coefficients specified as a vector labelled `beta`, then `fit2df()` will work directly on the `stanfit` object in a similar manner to if it was a `glm` or `glmerMod` object. 

### Notes

Use `Hmisc::label()` to assign labels to variables for tables and plots.

``` r
label(colon_s$age.factor) = "Age (years)"
```

Export dataframe tables directly or to [R Markdown](http://rmarkdown.rstudio.com) using [`knitr::kable()`](https://yihui.name/knitr/).

Note wrapper `finalfit_missing()` can be useful. Wraps `mice::md.pattern`.

``` r
colon_s %>%
  finalfit_missing(dependent, explanatory)
```
