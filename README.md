summarizer
==========

The `summarizer` package provides functions that help you create elegant final results tables and charts when modelling. 
Its design follows Hadley Wickham's [tidy tool manifesto](http://tidyverse.tidyverse.org/articles/manifesto.html).

Installation and Documentation
------------------------------

You can install `summarizer` from github with:

``` r
# install.packages("devtools")
devtools::install_github("ewenharrison/summarizer")
```

It is not a dependent, but it is recommended that this package is used together with 
`dplyr` which can be installed via:

``` r
install.packages("dplyr")
```

Main Features
-------------

### 1. Summarise variables/factors by a categorical variable

`summary.factorlist()` is a simple wrapper used to summarise any number of variables by a single categorical variable. 
This is usually "Table 1" of a study report. 

``` r
library(summarizer)
library(dplyr)

# Load example dataset, modified version of survival::colon
data(colon_s)

# Table 1 - Patient demographics ----
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary.factorlist(dependent, explanatory, p=T)

```

`summary.factorlist()` is also commonly used to summarise any number of variables by an *outcome variable* (say dead yes/no).  

``` r
# Table 2 - 5 yr mortality ----
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  summary.factorlist(dependent, explanatory)
```

### 2. Summarise regression model results in final table format

The second main feature is the ability to create final tables for logistic `glm()`, hierarchical logistic `lme4::glmer()` and 
Cox proprotional hazard `survival::coxph()` regression models.

The `summarizer()` "all-in-one" function takes a single dependent variable with a vector of explanatory variable names 
(continuous or categorical variables) to produce a final table for publication including summary statistics, 
univariable and multivariable regression analyses. The first columns are those produced by 
`summary.factorist()`. 

`glm(depdendent ~ explanatory, family="binomial")`

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory)
```

Where a multivariable model contains a subset of the variables specified in the full univariable set, this can be specified. 

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi)
```

Random effects. 

`lme4::glmer(dependent ~ explanatory + (1 | random_effect), family="binomial")`

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi, random.effect)
```

`metrics=TRUE` provides common model metrics. 

``` r
colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi,  metrics=TRUE)
```

Cox proportional hazards 

`survival::coxph(dependent ~ explanatory)`

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"

colon_s %>% 
	summarizer(dependent, explanatory)
```

Rather than going all-in-one, any number of subset models can be manually added on to a `summary.factorlist()` table using `summarizer.merge()`. This is particularly useful when models take a long-time to run or are complicated. 

Note requirement for `glm.id=TRUE`. `fit2df` is a subfunction extracting most common models to a dataframe. 


``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'

# Separate tables
colon_s %>%
  summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example.summary

colon_s %>%
  glmuni(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (univariable)") -> example.univariable

colon_s %>%
  glmmulti(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (multivariable)") -> example.multivariable


colon_s %>%
  glmmixed(dependent, explanatory, random.effect) %>%
  fit2df(estimate.suffix=" (multilevel") -> example.multilevel

# Pipe together
example.summary %>% 
  summarizer.merge(example.univariable) %>% 
  summarizer.merge(example.multivariable) %>% 
  summarizer.merge(example.multilevel) %>% 
  select(-c(glm.id, index)) -> example.final
example.final

```

Cox Proportional Hazards example with separate tables merged together.

``` r
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
dependent = "Surv(time, status)"

# Separate tables
colon_s %>%
	summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example2.summary

colon_s %>%
	coxphuni(dependent, explanatory) %>%
	fit2df(estimate.suffix=" (univariable)") -> example2.univariable

colon_s %>%
  coxphmulti(dependent, explanatory.multi) %>%
  fit2df(estimate.suffix=" (multivariable)") -> example2.multivariable

# Pipe together
example2.summary %>% 
	summarizer.merge(example2.univariable) %>% 
	summarizer.merge(example2.multivariable) %>% 
	select(-c(glm.id, index)) -> example2.final
example2.final
```

### 3. Summarise regression model results in plot

Models can be summarized with odds ratio/hazard ratio plots using `or.plot` or `hr.plot` (hr.plot not fully tested). 

``` r
# OR plot
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or.plot(dependent, explanatory)
# Previously fitted models (`glmmulti`) can be provided directly to `glmfit`  
  
# HR plot (not fully tested)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
  hr.plot(dependent, explanatory, dependent_label = "Survival")
# Previously fitted models (`coxphmulti`) can be provided directly using `coxfit`
```

Our own particular `Rstan` models are supported and will be documented in the future. Broadly, if you are running (hierarchical) logistic regression models in [Stan](http://mc-stan.org/users/interfaces/rstan) with coefficients specified as a vector labelled `beta`, then `fit2df()` will work directly on the `stanfit` object in a similar manner to if it was a `glm` or `glmerMod` object. 

### Notes

Use `Hmisc::label()` to assign labels to variables for tables and plots.

``` r
label(colon_s$age.factor) = "Age (years)"
```

Export dataframe tables directly or to [R Markdown](http://rmarkdown.rstudio.com) using [`knitr::kable()`](https://yihui.name/knitr/).

Note wrapper `summary.missing()` can be useful. Wraps `mice::md.pattern`.

``` r
colon_s %>%
  summary.missing(dependent, explanatory)
```
