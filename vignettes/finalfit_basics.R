## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("ewenharrison/finalfit", ref = "development")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("rstan")
#  install.packages("boot")

## ---- warning=FALSE, message=FALSE---------------------------------------
library(finalfit)
library(dplyr)

# Load example dataset, modified version of survival::colon
data(colon_s)

# Table 1 - Patient demographics by variable of interest ----
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor" # Bowel perforation
colon_s %>%
  summary_factorlist(dependent, explanatory,
  p=TRUE, add_dependent_label=TRUE) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r"))

## ---- warning=FALSE, message=FALSE---------------------------------------
# Table 2 - 5 yr mortality ----
explanatory = c("age.factor", "sex.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s %>%
  summary_factorlist(dependent, explanatory, 
  p=TRUE, add_dependent_label=TRUE) -> t2
knitr::kable(t2, row.names=FALSE, align=c("l", "l", "r", "r", "r"))

## ---- warning=FALSE, message=FALSE---------------------------------------
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory) -> t3
knitr::kable(t3, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

## ---- warning=FALSE, message=FALSE---------------------------------------
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory, explanatory_multi) -> t4
knitr::kable(t4, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

## ---- warning=FALSE, message=FALSE---------------------------------------
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
random_effect = "hospital"
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory, explanatory_multi, random_effect) -> t5
knitr::kable(t5, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

## ---- warning=FALSE, message=FALSE---------------------------------------
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
  finalfit(dependent, explanatory) -> t6
knitr::kable(t6, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

## ---- warning=FALSE, message=FALSE---------------------------------------
explanatory = c("age.factor", "sex.factor", 
  "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory, metrics=TRUE) -> t7
knitr::kable(t7[[1]], row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))
knitr::kable(t7[[2]], row.names=FALSE, col.names="")

## ---- warning=FALSE, message=FALSE---------------------------------------
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
random_effect = "hospital"
dependent = 'mort_5yr'

# Separate tables
colon_s %>%
  summary_factorlist(dependent, 
  explanatory, fit_id=TRUE) -> example.summary

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
  select(-c(fit_id, index)) %>% # remove unnecessary columns
  dependent_label(colon_s, dependent, prefix="") -> t8 # place dependent variable label
knitr::kable(t8, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r", "r"))

## ---- eval=FALSE---------------------------------------------------------
#  explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#  dependent = 'mort_5yr'
#  colon_s %>%
#    or_plot(dependent, explanatory)
#  # Previously fitted models (`glmmulti()` or # `glmmixed()`) can be provided directly to `glmfit`

## ---- eval=FALSE---------------------------------------------------------
#  explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#  dependent = "Surv(time, status)"
#  colon_s %>%
#    hr_plot(dependent, explanatory, dependent_label = "Survival")
#  # Previously fitted models (`coxphmulti`) can be provided directly using `coxfit`

## ---- eval=FALSE---------------------------------------------------------
#  explanatory = c("perfor.factor")
#  dependent = "Surv(time, status)"
#  colon_s %>%
#    surv_plot(dependent, explanatory,
#    xlab="Time (days)", pval=TRUE, legend="none")

## ---- eval=FALSE---------------------------------------------------------
#  label(colon_s$age.factor) = "Age (years)"

## ---- warning=FALSE, message=FALSE---------------------------------------
colon_s %>%
  finalfit_missing(dependent, explanatory)

