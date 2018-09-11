## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("finalfit")

## ---- warning=FALSE, message=FALSE---------------------------------------
library(finalfit)
library(dplyr)

dependent = "differ.factor"

# Specify explanatory variables of interest
explanatory = c("age", "sex.factor", 
  "extent.factor", "obstruct.factor", 
  "nodes")

## ---- warning=FALSE, message=FALSE---------------------------------------
colon_s %>% 
  select(age, sex.factor, extent.factor, obstruct.factor, nodes) %>% 
  names() -> explanatory

## ---- warning=FALSE, message=FALSE---------------------------------------
colon_s %>% 
  ff_glimpse(dependent, explanatory)

## ---- eval=FALSE---------------------------------------------------------
#  colon_s %>%
#    summary_factorlist(dependent, explanatory,
#    p=TRUE, na_include=TRUE)

## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
colon_s %>% 
  summary_factorlist(dependent, explanatory, 
  p=TRUE, na_include=TRUE) %>% 
  knitr::kable(row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

## ---- eval=FALSE---------------------------------------------------------
#  Hmisc::label(colon_s$nodes) = "Lymph nodes involved"
#  explanatory = c("age", "sex.factor",
#    "extent.factor", "nodes")
#  
#  colon_s %>%
#    summary_factorlist(dependent, explanatory,
#    p=TRUE, na_include=TRUE,
#    add_dependent_label=TRUE) -> table1
#  table1

## ---- warning=FALSE, message=FALSE, cache=TRUE, echo=FALSE---------------
Hmisc::label(colon_s$nodes) = "Lymph nodes involved"
explanatory = c("age", "sex.factor", 
  "extent.factor", "nodes")

colon_s %>% 
  summary_factorlist(dependent, explanatory, 
  p=TRUE, na_include=TRUE, 
  add_dependent_label=TRUE) %>% 
	knitr::kable(row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

## ---- eval=FALSE---------------------------------------------------------
#  explanatory = c("age", "sex.factor",
#    "extent.factor", "nodes", "differ.factor")
#  dependent = "mort_5yr"
#  colon_s %>%
#    finalfit(dependent, explanatory,
#    dependent_label_prefix = "") -> table2
#  table2

## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
explanatory = c("age", "sex.factor", 
  "extent.factor", "nodes", "differ.factor")
dependent = "mort_5yr"
colon_s %>% 
  finalfit(dependent, explanatory, 
  dependent_label_prefix = "") %>% 
	knitr::kable(row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  colon_s %>%
#    or_plot(dependent, explanatory,
#    breaks = c(0.5, 1, 5, 10, 20, 30))

## ---- eval=FALSE---------------------------------------------------------
#  # Save objects for knitr/markdown
#  save(table1, table2, dependent, explanatory, file = "out.rda")

