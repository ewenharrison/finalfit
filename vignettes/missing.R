## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  # Make sure finalfit is up-to-date
#  install.packages("finalfit")

## ------------------------------------------------------------------------
library(finalfit) 

# Create some extra missing data
## Smoking missing completely at random
set.seed(1)
colon_s$smoking_mcar = 
  sample(c("Smoker", "Non-smoker", NA), 
    dim(colon_s)[1], replace=TRUE, 
    prob = c(0.2, 0.7, 0.1)) %>% 
  factor()
Hmisc::label(colon_s$smoking_mcar) = "Smoking (MCAR)"

## Smoking missing conditional on patient sex
colon_s$smoking_mar[colon_s$sex.factor == "Female"] = 
  sample(c("Smoker", "Non-smoker", NA), 
    sum(colon_s$sex.factor == "Female"), 
    replace = TRUE,
    prob = c(0.1, 0.5, 0.4))

colon_s$smoking_mar[colon_s$sex.factor == "Male"] = 
  sample(c("Smoker", "Non-smoker", NA), 
    sum(colon_s$sex.factor == "Male"), 
    replace=TRUE, prob = c(0.15, 0.75, 0.1))
colon_s$smoking_mar = factor(colon_s$smoking_mar)
Hmisc::label(colon_s$smoking_mar) = "Smoking (MAR)"

# Examine with ff_glimpse
explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor",  
  "smoking_mcar", "smoking_mar")
dependent = "mort_5yr"

colon_s %>% 
  ff_glimpse(dependent, explanatory)

