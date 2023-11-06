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

## ---- eval=FALSE---------------------------------------------------------
#  colon_s %>%
#    ff_glimpse()

## ---- eval=FALSE---------------------------------------------------------
#  library(dplyr)
#  colon_s %>%
#    select(-hospital) %>%
#    ff_glimpse()

## ---- eval=FALSE---------------------------------------------------------
#  colon_s %>%
#    missing_plot()

## ------------------------------------------------------------------------
explanatory = c("age", "sex.factor", 
  "obstruct.factor",  
  "smoking_mcar", "smoking_mar")
dependent = "mort_5yr"

colon_s %>% 
  missing_pattern(dependent, explanatory)

## ------------------------------------------------------------------------
# Explanatory or confounding variables
explanatory = c("age", "sex.factor", 
  "nodes",  
  "smoking_mcar", "smoking_mar")

# Explanatory variable of interest
dependent = "obstruct.factor" # Bowel obstruction

colon_s %>% 
  summary_factorlist(dependent, explanatory, 
  na_include=TRUE, p=TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  explanatory = c("age", "sex.factor",
#    "nodes", "obstruct.factor",
#    "smoking_mcar", "smoking_mar")
#  dependent = "mort_5yr"
#  colon_s %>%
#    missing_pairs(dependent, explanatory)

## ---- eval=FALSE---------------------------------------------------------
#  colon_s %>%
#    missing_pairs(dependent, explanatory, position = "fill", )

## ------------------------------------------------------------------------
explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor")
dependent = "smoking_mcar"
colon_s %>% 
  missing_compare(dependent, explanatory) %>% 
	knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r")) # Omit when you run

## ------------------------------------------------------------------------
dependent = "smoking_mar"
colon_s %>% 
  missing_compare(dependent, explanatory) %>% 
	knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r")) # Omit when you run

## ---- eval=FALSE---------------------------------------------------------
#  library(finalfit)
#  library(dplyr)
#  library(MissMech)
#  explanatory = c("age", "nodes")
#  dependent = "mort_5yr"
#  
#  colon_s %>%
#    select(explanatory) %>%
#    MissMech::TestMCARNormality()

## ------------------------------------------------------------------------
explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor",  
  "smoking_mcar")
dependent = "mort_5yr"
colon_s %>% 
	finalfit(dependent, explanatory) %>% 
	knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r")) # Omit when you run

## ---- warning=FALSE, message=FALSE---------------------------------------
# Multivariate Imputation by Chained Equations (mice)
library(finalfit)
library(dplyr)
library(mice)
explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor", "smoking_mar")
dependent = "mort_5yr"

# Choose not to impute missing values
# for explanatory variable of interest and
# outcome variable. 
# But include in algorithm for imputation.
colon_s %>% 
	select(dependent, explanatory) %>% 
	missing_predictorMatrix(
		drop_from_imputed = c("obstruct.factor", "mort_5yr")
		) -> predM

colon_s %>% 
  select(dependent, explanatory) %>% 
  # Usually run imputation with 10 imputed sets, 4 here for demonstration
  mice(m = 4, predictorMatrix = predM) %>% 
  # Run logistic regression on each imputed set
  with(glm(formula(ff_formula(dependent, explanatory)), 
    family="binomial")) %>%
  # Pool and summarise results
  pool() %>%                                            
  summary(conf.int = TRUE, exponentiate = TRUE) %>%
  # Jiggle into finalfit format
  mutate(explanatory_name = rownames(.)) %>%            
  select(explanatory_name, estimate, `2.5 %`, `97.5 %`, p.value) %>% 
  condense_fit(estimate_suffix = " (multiple imputation)") %>% 
  remove_intercept() -> fit_imputed

# Use finalfit merge methods to create and compare results
colon_s %>% 
  summary_factorlist(dependent, explanatory, fit_id = TRUE) -> summary1

colon_s %>% 
  glmuni(dependent, explanatory) %>% 
  fit2df(estimate_suffix = " (univariable)") -> fit_uni

colon_s %>% 
  glmmulti(dependent, explanatory) %>% 
  fit2df(estimate_suffix = " (multivariable inc. smoking)") -> fit_multi

explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor")
colon_s %>% 
  glmmulti(dependent, explanatory) %>% 
  fit2df(estimate_suffix = " (multivariable)") -> fit_multi_r

# Combine to final table
summary1 %>% 
  ff_merge(fit_uni) %>% 
  ff_merge(fit_multi_r) %>% 
  ff_merge(fit_multi) %>% 
  ff_merge(fit_imputed) %>% 
  select(-fit_id, -index) %>% 
	knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r","r", "r", "r"))

## ---- warning=FALSE, message=FALSE---------------------------------------
library(dplyr)
colon_s %>% 
  mutate(
    smoking_mar = forcats::fct_na_value_to_level(smoking_mar, level = "(Missing)")
  ) %>% 
  finalfit(dependent, explanatory) %>% 
	knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))


