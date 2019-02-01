## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(broom)
library(tibble)

## ---- eval = FALSE-------------------------------------------------------
#  td <- tidy(object)
#  gl <- glance(object)
#  
#  check_tidy_output(td)
#  check_glance_outputs(gl)

## ------------------------------------------------------------------------
model <- lm(speed ~ dist, data = cars)
augment(model, data = cars)
augment(model, newdata = cars)

## ----eval = FALSE--------------------------------------------------------
#  model <- mixed_model(...)
#  tidy(model, effects = "fixed")
#  tidy(model, effects = "random")

