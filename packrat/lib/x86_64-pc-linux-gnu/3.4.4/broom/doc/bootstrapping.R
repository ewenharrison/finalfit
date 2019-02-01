## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE
)

## ------------------------------------------------------------------------
library(ggplot2)

ggplot(mtcars, aes(mpg, wt)) + 
    geom_point()

## ------------------------------------------------------------------------
nlsfit <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
summary(nlsfit)

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_line(aes(y = predict(nlsfit)))

## ------------------------------------------------------------------------
library(dplyr)
library(rsample)
library(broom)
library(purrr)

set.seed(27)

boots <- bootstraps(mtcars, times = 100)
boots

## ------------------------------------------------------------------------
fit_nls_on_bootstrap <- function(split) {
    nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
}

boot_models <- boots %>% 
    mutate(model = map(splits, fit_nls_on_bootstrap),
           coef_info = map(model, tidy))

boot_coefs <- boot_models %>% 
    unnest(coef_info)

## ------------------------------------------------------------------------
boot_coefs

## ------------------------------------------------------------------------
alpha <- .05
boot_coefs %>% 
    group_by(term) %>%
    summarize(low = quantile(estimate, alpha / 2),
              high = quantile(estimate, 1 - alpha / 2))

## ------------------------------------------------------------------------
ggplot(boot_coefs, aes(estimate)) + 
    geom_histogram(binwidth = 2) + 
    facet_wrap(~ term, scales = "free")

## ------------------------------------------------------------------------
boot_aug <- boot_models %>% 
    mutate(augmented = map(model, augment)) %>% 
    unnest(augmented)

boot_aug

## ------------------------------------------------------------------------
ggplot(boot_aug, aes(wt, mpg)) +
    geom_point() +
    geom_line(aes(y = .fitted, group = id), alpha=.2)

## ------------------------------------------------------------------------
fit_spline_on_bootstrap <- function(split) {
    data <- analysis(split)
    smooth.spline(data$wt, data$mpg, df = 4)
}

boot_splines <- boots %>% 
    mutate(spline = map(splits, fit_spline_on_bootstrap),
           aug_train = map(spline, augment))

splines_aug <- boot_splines %>% 
    unnest(aug_train)

ggplot(splines_aug, aes(x, y)) +
    geom_point() +
    geom_line(aes(y = .fitted, group = id), alpha = 0.2)

