## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library(broom)
library(tibble)

data(Orange)

Orange <- as_tibble(Orange)
Orange

## ------------------------------------------------------------------------
cor(Orange$age, Orange$circumference)

library(ggplot2)

ggplot(Orange, aes(age, circumference, color = Tree)) +
  geom_line()

## ------------------------------------------------------------------------
library(dplyr)

Orange %>% 
  group_by(Tree) %>%
  summarize(correlation = cor(age, circumference))

## ------------------------------------------------------------------------
ct <- cor.test(Orange$age, Orange$circumference)
ct

## ------------------------------------------------------------------------
tidy(ct)

## ------------------------------------------------------------------------
library(tidyr)
library(purrr)

nested <- Orange %>% 
  nest(-Tree)

## ------------------------------------------------------------------------
nested %>% 
  mutate(test = map(data, ~ cor.test(.x$age, .x$circumference)))

## ------------------------------------------------------------------------
nested %>% 
  mutate(
    test = map(data, ~ cor.test(.x$age, .x$circumference)), # S3 list-col
    tidied = map(test, tidy)
  ) 

## ------------------------------------------------------------------------
Orange %>% 
  nest(-Tree) %>% 
  mutate(
    test = map(data, ~ cor.test(.x$age, .x$circumference)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied, .drop = TRUE)

## ------------------------------------------------------------------------
lm_fit <- lm(age ~ circumference, data = Orange)
summary(lm_fit)

## ------------------------------------------------------------------------
tidy(lm_fit)

## ------------------------------------------------------------------------
Orange %>%
  nest(-Tree) %>% 
  mutate(
    fit = map(data, ~ lm(age ~ circumference, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)

## ------------------------------------------------------------------------
data(mtcars)
mtcars <- as_tibble(mtcars)  # to play nicely with list-cols
mtcars

mtcars %>%
  nest(-am) %>% 
  mutate(
    fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),  # S3 list-col
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)

## ------------------------------------------------------------------------
regressions <- mtcars %>%
  nest(-am) %>% 
  mutate(
    fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

regressions %>% 
  unnest(tidied)

regressions %>% 
  unnest(glanced, .drop = TRUE)

regressions %>% 
  unnest(augmented)

