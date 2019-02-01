library(pROC)

context("large data sets")

test_that("roc can deal with 1E5 data points and many thresholds", {
  response <- rbinom(1E5, 1, .5)
  predictor <- rnorm(1E5)
  # ~ 0.6s
  r <- roc(response, predictor)
  ci(r)
  auc(r, partial.auc = c(0.9, 1))
})

test_that("roc can deal with 1E6 data points and few thresholds", {
  response <- rbinom(1E6, 1, .5)
  predictor <- rpois(1E6, 1)
  # ~ 0.3s
  r <- roc(response, predictor)
  ci(r)
  auc(r, partial.auc = c(0.9, 1))
})

test_that("roc can deal with 1E7 data points and few thresholds", {
  skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
  response <- rbinom(1E7, 1, .5)
  predictor <- rpois(1E7, 1)
  # ~ 3s
  r <- roc(response, predictor)
  ci(r)
  auc(r, partial.auc = c(0.9, 1))
})
