
# D. Wang, Murphy M,. Estimating Optimal Transformations for Multiple
# Regression Using the ACE Algorithm.
# Journal of Data Science 2(2004), 329-346.
test_that("Estimates Multiple Transformations",
{
  set.seed(1) # For repeatability
  
  x  <- matrix(runif(500)*2 - 1, ncol=5)
  e  <- rnorm(100)
  
  y  <- log(4 + sin(4*x[,1]) + abs(x[,2]) + x[,3]^2 + + x[,4]^3 + x[,5] + 0.1*e)
  
  model <- ace(x, y)
  
  # Linear offset were computed using lm
  expect_true(max(sin(4*x[,1]) - model$tx[,1] - 0.003874) < 0.1)
  expect_true(max(abs(x[,2])   - model$tx[,2] - 0.481000) < 0.1)
  expect_true(max(x[,3]^2      - model$tx[,3] - 0.321443) < 0.1)
  expect_true(max(x[,4]^3      - model$tx[,4] - 0.039418) < 0.12)
  expect_true(max(x[,5]        - model$tx[,5] - 0.008231) < 0.1)
})
