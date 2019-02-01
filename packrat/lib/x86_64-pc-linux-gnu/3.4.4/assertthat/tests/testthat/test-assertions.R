context("Assertion assertions")

test_that("is.integerish works correctly", {
  expect_true(is.integerish(1L))
  expect_true(is.integerish(c(1L, 2L, 3L)))
  expect_true(is.integerish(c(1L, NA, 3L)))
  expect_false(is.integerish(c(1L, 2.1, 3L)))
  
  # base::.Machine holds info on machine numerical precision
  expect_false(is.integerish(1L + .Machine$double.eps))
  expect_false(is.integerish(1L - .Machine$double.neg.eps))
  
  expect_false(is.integerish(NA))
  expect_false(is.integerish(NULL))
})

test_that("is.named works correctly", {
  expect_false(is.named(1))
  x <- 1:3
  expect_false(is.named(x))
  names(x) <- letters[1:3]
  expect_true(is.named(x))
  
  # Malformed or weird names
  names(x)[2] <- ""
  expect_false(is.named(x))
  names(x)[2] <- NA
  expect_false(is.named(x))
  names(x) <- NULL
  expect_false(is.named(x))
  
  expect_false(is.named(NA))
  expect_false(is.named(NULL))
})

test_that("has_attr works correctly", {
  x <- 1:3
  expect_false(has_attr(x, "names"))
  names(x) <- letters[1:3]
  expect_true(has_attr(x, "names"))
  expect_false(has_attr(x, "something else"))
  # not sure what else to test here
})

test_that("has_name works correctly", {
  x <- 1:3
  expect_false(has_name(x, "a"))
  names(x) <- letters[1:3]
  expect_true(has_name(x, letters[2]))
  expect_false(has_name(x, "something else"))
  expect_false(has_name(x, NA))
})

test_that("noNA works correctly", {
  expect_true(noNA("a"))
  expect_false(noNA(c(TRUE, NA)))
  x <- sample(c(1:10, NA), 100, rep = TRUE)
  expect_false(noNA(x))
  expect_true(noNA(1:1000))
})

test_that("are_equal works correctly", {
  x <- 2
  expect_false(are_equal(x, 1.9))
  expect_true(are_equal(x, 1.999, tol = 0.01))
  expect_true(are_equal(x, 2))
  expect_true(are_equal('a', 'a'))
  expect_false(are_equal('a', 'b'))
  
  expect_true(are_equal(NA, NA))
  expect_true(are_equal(NULL, NULL))
})

test_that("is.error works correctly", {
  x <- try(stop("!!"), silent=TRUE)
  expect_true(is.error(x))
  expect_false(is.error(1))
  
  expect_false(is.error(NA))  
  expect_false(is.error(NULL))
})

test_that("is.time works correctly", {
  expect_true(is.time(Sys.time()))
  expect_false(is.time(Sys.Date()))
  expect_false(is.time(1))
  
  expect_false(is.time(NA))  
  expect_false(is.time(NULL))
})

test_that("is.date works correctly", {
  expect_false(is.date(Sys.time()))
  expect_true(is.date(Sys.Date()))
  expect_false(is.date(1))
  
  expect_false(is.date(NA))  
  expect_false(is.date(NULL))
})

test_that("has_args works correctly", {
  expect_error(1 %has_args% "x")
  expect_true(mean %has_args% "x")
  expect_false(mean %has_args% "y")
  
  expect_error(NA %has_args% "x")
  expect_error(NULL %has_args% "x")  
})

test_that("not_empty works correctly", {
  expect_true(not_empty(1))
  expect_false(not_empty(numeric()))
  expect_false(not_empty(mtcars[0, ]))
  expect_false(not_empty(mtcars[, 0]))

  expect_true(not_empty(NA))
  expect_false(not_empty(NULL))
})
