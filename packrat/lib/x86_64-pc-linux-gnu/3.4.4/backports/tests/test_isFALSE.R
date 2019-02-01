source("helper/helper.R")


f = backports:::isFALSE
expect_identical(f(FALSE), TRUE)
expect_identical(f(TRUE), FALSE)
expect_identical(f(1), FALSE)
expect_identical(f(iris), FALSE)
