library(pROC)

test_that("roc rejects rejects invalid data", {
	# Control always negative
	controls <- c(-Inf, 1,2,3,4,5)
	cases <- c(2,3,4,5,6)
	expect_warning(r <- roc(controls = controls, cases = cases), "Infinite value")
	expect_equal(r, NaN)
	
	# Control always positive
	# 100% specificity impossible
	controls <- c(1,2,3,4,5, Inf)
	cases <- c(2,3,4,5,6)
	expect_warning(r <- roc(controls = controls, cases = cases), "Infinite value")
	expect_equal(r, NaN)
})

test_that("roc rejects rejects also valid data", {
	# OK
	controls <- c(1,2,3,4,5)
	cases <- c(-Inf, 2,3,4,5,6)
	expect_warning(r <- roc(controls = controls, cases = cases), "Infinite value")
	expect_equal(r, NaN)
	
	# OK
	controls <- c(1,2,3,4,Inf)
	cases <- c(2,3,4,5,6)
	expect_warning(r <- roc(controls = controls, cases = cases), "Infinite value")
	expect_equal(r, NaN)
})