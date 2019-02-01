library(pROC)
data(aSAH)

a.ndka <- auc(aSAH$outcome, aSAH$ndka)

test_that("can convert auc to numeric", {
	expect_is(a.ndka, "auc") # a.ndka is not a numeric to start with
	expect_equal(as.numeric(a.ndka), 0.611957994579946)
})

test_that("can do math on an AUC", {
	expect_equal(sqrt(a.ndka), 0.782277440924859)
	expect_equal(a.ndka * 2, 1.22391598915989)
	expect_equal(a.ndka / 0.5, 1.22391598915989)
	expect_equal(a.ndka + 5, 5.611957994579946)
	expect_equal(a.ndka - 1, -0.388042005420054)
	expect_equal(round(a.ndka, digits=1), 0.6)
})