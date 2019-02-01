library(pROC)
data(aSAH)

context("are.paired")

test_that("are.paired works", {
	# most basic example
	expect_true(are.paired(r.wfns, r.ndka))
	
	# Missing values shouldn't screw up
	aSAH.missing <- aSAH
	aSAH.missing$wfns[1:20] <- NA
	expect_true(are.paired(roc(aSAH.missing$outcome, aSAH.missing$wfns), roc(aSAH.missing$outcome, aSAH.missing$ndka)))
	# Also with different data.frames
	expect_true(are.paired(roc(aSAH.missing$outcome, aSAH.missing$wfns), r.ndka))
	
	# The following should fail though
	expect_false(are.paired(roc(aSAH$outcome[21:113], aSAH$wfns[21:113]), roc(aSAH$outcome, aSAH$ndka)))
	
	# Opposite levels should probably fail
	expect_false(are.paired(roc(aSAH$outcome, aSAH$wfns, levels = c("Good", "Poor")), roc(aSAH$outcome, aSAH$ndka, levels = c("Poor", "Good"))))
})

test_that("are.paired works with auc and mixed roc", {
	expect_true(are.paired(auc(aSAH$outcome, aSAH$wfns), auc(aSAH$outcome, aSAH$ndka)))
	expect_true(are.paired(roc(aSAH$outcome, aSAH$wfns), auc(aSAH$outcome, aSAH$ndka)))
	expect_true(are.paired(auc(aSAH$outcome, aSAH$wfns), roc(aSAH$outcome, aSAH$ndka)))
})

test_that("are.paired return.paired.rocs works", {
	pair <- are.paired(r.wfns, r.ndka, return.paired.rocs = TRUE)
	expect_true(pair)
	expect_identical(attr(pair, "roc1"), r.wfns)
	expect_identical(attr(pair, "roc2"), r.ndka)
})

test_that("are.paired return.paired.rocs works with missing values", {
	aSAH.missing <- aSAH
	aSAH.missing$ndka[1:20] <- NA
	r1 <- roc(aSAH.missing$outcome, aSAH.missing$ndka)
	pair <- are.paired(r1, r.wfns, return.paired.rocs = TRUE)
	expect_true(pair)
	expect_identical(attr(pair, "roc1")$thresholds, roc(aSAH$outcome[21:113], aSAH$ndka[21:113])$thresholds)
	expect_identical(attr(pair, "roc2")$thresholds, roc(aSAH$outcome[21:113], aSAH$wfns[21:113])$thresholds)
})


test_that("are.paired return.paired.rocs doesn't return when unpaired", {
	pair <- are.paired(roc(aSAH$outcome[21:113], aSAH$wfns[21:113]), r.ndka, return.paired.rocs = TRUE)
	expect_null(attributes(pair))
})