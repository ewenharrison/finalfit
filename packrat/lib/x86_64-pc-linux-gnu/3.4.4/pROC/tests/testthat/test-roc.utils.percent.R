library(pROC)
data(aSAH)

context("roc.utils.percent")

remove.calls.recursive <- function(x) {
	if (is.null(x)) return(NULL)
	attr(x, "roc") <- remove.calls.recursive(attr(x, "roc"))
	attr(x, "auc") <- remove.calls.recursive(attr(x, "auc"))
	attr(x, "ci") <- remove.calls.recursive(attr(x, "ci"))
	if (!is.list(x)) return(x)
	x$roc <- remove.calls.recursive(x$roc)
	x$auc <- remove.calls.recursive(x$auc)
	x$ci <- remove.calls.recursive(x$ci)
	x$call <- NULL
	return(x)
}

expect_equal_ignore_call <- function(x, y, ...) {
	x <- remove.calls.recursive(x)
	y <- remove.calls.recursive(y)
	expect_equal(x, y, ...)
}

test_that("roc.utils.topercent works on full AUC", {
	expect_equal_ignore_call(pROC:::roc.utils.topercent.roc(r.wfns), r.wfns.percent)
})

test_that("roc.utils.unpercent works on full AUC", {
	expect_equal_ignore_call(pROC:::roc.utils.unpercent.roc(r.wfns.percent), r.wfns)
})

test_that("roc.utils.topercent works on partial AUC", {
	expect_equal_ignore_call(pROC:::roc.utils.topercent.roc(r.wfns.partial1), r.wfns.percent.partial1)
})

test_that("roc.utils.unpercent works on partial AUC", {
	expect_equal_ignore_call(pROC:::roc.utils.unpercent.roc(r.wfns.percent.partial1), r.wfns.partial1)
})

test_that("roc.utils.topercent works with CI", {
	r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc.utils.topercent.roc(r), r.percent)
})

test_that("roc.utils.unpercent works with CI", {
	r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc.utils.unpercent.roc(r.percent), r)
})

test_that("roc.utils.topercent works without AUC", {
	r <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc.utils.topercent.roc(r), r.percent)
})

test_that("roc.utils.unpercent works without AUC", {
	r <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc.utils.unpercent.roc(r.percent), r)
})

test_that("roc.utils.topercent works with different types of CI", {
	r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc.utils.topercent.roc(r), r.percent)
})

test_that("roc.utils.to/unpercent works with ci .thresholds, .sp, .se", {
	skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
	for (of in c("thresholds", "sp", "se")) {
		set.seed(42)
		r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, of = of)
		set.seed(42)
		r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE, of = of)
		expect_equal_ignore_call(pROC:::roc.utils.unpercent.roc(r.percent), r)
		expect_equal_ignore_call(pROC:::roc.utils.topercent.roc(r), r.percent)
	}
})