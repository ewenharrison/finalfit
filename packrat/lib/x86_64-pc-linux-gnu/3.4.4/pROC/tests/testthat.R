library(testthat)
library(pROC)
data(aSAH)


# Set environment variable RUN_SLOW_TESTS to run the slower tests
run_slow_tests <- identical(Sys.getenv("RUN_SLOW_TESTS"), "true")


test_that("We can build basic ROC curves", {
	# These curves will be re-used throughout the tests
	r.wfns <<- roc(aSAH$outcome, aSAH$wfns, quiet = TRUE)
	r.ndka <<- roc(aSAH$outcome, aSAH$ndka, quiet = TRUE)
	r.s100b <<- roc(aSAH$outcome, aSAH$s100b, quiet = TRUE)
	r.wfns.percent <<- roc(aSAH$outcome, aSAH$wfns, percent = TRUE, quiet = TRUE)
	r.ndka.percent <<- roc(aSAH$outcome, aSAH$ndka, percent = TRUE, quiet = TRUE)
	r.s100b.percent <<- roc(aSAH$outcome, aSAH$s100b, percent = TRUE, quiet = TRUE)
	
	r.wfns.partial1 <<- roc(aSAH$outcome, aSAH$wfns, quiet = TRUE, partial.auc = c(0.9, 0.99))
	r.ndka.partial1 <<- roc(aSAH$outcome, aSAH$ndka, quiet = TRUE, partial.auc = c(0.9, 0.99))
	r.s100b.partial1 <<- roc(aSAH$outcome, aSAH$s100b, quiet = TRUE, partial.auc = c(0.9, 0.99))
	r.wfns.percent.partial1 <<- roc(aSAH$outcome, aSAH$wfns, percent = TRUE, quiet = TRUE, partial.auc = c(90, 99))
	r.ndka.percent.partial1 <<- roc(aSAH$outcome, aSAH$ndka, percent = TRUE, quiet = TRUE, partial.auc = c(90, 99))
	r.s100b.percent.partial1 <<- roc(aSAH$outcome, aSAH$s100b, percent = TRUE, quiet = TRUE, partial.auc = c(90, 99))
})

test_check("pROC")
