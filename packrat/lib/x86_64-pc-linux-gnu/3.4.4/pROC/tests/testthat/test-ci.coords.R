library(pROC)
data(aSAH)

context("ci.coords")


test_that("ci.coords accepts threshold output with x=best", {
	expect_error(ci.coords(r.wfns, x="best", input="specificity", ret=c("threshold", "specificity", "sensitivity"), boot.n = 1), NA)
})

test_that("ci.coords rejects threshold output except with x=best", {
	expect_error(ci.coords(r.wfns, x=0.9, input="specificity", ret=c("threshold", "specificity", "sensitivity"), boot.n = 1))
})
