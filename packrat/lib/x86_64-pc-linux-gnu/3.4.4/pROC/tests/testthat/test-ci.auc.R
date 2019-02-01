library(pROC)
data(aSAH)

context("ci.auc")

expected.ci.auc <- c(0.501244999271703, 0.611957994579946, 0.722670989888189)

test_that("ci.auc with delong works", {
	test.ci <- ci.auc(r.ndka)
	expect_is(test.ci, "ci.auc")
	expect_equal(as.numeric(test.ci), expected.ci.auc)
})


test_that("ci.auc with delong and percent works", {
	expect_equal(as.numeric(ci.auc(r.ndka.percent)), expected.ci.auc * 100)
})


test_that("ci.auc works with an auc", {
	expect_equal(as.numeric(ci.auc(auc(r.ndka))), expected.ci.auc)
})


test_that("ci.auc works with a formula", {
	skip("ci.auc.formula must be fixed!")
	expect_equal(as.numeric(ci.auc(outcome ~ ndka, data = aSAH)), expected.ci.auc)
	expect_equal(as.numeric(ci.auc(outcome ~ ndka, data = aSAH, subset = (gender == "Female"))), expected.ci.auc)
})


test_that("ci.auc works with a response, predictor", {
	expect_equal(as.numeric(ci.auc(aSAH$outcome, aSAH$ndka)), expected.ci.auc)
})


test_that("ci.auc works with a direction = >", {
	expect_equal(as.numeric(ci.auc(aSAH$outcome, -aSAH$ndka)), expected.ci.auc)
})


test_that("ci.auc works with a direction = > and percent", {
	expect_equal(as.numeric(ci.auc(aSAH$outcome, -aSAH$ndka, percent = TRUE)), expected.ci.auc * 100)
})
