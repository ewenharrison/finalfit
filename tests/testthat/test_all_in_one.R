context("finalfit function")
library(finalfit)

<<<<<<< HEAD
test_that("finalfit.lm with metrics gives list", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", metrics=TRUE), "list")
})

test_that("finalfit.lm with metrics gives list", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", random_effect="hospital", metrics=TRUE), "list")
})

test_that("finalfit.glm with metrics gives list", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor", metrics=TRUE), "list")
})

test_that("finalfit mixed with metrics gives list", {
	expect_is(finalfit(colon_s,  "mort_5yr", "age.factor", random_effect="hospital", metrics=TRUE), "list")
})

test_that("finalfit.coxph gives dataframe", {
	expect_is(finalfit(colon_s,  "Surv(time, status)", "age.factor"), "data.frame")
})

=======
test_that("finalfit.glm gives dataframe", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor"), "data.frame")
})

test_that("finalfit.lm gives dataframe", {
	expect_is(finalfit(colon_s, "nodes", "age.factor"), "data.frame")
})

test_that("finalfit.coxph gives dataframe", {
	expect_is(finalfit(colon_s,  "Surv(time, status)", "age.factor"), "data.frame")
})

test_that("finalfit  mixed gives dataframe", {
	expect_is(finalfit(colon_s,  "mort_5yr", "age.factor", random_effect="hospital"), "data.frame")
})

test_that("finalfit.glm with metrics gives list", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor", metrics=TRUE), "list")
})


>>>>>>> 24b4f18776f5b1400a0aaf522404e77d7ff52e40
context("summary_factorlist function")
library(finalfit)

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "age", "age.factor",
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	colon_s$one = 1
	colon_s$one = factor(colon_s$one)
	expect_is(summary_factorlist(colon_s, "one", "age.factor",
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})


test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "mort_5yr", "age.factor",
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "rx", "age.factor",
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "extent.factor", "age.factor",
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	colon_s$age5 = cut(colon_s$age, breaks=5)
	expect_is(summary_factorlist(colon_s, "age5", "age.factor",
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})
