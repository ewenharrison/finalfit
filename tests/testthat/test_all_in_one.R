context("finalfit function")
library(finalfit)
test_that("finalfit.lm with metrics gives list", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", metrics=TRUE), "list")
})

test_that("finalfit.lmer with metrics gives list", {
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




context("summary_factorlist function")
library(finalfit)

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "age", c("age.factor", "nodes"),
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	colon_s$one = 1
	colon_s$one = factor(colon_s$one)
	expect_is(summary_factorlist(colon_s, "one", c("age.factor", "nodes"),
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})


test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "mort_5yr", c("age.factor", "nodes"),
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "rx", c("age.factor", "nodes"),
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "extent.factor", c("age.factor", "nodes"),
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	colon_s$age5 = cut(colon_s$age, breaks=5)
	expect_is(summary_factorlist(colon_s, "age5", c("age.factor", "nodes"),
															 column=TRUE, total_col = TRUE, p=TRUE), "data.frame")
})





context("summary_factorlist function with median and dep label")
library(finalfit)

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "age", c("age.factor", "nodes"),
															 cont="median", add_dependent_label=TRUE, fit_id=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	colon_s$one = 1
	colon_s$one = factor(colon_s$one)
	expect_is(summary_factorlist(colon_s, "one", c("age.factor", "nodes"),
															 cont="median", add_dependent_label=TRUE, fit_id=TRUE), "data.frame")
})


test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "mort_5yr", c("age.factor", "nodes"),
															 cont="median", add_dependent_label=TRUE, fit_id=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "rx", c("age.factor", "nodes"),
															 cont="median", add_dependent_label=TRUE, fit_id=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "extent.factor", c("age.factor", "nodes"),
															 cont="median", add_dependent_label=TRUE, fit_id=TRUE), "data.frame")
})

test_that("summary_factorlist gives dataframe", {
	colon_s$age5 = cut(colon_s$age, breaks=5)
	expect_is(summary_factorlist(colon_s, "age5", c("age.factor", "nodes"),
															 cont="median", add_dependent_label=TRUE, fit_id=TRUE), "data.frame")
})



context("summary_factorlist function with survival")
library(finalfit)

test_that("summary_factorlist with survival dataframe", {
	expect_is(summary_factorlist(colon_s, "Surv(time, status)", "age.factor"), "data.frame")
})


context("summary_factorlist warnings")
library(finalfit)

test_that("summary_factorlist warnings", {
	expect_error(summary_factorlist(1))
	expect_error(summary_factorlist(colon_s, "rx"))
})


context("summary_factorlist cont_cut")
library(finalfit)

test_that("summary_factorlist convert factor", {
	expect_equal(summary_factorlist(colon_s, "mort_5yr", "sex") %>%
									dim() %>%
									sum(), 6)
})

test_that("summary_factorlist don't convert factor", {
	expect_equal(summary_factorlist(colon_s, "mort_5yr", "sex", cont_cut=0) %>%
									dim() %>%
									sum(), 5)
})
