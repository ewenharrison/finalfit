context("finalfit function no metrics no keep")
library(finalfit)
test_that("finalfit.lm with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "nodes", "age.factor"), "data.frame")
})

test_that("finalfit.lmer with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", random_effect="hospital"), "data.frame")
})

test_that("finalfit.glm with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor"), "data.frame")
})

test_that("finalfit mixed with metrics gives data.frame", {
	expect_is(finalfit(colon_s,  "mort_5yr", "age.factor", random_effect="hospital"), "data.frame")
})

test_that("finalfit.coxph gives dataframe", {
	expect_is(finalfit(colon_s,  "Surv(time, status)", "age.factor"), "data.frame")
})

test_that("finalfit.glm with ff_remove_ref gives data.frame", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor", add_dependent_label = FALSE) %>% 
							ff_remove_ref(), "data.frame")
})


context("finalfit function no metrics keep models")
library(finalfit)
test_that("finalfit.lm with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", keep_models=TRUE), "data.frame")
})

test_that("finalfit.lm with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", "age.factor", keep_models=TRUE), "data.frame")
})

test_that("finalfit.lmer with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", random_effect="hospital", keep_models=TRUE), "data.frame")
})
test_that("finalfit.lmer with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", "age.factor", random_effect="hospital", keep_models=TRUE), "data.frame")
})

test_that("finalfit.glm with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor", keep_models=TRUE), "data.frame")
})

test_that("finalfit mixed with metrics gives data.frame", {
	expect_is(finalfit(colon_s,  "mort_5yr", "age.factor", random_effect="hospital", keep_models=TRUE), "data.frame")
})

test_that("finalfit.glm with metrics gives data.frame", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor", "age.factor", keep_models=TRUE), "data.frame")
})

test_that("finalfit mixed with metrics gives data.frame", {
	expect_is(finalfit(colon_s,  "mort_5yr", "age.factor", "age.factor", random_effect="hospital", keep_models=TRUE), "data.frame")
})

test_that("finalfit.coxph gives dataframe", {
	expect_is(finalfit(colon_s,  "Surv(time, status)", "age.factor", keep_models=TRUE), "data.frame")
})

test_that("finalfit.coxph gives dataframe", {
	expect_is(finalfit(colon_s,  "Surv(time, status)", "age.factor", "age.factor", keep_models=TRUE), "data.frame")
})





context("finalfit function no metrics no keep")
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
	expect_is(finalfit(colon_s,  "Surv(time, status)", "age.factor", metrics=TRUE), "list")
})




context("finalfit function metrics keep models")
library(finalfit)
test_that("finalfit.lm with metrics gives list", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", keep_models=TRUE, metrics=TRUE), "list")
})

test_that("finalfit.lmer with metrics gives list", {
	expect_is(finalfit(colon_s, "nodes", "age.factor", random_effect="hospital", keep_models=TRUE, metrics=TRUE), "list")
})

test_that("finalfit.glm with metrics gives list", {
	expect_is(finalfit(colon_s, "mort_5yr", "age.factor", keep_models=TRUE, metrics=TRUE), "list")
})

test_that("finalfit mixed with metrics gives list", {
	expect_is(finalfit(colon_s,  "mort_5yr", "age.factor", random_effect="hospital", keep_models=TRUE, metrics=TRUE), "list")
})

test_that("finalfit.coxph gives dataframe", {
	expect_is(finalfit(colon_s,  "Surv(time, status)", "age.factor", keep_models=TRUE, metrics=TRUE), "list")
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


context("summary_factorlist function with geometric mean")
library(finalfit)

test_that("summary_factorlist gives dataframe", {
	expect_is(summary_factorlist(colon_s, "extent.factor", c("age.factor", "age"),
															 cont="geometric"), "data.frame")
})

test_that("summary_factorlist gives error when variable contains zero and geometric mean called", {
	expect_error(summary_factorlist(colon_s, "extent.factor", c("age.factor", "nodes"),
															 cont="geometric"))
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


context("finalfit_permute")
library(finalfit)

test_that("finalfit_permute gives a list", {
	expect_is(ff_permute(
		colon_s,
		"nodes",
		c("age.factor"),
		c("obstruct.factor", "perfor.factor"),
		multiple_tables = TRUE,
		base_on_top = TRUE
	), "list")
})

test_that("finalfit_permute gives a list", {
	expect_is(ff_permute(
		colon_s,
		"nodes",
		c("age.factor"),
		c("obstruct.factor", "perfor.factor"),
		multiple_tables = TRUE,
		base_on_top = FALSE
	), "list")
})

test_that("finalfit_permute gives a list", {
	expect_is(ff_permute(
		colon_s,
		"nodes",
		c("age.factor"),
		c("obstruct.factor", "perfor.factor"),
		multiple_tables = FALSE,
		base_on_top = FALSE
	), "data.frame")
})

test_that("finalfit_permute gives a list", {
	expect_is(ff_permute(
		colon_s,
		"nodes",
		c("age.factor"),
		c("obstruct.factor", "perfor.factor"),
		multiple_tables = FALSE,
		base_on_top = TRUE
	), "data.frame")
})


test_that("finalfit_permute gives a list", {
	expect_is(ff_permute(
		colon_s,
		"nodes",
		c("age.factor"),
		c("obstruct.factor", "perfor.factor"),
		multiple_tables = TRUE,
		include_base_model = FALSE,
		include_full_model = FALSE
	), "list")
})

test_that("finalfit_permute gives a list", {
	expect_is(ff_permute(
		colon_s,
		"nodes",
		c("age.factor"),
		c("obstruct.factor", "perfor.factor"),
		multiple_tables = FALSE,
		include_base_model = FALSE,
		include_full_model = FALSE
	), "data.frame")
})