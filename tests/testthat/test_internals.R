context("Internals")
library(finalfit)
test_that("ff_merge fit_id==NULL", {
	expect_error(ff_merge(summary_factorlist(colon_s, "mort_5yr", "age.factor")))
})

test_that("ff_merge fit_id==NULL", {
	expect_error(finalfit_merge(summary_factorlist(colon_s, "mort_5yr", "age.factor")))
})

test_that("extract_variable_label gives character", {
	expect_is(extract_variable_label(colon_s[,1:2]), "character")
})


test_that("round_tidy", {
	expect_is(round_tidy(1.123, 2), "character")
})

test_that("p_tidy", {
	expect_is(p_tidy(1.123, 2), "character")
})

test_that("dependent_label", {
	expect_is(
		summary_factorlist(colon_s, "mort_5yr", "age.factor") %>%
			dependent_label(colon_s, "mort_5yr"), "data.frame")
})


test_that("extract_labels", {
	expect_is(extract_labels(colon_s[,1:2]), "data.frame")
})

test_that("extract_variable_labels", {
	expect_is(extract_variable_label(colon_s[,1:2]), "character")
})

test_that("remove_label", {
	expect_is(remove_labels(colon_s[,1:2]), "data.frame")
})

test_that("missing_glimpse", {
	expect_is(missing_glimpse(colon_s, digits=2), "data.frame")
})

test_that("missing_glimpse", {
	expect_is(missing_glimpse(colon_s, dependent="mort_5yr"), "data.frame")
})

test_that("variable type works", {
	expect_match(variable_type(as.Date("12.03.18", "%d.%m.%y")), "date")
})

test_that("variable type works", {
	expect_match(variable_type(factor(c("yes", "no"))), "factor")
})

test_that("variable type works", {
	expect_match(variable_type(c("yes", "no")), "character")
})

test_that("variable type works", {
	expect_match(variable_type(1:10), "numeric")
})

test_that("variable type works", {
	expect_match(variable_type(as.logical(c("true", "false"))), "logical")
})

test_that("is.factor", {
	expect_true(is.survival("Surv(mort, time)"))
})

test_that("is.factor", {
	expect_false(is.survival("Sur(mort, time)"))
})


test_that("is.factor", {
	expect_false(is.survival("Sur(mort, time)"))
})

test_that("ff_label", {
	expect_is(colon_s$sex.factor %>% ff_label("Sex"), "factor")
})

test_that("finalfit_label", {
	expect_is(colon_s$sex.factor %>% finalfit_label("Sex"), "factor")
})
