context("Intervals")
library(finalfit)
test_that("ff_merge fit_id==NULL", {
	expect_error(ff_merge(summary_factorlist(colon_s, "mort_5yr", "age.factor")))
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

test_that("ff_describe", {
	expect_is(ff_describe(colon_s), "data.frame")
})

test_that("ff_describe", {
	expect_is(ff_describe(colon_s, na.rm=FALSE, interp=TRUE, skew=FALSE, ranges=FALSE,
												check=FALSE, IQR=TRUE, omit=TRUE), "data.frame")
})
