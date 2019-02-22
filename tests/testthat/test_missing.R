context("Missing")
library(finalfit)

test_that("ff_glimpse gives list", {
	expect_is(ff_glimpse(colon_s, explanatory = c("age", "age.factor")), "list")
})


test_that("missing_pattern gives matrix", {
	expect_is(missing_pattern(colon_s, "mort_5yr", c("age", "age.factor")), "matrix")
})

test_that("missing_pattern gives matrix", {
	expect_is(missing_pattern(colon_s), "matrix")
})

test_that("missing_pairs gives plot", {
	expect_is(missing_pairs(colon_s, explanatory = c("age", "age.factor")), "ggmatrix")
})

test_that("missing_pairs gives plot", {
	expect_is(missing_compare(colon_s, "mort_5yr", explanatory = c("age", "age.factor")), "data.frame")
})

test_that("missing_predictorMatrix gives matrix", {
	expect_is(colon_s %>%
							dplyr::select(age, age.factor) %>%
							missing_predictorMatrix(drop_from_imputed = "age") -> predM,
						"matrix")
})
