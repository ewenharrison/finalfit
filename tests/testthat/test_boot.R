context("Bootstrapping")
library(finalfit)

test_that("ff_newdata gives dataframe", {
	expect_is(ff_newdata(colon_s, explanatory = c("age.factor", "extent.factor"), newdata = list(
		c("<40 years",  "Submucosa"),
		c("<40 years", "Submucosa"))) -> newdata,
		"data.frame")
})

test_that("ff_newdata gives dataframe", {
	expect_is(ff_newdata(colon_s, explanatory = c("nodes", "extent.factor", "perfor.factor"), newdata = list(
		rep(seq(0, 30), 4),
		c(rep("Muscle", 62), rep("Adjacent structures", 62)),
		c(rep("No", 31), rep("Yes", 31), rep("No", 31), rep("Yes", 31))
	)) -> newdata,
		"data.frame")
})

test_that("ff_newdata gives dataframe", {
	expect_is(colon_s %>%
							glmmulti("mort_5yr", c("age.factor", "extent.factor")) %>%
							boot_predict(newdata = ff_newdata(colon_s, explanatory = c("age.factor", "extent.factor"),
																								newdata = list(
																									c("<40 years",  "Submucosa"),
																									c("<40 years", "Submucosa"))),
													 estimate_name = "Predicted probability of death",
													 compare_name = "Absolute risk difference", R=40, digits = c(2,3)),
						"data.frame")
})
