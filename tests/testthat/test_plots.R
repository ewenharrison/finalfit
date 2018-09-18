context("Plots")
library(finalfit)

test_that("or_plot gives plot", {
	expect_is(or_plot(colon_s, "mort_5yr", "age.factor"), "grob")
})

test_that("hr_plot gives plot", {
	expect_is(hr_plot(colon_s, "Surv(time, status)", "age.factor"), "grob")
})

test_that("surv_plot gives plot", {
	expect_is(surv_plot(colon_s, "Surv(time, status)", "age.factor"), "ggsurvplot")
})

test_that("missing_plot gives plot", {
	expect_is(missing_plot(colon_s[,1:2]), "ggplot")
})

test_that("missing_plot gives plot", {
	expect_is(missing_plot(colon_s[,1:2]), "ggplot")
})

dependent = "Surv(time,status)"
explanatory="age.factor"
surv_plot(colon_s, dependent, explanatory)
surv_plot(colon_s, dependent="Surv(time,status)", explanatory="age.factor")
