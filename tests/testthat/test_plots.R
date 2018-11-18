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

test_that("missing_plot gives plot", {
	expect_is(missing_plot(colon_s[,1:2]), "ggplot")
})

test_that("ff_plot gives grob", {
	expect_is(ff_plot(colon_s, "nodes", "age.factor"), "grob")
})

test_that("ff_plot gives grob", {
	expect_is(ff_plot(colon_s, "mort_5yr", "age.factor"), "grob")
})

test_that("ff_plot gives grob", {
	expect_is(ff_plot(colon_s, "Surv(time, status)", "age.factor"), "grob")
})
