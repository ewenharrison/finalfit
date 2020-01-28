context("Metrics")
library(finalfit)
test_that("ff_metrics glm", {
	expect_is(glm(mort_5yr ~  age.factor, data=colon_s, family="binomial") %>%
							ff_metrics(), "data.frame")
})


test_that("ff_metrics glmlist", {
	expect_is(glmmulti(colon_s, "mort_5yr", "age.factor") %>%
							ff_metrics(), "data.frame")
})

test_that("ff_metrics glmerMod", {
	expect_is(glmmixed(colon_s, "mort_5yr", "age.factor", "hospital") %>%
							ff_metrics(), "data.frame")
})


test_that("ff_metrics lm", {
	expect_is(lm(nodes ~  age.factor, data=colon_s) %>%
							ff_metrics(), "data.frame")
})

test_that("ff_metrics lmerlist", {
	expect_is(lmmulti(colon_s, "nodes", "age.factor") %>%
							ff_metrics(), "data.frame")
})


test_that("ff_metrics lmermixed", {
	expect_is(lmmixed(colon_s, "nodes", "age.factor", "hospital") %>%
							ff_metrics(), "data.frame")
})

test_that("ff_metrics coxphlist", {
	expect_is(coxphmulti(colon_s, "Surv(time, status)", "age.factor") %>%
							ff_metrics(), "data.frame")
})

test_that("ff_metrics coxph", {
	expect_is(survival::coxph(survival::Surv(time, status) ~ age.factor, data = colon_s) %>%
							ff_metrics(), "data.frame")
})
