context("Model wrappers: glm")
library(finalfit)

test_that("glmuni gives glmlist", {
	expect_is(glmuni(colon_s, "mort_5yr", "age.factor"), "glmlist")
})

test_that("glmmulti gives glmlist", {
	expect_is(glmmulti(colon_s, "mort_5yr", "age.factor"), "glmlist")
})

test_that("glmmixed gives glmerMod", {
	expect_is(glmmixed(colon_s, "mort_5yr", "age.factor", "hospital"), "glmerMod")
})

test_that("glmmixed_boot gives list", {
	expect_is(glmmulti_boot(colon_s, "mort_5yr", "age.factor", R=50), "glmboot")
})


context("Model wrappers: lm")
library(finalfit)
test_that("lmuni gives lmlist", {
	expect_is(lmuni(colon_s, "nodes", "age.factor"), "lmlist")
})

test_that("lmmulti gives lmlist", {
	expect_is(lmmulti(colon_s, "nodes", "age.factor"), "lmlist")
})

test_that("lmmixed gives lmerMod", {
	expect_is(lmmixed(colon_s,  "nodes", "age.factor", "hospital"), "lmerMod")
})

context("Model wrappers: coxph")
library(finalfit)
test_that("coxphuni gives coxphlist", {
	expect_is(coxphuni(colon_s,  "Surv(time, status)", "age.factor"), "coxphlist")
})

test_that("coxphmulti gives coxphlist", {
	expect_is(coxphmulti(colon_s,  "Surv(time, status)", "age.factor"), "coxphlist")
})
