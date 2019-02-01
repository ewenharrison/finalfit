library(pROC)
data(aSAH)

context("power.roc.test")

test_that("power.roc.test basic function", {
  res <- power.roc.test(r.s100b)
  expect_equal(as.numeric(res$auc), as.numeric(r.s100b$auc))
  expect_equal(res$ncases, length(r.s100b$cases))
  expect_equal(res$ncontrols, length(r.s100b$controls))
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$power, 0.9904833, tolerance = 0.000001)
})

test_that("power.roc.test with percent works", {
	res <- power.roc.test(r.s100b.percent)
	expect_equal(as.numeric(res$auc), as.numeric(r.s100b$auc))
	expect_equal(res$ncases, length(r.s100b$cases))
	expect_equal(res$ncontrols, length(r.s100b$controls))
	expect_equal(res$sig.level, 0.05)
	expect_equal(res$power, 0.9904833, tolerance = 0.000001)
})

test_that("power.roc.test with given auc function", {
  res <- power.roc.test(ncases=41, ncontrols=72, auc=0.73, sig.level=0.05)
  expect_equal(as.numeric(res$auc), 0.73)
  expect_equal(res$ncases, 41)
  expect_equal(res$ncontrols, 72)
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$power, 0.9897453, tolerance = 0.000001)
})


test_that("power.roc.test sig.level can be omitted", {
  res <- power.roc.test(ncases=41, ncontrols=72, auc=0.73)
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$power, 0.9897453, tolerance = 0.000001)
})

test_that("power.roc.test can determine ncases & ncontrols", {
  res <- power.roc.test(auc=r.s100b$auc, sig.level=0.05, power=0.95, kappa=1.7)
  expect_equal(as.numeric(res$auc), as.numeric(r.s100b$auc))
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$power, 0.95)
  expect_equal(res$ncases, 29.29764, tolerance = 0.000001)
  expect_equal(res$ncontrols, 49.806, tolerance = 0.000001)
})
  
test_that("power.roc.test can determine sig.level", {
  res <- power.roc.test(ncases=41, ncontrols=72, auc=0.73, power=0.95, sig.level=NULL)
  expect_equal(as.numeric(res$auc), 0.73)
  expect_equal(res$ncases, 41)
  expect_equal(res$ncontrols, 72)
  expect_equal(res$power, 0.95)
  expect_equal(res$sig.level, 0.009238584, tolerance = 0.000001)
})

test_that("power.roc.test can determine AUC", {
  res <- power.roc.test(ncases=41, ncontrols=72, sig.level=0.05, power=0.95)
  expect_equal(res$ncases, 41)
  expect_equal(res$ncontrols, 72)
  expect_equal(res$power, 0.95)
  expect_equal(res$sig.level, 0.05)
  expect_equal(as.numeric(res$auc), 0.6961054, tolerance = 0.000001)
})

test_that("power.roc.test can take 2 ROC curves with DeLong variance", {
  res <- power.roc.test(r.ndka, r.wfns)
  expect_equal(res$ncases, 41)
  expect_equal(res$ncontrols, 72)
  expect_equal(as.numeric(res$auc1), as.numeric(r.ndka$auc))
  expect_equal(as.numeric(res$auc2), as.numeric(r.wfns$auc))
  expect_equal(res$power, 0.7131594, tolerance = 0.000001)
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$alternative, "two.sided")
})


test_that("power.roc.test can take 2 percent ROC curves with DeLong variance", {
	res <- power.roc.test(r.ndka.percent, r.wfns.percent)
	expect_equal(res$ncases, 41)
	expect_equal(res$ncontrols, 72)
	expect_equal(as.numeric(res$auc1), as.numeric(r.ndka$auc))
	expect_equal(as.numeric(res$auc2), as.numeric(r.wfns$auc))
	expect_equal(res$power, 0.7131594, tolerance = 0.000001)
	expect_equal(res$sig.level, 0.05)
	expect_equal(res$alternative, "two.sided")
})

test_that("power.roc.test can take 2 ROC curves with Obuchowski variance", {
  res <- power.roc.test(r.ndka, r.wfns, method="obuchowski")
  expect_equal(res$ncases, 41)
  expect_equal(res$ncontrols, 72)
  expect_equal(as.numeric(res$auc1), as.numeric(r.ndka$auc))
  expect_equal(as.numeric(res$auc2), as.numeric(r.wfns$auc))
  expect_equal(res$power, 0.8061004, tolerance = 0.000001)
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$alternative, "two.sided")
})

test_that("power.roc.test ncases/ncontrols can take 2 ROC curves with DeLong variance", {
  res <- power.roc.test(r.ndka, r.wfns, power=0.9)
  expect_equal(res$ncases, 64.77777, tolerance = 0.000001)
  expect_equal(res$ncontrols, 113.7561, tolerance = 0.000001)
  expect_equal(as.numeric(res$auc1), as.numeric(r.ndka$auc))
  expect_equal(as.numeric(res$auc2), as.numeric(r.wfns$auc))
  expect_equal(res$power, 0.9)
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$alternative, "two.sided")
})

test_that("power.roc.test ncases/ncontrols can take 2 ROC curves with Obuchowski variance", {
  res <- power.roc.test(r.ndka, r.wfns, power=0.9, method="obuchowski")
  expect_equal(res$ncases, 53.23685, tolerance = 0.000001)
  expect_equal(res$ncontrols, 93.48911, tolerance = 0.000001)
  expect_equal(as.numeric(res$auc1), as.numeric(r.ndka$auc))
  expect_equal(as.numeric(res$auc2), as.numeric(r.wfns$auc))
  expect_equal(res$power, 0.9)
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$alternative, "two.sided")
})

test_that("power.roc.test sig.level can take 2 ROC curves with DeLong variance", {
  res <- power.roc.test(r.ndka, r.wfns, power=0.9, sig.level=NULL)
  expect_equal(res$ncases, 41)
  expect_equal(res$ncontrols, 72)
  expect_equal(as.numeric(res$auc1), as.numeric(r.ndka$auc))
  expect_equal(as.numeric(res$auc2), as.numeric(r.wfns$auc))
  expect_equal(res$power, 0.9)
  expect_equal(res$sig.level, 0.1836639, tolerance = 0.000001)
  expect_equal(res$alternative, "two.sided")
})

test_that("power.roc.test sig.level can take 2 ROC curves with Obuchowski variance", {
  res <- power.roc.test(r.ndka, r.wfns, power=0.9, sig.level=NULL, method="obuchowski")
  expect_equal(res$ncases, 41)
  expect_equal(res$ncontrols, 72)
  expect_equal(as.numeric(res$auc1), as.numeric(r.ndka$auc))
  expect_equal(as.numeric(res$auc2), as.numeric(r.wfns$auc))
  expect_equal(res$power, 0.9)
  expect_equal(res$sig.level, 0.1150686, tolerance = 0.000001)
  expect_equal(res$alternative, "two.sided")
})

test_that("power.roc.test works with partial AUC", {
  skip_if_not(exists("run_slow_tests") && run_slow_tests, message = "Slow test skipped")
  skip("Bootstrap cannot be tested yet")
  r.wfns.partial <<- roc(aSAH$outcome, aSAH$wfns, quiet = TRUE, partial.auc=c(1, 0.9))
  r.ndka.partial <<- roc(aSAH$outcome, aSAH$ndka, quiet = TRUE, partial.auc=c(1, 0.9))
  power.roc.test(r.wfns.partial, r.ndka.partial, power=0.9)
})


test_that("power.roc.test works with partial AUC", {
  r.wfns.partial <<- roc(aSAH$outcome, aSAH$wfns, quiet = TRUE, partial.auc=c(1, 0.9))
  r.ndka.partial <<- roc(aSAH$outcome, aSAH$ndka, quiet = TRUE, partial.auc=c(1, 0.9))
  res <- power.roc.test(r.wfns.partial, r.ndka.partial, power=0.9, method="obuchowski")
  
  expect_equal(res$ncases, 0.5061498, tolerance = 0.000001)
  expect_equal(res$ncontrols, 0.8888484, tolerance = 0.000001)
  expect_equal(as.numeric(res$auc1), as.numeric(r.wfns.partial$auc))
  expect_equal(as.numeric(res$auc2), as.numeric(r.ndka.partial$auc))
  expect_equal(res$power, 0.9)
  expect_equal(res$sig.level, 0.05)
  expect_equal(res$alternative, "two.sided")
})

test_that("power.roc.test works with binormal parameters", {
  ob.params <- list(A1=2.6, B1=1, A2=1.9, B2=1, rn=0.6, ra=0.6, FPR11=0,
                    FPR12=0.2, FPR21=0, FPR22=0.2, delta=0.037) 
  
  res1 <- power.roc.test(ob.params, power=0.8, sig.level=0.05)
  expect_equal(res1$ncases, 107.0238, tolerance = 0.000001)
  expect_equal(res1$ncontrols, 107.0238, tolerance = 0.000001)
  expect_equal(res1$power, 0.8)
  expect_equal(res1$sig.level, 0.05)
  
  res2 <- power.roc.test(ob.params, power=0.8, sig.level=NULL, ncases=107)
  expect_equal(res2$ncases, 107)
  expect_equal(res2$ncontrols, 107)
  expect_equal(res2$power, 0.8)
  expect_equal(res2$sig.level, 0.05004012, tolerance = 0.000001)
  
  res3 <- power.roc.test(ob.params, power=NULL, sig.level=0.05, ncases=107)
  expect_equal(res3$ncases, 107)
  expect_equal(res3$ncontrols, 107)
  expect_equal(res3$sig.level, 0.05)
  expect_equal(res3$power, 0.7999286, tolerance = 0.000001)
})

## With only binormal parameters given
# From example 2 of Obuchowski and McClish, 1997.
