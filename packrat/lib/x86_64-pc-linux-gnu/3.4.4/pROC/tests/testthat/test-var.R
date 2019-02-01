library(pROC)
data(aSAH)


test_that("var with delong works", {
	expect_equal(var(r.wfns), 0.00146991470882363)
	expect_equal(var(r.ndka), 0.0031908105493913)
	expect_equal(var(r.s100b), 0.00266868245717244)
})


test_that("var works with auc", {
	expect_equal(var(auc(r.wfns)), 0.00146991470882363)
	expect_equal(var(auc(r.ndka)), 0.0031908105493913)
	expect_equal(var(auc(r.s100b)), 0.00266868245717244)
})


test_that("var with delong and percent works", {
	expect_equal(var(r.wfns.percent), 14.6991470882363)
	expect_equal(var(r.ndka.percent), 31.908105493913)
	expect_equal(var(r.s100b.percent), 26.6868245717244)
})


test_that("var works with auc and percent", {
	expect_equal(var(auc(r.wfns.percent)), 14.6991470882363)
	expect_equal(var(auc(r.ndka.percent)), 31.908105493913)
	expect_equal(var(auc(r.s100b.percent)), 26.6868245717244)
})


test_that("var with delong and percent works", {
	expect_equal(var(roc(aSAH$outcome, -aSAH$ndka, percent=TRUE)), 31.908105493913)
	expect_equal(var(roc(aSAH$outcome, -aSAH$s100b, percent=TRUE)), 26.6868245717244)
})
