context("Statistical tests")
library(finalfit)
test_that("catTestfisher works", {
	expect_is(summary_factorlist(colon_s, "mort_5yr", "age.factor", catTest = catTestfisher), "data.frame")
})

