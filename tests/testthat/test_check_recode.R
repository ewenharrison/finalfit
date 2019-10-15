context("check_recode")
library(finalfit)
test_that("check_recode gives list", {
	expect_is(check_recode(colon_s[,1:2]), "list")
})

test_that("check_recode gives list", {
	expect_is(check_recode(colon_s[,1:2], include_numerics = TRUE), "list")
})

