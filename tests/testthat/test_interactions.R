context("Interactions")
library(finalfit)
test_that("ff_interaction", {
	expect_is(colon_s %>% ff_interaction(sex.factor, perfor.factor), "data.frame")
})
