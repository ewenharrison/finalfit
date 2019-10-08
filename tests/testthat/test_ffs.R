context("ff")
library(finalfit)
test_that("ff_columns_totals gives data.frame", {
	expect_is(summary_factorlist(colon_s, "mort_5yr", "age.factor") %>%
							ff_column_totals(colon_s, "mort_5yr"), "data.frame")
})
