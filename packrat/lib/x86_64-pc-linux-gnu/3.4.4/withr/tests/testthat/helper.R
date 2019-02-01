expect_no_output <- function(...) {
  testthat::expect_output(..., regexp = NA)
}
