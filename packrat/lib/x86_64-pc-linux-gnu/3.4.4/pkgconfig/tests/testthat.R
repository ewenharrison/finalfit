
if (require(testthat, quietly = TRUE) &&
    require(disposables, quietly = TRUE)) {
  library(pkgconfig)
  test_check("pkgconfig")

} else {
  cat("The testthat and disposables packages are required for unit tests")
}
