library('testthat')
library('magrittr', warn.conflicts = FALSE)
library('XML', warn.conflicts = FALSE)
context('htmlTable')

# Check that a css.cell passes without errors
test_that("Check inputs", {
  mx <- matrix(1:6, ncol=3)
  css.cell ="background: red"
  htmlTable(mx, css.cell=css.cell)
})

