context("Bind")

test_that("Number of rows grow with rbind", {
  
  lay1 <- gtable_add_rows(gtable(), cm)
  lay2 <- gtable_add_rows(gtable(), rep(cm, 2))
  
  expect_that(nrow(rbind(lay1, lay2)), equals(3))
  expect_that(nrow(rbind(lay2, lay1)), equals(3))
})

test_that("Number of cols grow with cbind", {

  lay1 <- gtable_add_cols(gtable(), cm)
  lay2 <- gtable_add_cols(gtable(), rep(cm, 2))
  
  expect_that(ncol(cbind(lay1, lay2)), equals(3))
  expect_that(ncol(cbind(lay2, lay1)), equals(3))
})

test_that("Heights and widths vary with size parameter", {
  col1 <- gtable_col("col1", list(grob1), cm, cm)
  col2 <- gtable_col("col1", list(grob1), cm2, cm2)

  expect_equal(cbind(col1, col2, size = "first")$heights, cm)
  expect_equal(cbind(col1, col2, size = "last")$heights, cm2)
  expect_equal(cbind(col1, col2, size = "min")$heights, cm)
  expect_equal(cbind(col1, col2, size = "max")$heights, cm2)

  expect_equal(rbind(col1, col2, size = "first")$widths, cm)
  expect_equal(rbind(col1, col2, size = "last")$widths, cm2)
  expect_equal(rbind(col1, col2, size = "min")$widths, cm)
  expect_equal(rbind(col1, col2, size = "max")$widths, cm2)
})
