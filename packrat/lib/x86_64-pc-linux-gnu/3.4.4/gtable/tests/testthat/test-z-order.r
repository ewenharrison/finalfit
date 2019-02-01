context("z-order")

# z tests for gtable_add_grob are in test-layout.r, mixed with other tests


test_that("z order for row, column, and matrix layouts", {
  zorder <- c(3, 1, 2, 4)

  # ==== column ====
  gt <- gtable_col("test", list(grob1, grob2, grob3, grob4))
  # z for positions 1 2 3 4 (left to right) should equal 1:4
  expect_equal(gt$layout$z[gt$layout$t], 1:4)

  gt <- gtable_col("test", list(grob1, grob2, grob3, grob4), z = zorder)
  # z for position 1 2 3 4 (left to right) should equal zorder
  expect_equal(gt$layout$z[gt$layout$t], zorder)

  # ==== row ====
  gt <- gtable_row("test", list(grob1, grob2, grob3, grob4))
  # z for positions 1 2 3 4 (top to bottom) should equal 1:4
  expect_equal(gt$layout$z[gt$layout$l], 1:4)

  gt <- gtable_row("test", list(grob1, grob2, grob3, grob4), z = zorder)
  # z for position 1 2 3 4 (top to bottom) should equal zorder
  expect_equal(gt$layout$z[gt$layout$l], zorder)

  # ==== matrix ====
  gt <- gtable_matrix("test", matrix(list(grob1, grob2, grob3, grob4), 
    nrow = 2), unit(c(1, 1), "null"), unit(c(1, 1), "null"))
  # Get the position. Should be: 1 3
  #                              2 4
  loc <- 2 * (gt$layout$l - 1) + gt$layout$t
  # z for positions 1:4 should equal 1:4
  expect_equal(gt$layout$z[loc], 1:4)

  gt <- gtable_matrix("test", matrix(list(grob1, grob2, grob3, grob4), 
    nrow = 2), unit(c(1, 1), "null"), unit(c(1, 1), "null"),
    z = matrix(zorder, nrow = 2))
  # Get the position. Should be: 1 3
  #                              2 4
  loc <- 2 * (gt$layout$l - 1) + gt$layout$t
  # z for positions 1:4 should equal zorder
  expect_equal(gt$layout$z[loc], zorder)

})


test_that("z_normalise works properly", {
  # Non-integer starting zorder, in funny order
  zorder <- c(0.001, -4, 0, 1e6)
  gt <- gtable_col("test", list(grob1, grob2, grob3, grob4), z = zorder)
  expect_equal(gt$layout$z, zorder)
  gt1 <- z_normalise(gt)
  expect_equal(sort(gt1$layout$z), 1:4)

  # OK with empty layout (zero rows in data frame)
  gt <- gtable(unit(1:3, c("cm")), unit(c(2,4), "cm"))
  gt1 <- z_normalise(gt)
  expect_equal(nrow(gt1$layout), 0)
})



test_that("z_arrange_gtables properly sets z values", {
  gt <- list(
    gtable_col("test1", list(grob1, grob2, grob3), z = c(.9, .3, .6)),
    gtable_col("test2", list(grob4, grob1, grob2), z = c(1, 3, 2)),
    gtable_col("test3", list(grob3, grob4, grob1), z = c(2, 3, 1))
  )

  # Arrange the z values of each gtable
  gt1 <- z_arrange_gtables(gt, c(3, 2, 1))
  expect_equal(gt1[[1]]$layout$z, c(9, 7, 8))
  expect_equal(gt1[[2]]$layout$z, c(4, 6, 5))
  expect_equal(gt1[[3]]$layout$z, c(2, 3, 1))

  # Check that it works with cbind and rbind (which call z_arrange_gtables)
  gt1 <- cbind(gt[[1]], gt[[2]], gt[[3]], z = c(3, 2, 1))
  expect_equal(gt1$layout$z, c(9, 7, 8, 4, 6, 5, 2, 3, 1))
  gt1 <- rbind(gt[[1]], gt[[2]], gt[[3]], z = c(3, 2, 1))
  expect_equal(gt1$layout$z, c(9, 7, 8, 4, 6, 5, 2, 3, 1))
})