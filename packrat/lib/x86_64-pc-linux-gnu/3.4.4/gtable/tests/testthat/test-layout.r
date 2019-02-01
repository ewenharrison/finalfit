library(testthat)

# Find location of a grob
gtable_find <- function(x, grob) {
  pos <- vapply(x$grobs, identical, logical(1), grob)
  x$layout[pos, ]
}

loc_df <- function(t, l, b, r) {
  data.frame(t, l, b, r, z = 1, clip = "on", name = "layout", 
    stringsAsFactors = FALSE)
}

context("gtable")

test_that("Number of rows grows with add_rows", {
  layout <- gtable()
  expect_that(nrow(layout), equals(0))

  layout <- gtable_add_rows(layout, unit(1, "cm"))
  expect_that(nrow(layout), equals(1))

  layout <- gtable_add_rows(layout, unit(1, "cm"))
  layout <- gtable_add_rows(layout, unit(1, "cm"))
  expect_that(nrow(layout), equals(3))

  layout <- gtable_add_rows(layout, unit(1:2, "cm"))
  expect_that(nrow(layout), equals(5))
})


test_that("Number of columns grows with add_cols", {
  layout <- gtable()
  expect_that(ncol(layout), equals(0))

  layout <- gtable_add_cols(layout, unit(1, "cm"))
  expect_that(ncol(layout), equals(1))

  layout <- gtable_add_cols(layout, unit(c(1, 1), "cm"))
  expect_that(ncol(layout), equals(3))

  layout <- gtable_add_cols(layout, unit(1:2, "cm"))
  expect_that(ncol(layout), equals(5))
})


test_that("Setting and getting works", {
  layout <- gtable_add_cols(gtable_add_rows(gtable(), cm), cm)
  
  layout <- gtable_add_grob(layout, grob1, 1, 1)
  loc <- gtable_find(layout, grob1)
  
  expect_that(nrow(loc), equals(1))
  expect_that(loc$t, equals(1))
  expect_that(loc$r, equals(1))
  expect_that(loc$b, equals(1))
  expect_that(loc$l, equals(1))
})

test_that("Spanning grobs continue to span after row insertion", {  
  layout <- gtable_add_cols(gtable_add_rows(gtable(), rep(cm, 3)), rep(cm, 3))
  layout <- gtable_add_grob(layout, grob1, 1, 1, 3, 3)
  
  within <- gtable_add_rows(gtable_add_cols(layout, cm, pos = 2), cm, pos = 2)  
  loc <- gtable_find(within, grob1)
  
  expect_that(loc, equals(loc_df(t = 1, l = 1, b = 4, r = 4)))
  
  top_left <- layout
  top_left <- gtable_add_cols(top_left, cm, pos = 0)
  top_left <- gtable_add_rows(top_left, cm, pos = 0)

  loc <- gtable_find(top_left, grob1)
  expect_that(loc, equals(loc_df(t = 2, l = 2, b = 4, r = 4)))
  
  bottom_right <- layout
  bottom_right <- gtable_add_cols(bottom_right, cm)
  bottom_right <- gtable_add_rows(bottom_right, cm)

  loc <- gtable_find(bottom_right, grob1)
  expect_that(loc, equals(loc_df(t = 1, l = 1, b = 3, r = 3)))
})


test_that("n + 1 new rows/cols after spacing", {
  layout <- gtable()
  layout <- gtable_add_rows(layout, rep(cm, 3))
  layout <- gtable_add_cols(layout, rep(cm, 3))
  
  layout <- gtable_add_col_space(layout, cm)
  expect_that(ncol(layout), equals(5))
  
  layout <- gtable_add_row_space(layout, cm)
  expect_that(ncol(layout), equals(5))
})

test_that("Spacing adds rows/cols in correct place", {
  layout <- gtable()
  layout <- gtable_add_rows(layout, rep(cm, 2))
  layout <- gtable_add_cols(layout, rep(cm, 2))
  
  layout <- gtable_add_col_space(layout, null)
  layout <- gtable_add_row_space(layout, null)
  
  expect_that(as.vector(layout$heights), equals(rep(1, 3)))
  expect_that(attr(layout$heights, "unit"), equals(c("cm", "null", "cm")))

  expect_that(as.vector(layout$widths), equals(rep(1, 3)))
  expect_that(attr(layout$widths, "unit"), equals(c("cm", "null", "cm")))
  
})

test_that("Negative positions place from end", {
  layout <- gtable()
  layout <- gtable_add_rows(layout, rep(cm, 3))
  layout <- gtable_add_cols(layout, rep(cm, 3))
  
  col_span <- gtable_add_grob(layout, grob1, t = 1, l = 1, r = -1)
  expect_that(gtable_find(col_span, grob1), 
    equals(loc_df(t = 1, l = 1, b = 1, r = 3)))

  row_span <- gtable_add_grob(layout, grob1, t = 1, l = 1, b = -1)
  expect_that(gtable_find(row_span, grob1), 
    equals(loc_df(t = 1, l = 1, b = 3, r = 1)))
})

test_that("Adding multiple grobs", {
  grobs <- rep(list(grob1), 8)

  # With z = Inf, and t value for each grob
  tval <- c(1, 2, 3, 1, 2, 3, 1, 2)
  layout <- gtable_add_cols(gtable_add_rows(gtable(), rep(cm, 3)), rep(cm, 3))
  layout <- gtable_add_grob(layout, grobs, tval, 1, 3, 3, z = Inf)
  expect_equal(layout$layout$t, tval)
  expect_equal(layout$layout$z, 1:8)

  # With z = -Inf
  layout <- gtable_add_cols(gtable_add_rows(gtable(), rep(cm, 3)), rep(cm, 3))
  layout <- gtable_add_grob(layout, grobs, 1, 1, 3, 3, z = -Inf)
  expect_equal(layout$layout$z, -7:0)

  # Mixing Inf and non-Inf z values
  zval <- c(Inf, Inf, 6, 0, -Inf, Inf, -2, -Inf)
  layout <- gtable_add_cols(gtable_add_rows(gtable(), rep(cm, 3)), rep(cm, 3))
  layout <- gtable_add_grob(layout, grobs, 1, 1, 3, 3, z = zval)
  expect_equal(layout$layout$z, c(7, 8, 6, 0, -4, 9, -2, -3))

  # Error if inputs are not length 1 or same length as grobs
  layout <- gtable_add_cols(gtable_add_rows(gtable(), rep(cm, 3)), rep(cm, 3))
  expect_error(gtable_add_grob(layout, grobs, c(1:3), 1, 3, 3))
  expect_error(gtable_add_grob(layout, grobs, tval, 1:2, 3, 3))
  expect_error(gtable_add_grob(layout, grobs, tval, 1, 3, 3, z = 1:4))

})
