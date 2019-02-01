context("Subsetting")

base <- gtable(unit(rep(1, 3), "null"), unit(rep(1, 3), "null"))
rownames(base) <- LETTERS[1:3]
colnames(base) <- letters[1:3]

test_that("dimensions correct after subsetting", {
  expect_equal(dim(base[, ]), c(3, 3))
  expect_equal(dim(base[1:3, 1:3]), c(3, 3))
  expect_equal(dim(base[T, T]), c(3, 3))
  expect_equal(dim(base[c("A", "B", "C"), c("a", "b", "c")]), c(3, 3))

  expect_equal(dim(base[1, 1]), c(1, 1))
  expect_equal(dim(base[c(T, F, F), c(T, F, F)]), c(1, 1))
  expect_equal(dim(base[-(2:3), -(2:3)]), c(1, 1))
  expect_equal(dim(base["A", "b"]), c(1, 1))

  expect_equal(dim(base[1:2, 2:3]), c(2, 2))
})

rect <- rectGrob()
mid <- gtable_add_grob(base, rect, 2, 2)
row <- gtable_add_grob(base, rect, 2, l = 1, r = 3)
col <- gtable_add_grob(base, rect, 2, t = 1, b = 3)

tlbr <- function(x) unname(unlist(x$layout[c("t", "l", "b", "r")]))

test_that("grobs moved to correct location", {

  expect_equal(tlbr(mid[2, 2]),     c(1, 1, 1, 1))
  expect_equal(tlbr(mid[2:3, 2:3]), c(1, 1, 1, 1))

  expect_equal(tlbr(mid[1:2, 1:2]), c(2, 2, 2, 2))
  expect_equal(tlbr(mid[1:3, 1:3]), c(2, 2, 2, 2))
})

test_that("spanning grobs kept if ends kept", {

  expect_equal(length(row[, -2]), 1)
  expect_equal(tlbr(row[, -2]), c(2, 1, 2, 2))

  expect_equal(length(col[-2, ]), 1)
  expect_equal(tlbr(col[-2, ]), c(1, 2, 2, 2))

  expect_equal(length(row[, 1]), 0)
  expect_equal(length(col[1, ]), 0)

})


# Detailed tests for indexing with [.gtable ----------------------------------

# Some of these tests can be confusing; if you need to see
# what's going on, run grid.draw(gt)

# Make a bunch of grobs
g1 <- rectGrob()
g2 <- circleGrob()
g3 <- polygonGrob()
g4 <- linesGrob()
g5 <- circleGrob()
g6 <- rectGrob()

unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

# Check that two gtable objects are the same.
# This allows for differences in how units are stored and other subtle
# changes that don't affect appearance.
equal_gtable <- function(a, b) {
  identical(a$grobs, b$grobs)  &&
  # Normalized z values are the same (ensuring same render order)
  # Also ignore row names
  all.equal(unrowname(z_normalise(a)$layout),
            unrowname(z_normalise(b)$layout)) &&
  # Test widths/heights for equality.
  # This is the best way I could think of, but it's not very nice
  all(convertUnit(a$widths  - b$widths,  "cm", valueOnly = TRUE) == 0) &&
  all(convertUnit(a$heights - b$heights, "cm", valueOnly = TRUE) == 0) &&
  all.equal(a$respect, b$respect) &&
  all.equal(a$rownames, b$rownames) &&
  all.equal(a$colnames, b$colnames)
}


# This will create a new gtable made with gtable_matrix
# using the specified cols and rows from grobmat.
# The sizes of the rows/cols are the same as the index values (but in cm)
make_gt <- function(grobmat, rows, cols) {
  gtable_matrix("test", grobmat[rows, cols, drop = FALSE],
    heights=unit(rows, "cm"), widths=unit(cols, "cm") )
}


test_that("Indexing with single-cell grobs", {
  # Make a 2x3 gtable where each cell has one grob
  grobmat <- matrix(list(g1, g2, g3, g4, g5, g6), nrow=2)
  gt <- make_gt(grobmat, 1:2, 1:3)

  # Indexing in ways that don't change gt
  expect_true(equal_gtable(gt, gt[1:2, 1:3]))
  expect_true(equal_gtable(gt, gt[]))
  expect_true(equal_gtable(gt, gt[1:2, ]))
  expect_true(equal_gtable(gt, gt[, 1:3]))

  # New table from contiguous cells
  expect_true(equal_gtable(gt[1, 1], make_gt(grobmat, 1, 1)))
  expect_true(equal_gtable(gt[2, 2], make_gt(grobmat, 2, 2)))
  expect_true(equal_gtable(gt[1:2, 1], make_gt(grobmat, 1:2, 1)))
  expect_true(equal_gtable(gt[1:2, 2], make_gt(grobmat, 1:2, 2)))
  expect_true(equal_gtable(gt[1, 1:3], make_gt(grobmat, 1, 1:3)))
  expect_true(equal_gtable(gt[1, 1:2], make_gt(grobmat, 1, 1:2)))
  expect_true(equal_gtable(gt[1:2, 1:2], make_gt(grobmat, 1:2, 1:2)))
  expect_true(equal_gtable(gt[1:2, 2:3], make_gt(grobmat, 1:2, 2:3)))

  # New table from non-contiguous cells
  expect_true(equal_gtable(gt[1, c(1, 3)], make_gt(grobmat, 1, c(1, 3))))
  expect_true(equal_gtable(gt[1:2, c(1, 3)], make_gt(grobmat, 1:2, c(1, 3))))
})


test_that("Indexing with names", {
  # Make a 2x3 gtable where each cell has one grob
  grobmat <- matrix(list(g1, g2, g3, g4, g5, g6), nrow=2)
  gt <- make_gt(grobmat, 1:2, 1:3)
  dimnames(gt) <- list(c("a","b"), c("x","y","z"))

  expect_true(equal_gtable(gt, gt[c("a","b"), c("x","y","z")]))
  expect_true(equal_gtable(gt[1, ], gt["a", ]))
  expect_true(equal_gtable(gt[, 2], gt[, "y"]))
  expect_true(equal_gtable(gt[, 2:3], gt[, c("y","z")]))
  expect_true(equal_gtable(gt[1, 1:2], gt["a", c("x","y")]))
  expect_true(equal_gtable(gt[1, 1:2], gt["a", 1:2]))
})



# Make a gtable with grobs that span cells
make_span_gt <- function(rows, cols) {
  # Make gtable with one grob at (1:1, 1:3) and another at (1:2, 1:2)
  gt <- gtable(name = "test",
    heights=unit(rows, "cm"), widths=unit(cols, "cm") )

  if (all(1 %in% rows) && all(c(1,3) %in% cols)) {
    gt <- gtable_add_grob(gt, g3, 1, 1, 1, length(cols))
  }
  if (all(1:2 %in% rows) && all(c(1,2) %in% cols)) {
    gt <- gtable_add_grob(gt, g4, 1, 1, 2, 2)
  }
  gt
}

test_that("Indexing with grobs that span cells", {

  # Make a gtable with two grobs that span cells
  gt <- make_span_gt(1:2, 1:3)

  # Indexing in ways that don't change gt
  expect_true(equal_gtable(gt, gt[1:2, 1:3]))

  # If a cell at the end of a grob is dropped, drop the grob
  # These should drop all grobs
  expect_true(equal_gtable(gt[1, 2], make_span_gt(1, 2)))
  expect_equal(length(gt[1, 2]$grobs), 0)
  expect_true(equal_gtable(gt[1:2, 2], make_span_gt(1:2, 2)))
  expect_equal(length(gt[1:2, 2]$grobs), 0)

  # These should preserve one of the grobs
  expect_true(equal_gtable(gt[1:2, 1:2], make_span_gt(1:2, 1:2)))
  expect_equal(length(gt[1:2, 1:2]$grobs), 1)
  expect_true(equal_gtable(gt[1, 1:3], make_span_gt(1, 1:3)))
  expect_equal(length(gt[1, 1:3]$grobs), 1)

  # If a cell in the middle of a grob is dropped, don't drop the grob
  expect_true(equal_gtable(gt[1, c(1,3)], make_span_gt(1, c(1,3))))
  expect_equal(length(gt[1, c(1,3)]$grobs), 1)

  # Currently undefined behavior:
  # What happens when you do repeat rows/cols, like gt[1, c(1,1,1,3)] ?
  # What happens when order is non-monotonic, like gt[1, c(3,1,2)] ?
})
