context("Margins")

vars <- list(c("a", "b", "c"), c("d", "e", "f"))
test_that("margins expanded", {
  expect_that(margins(vars, "c")[[2]], equals(c("c")))
  expect_that(margins(vars, "b")[[2]], equals(c("b", "c")))
  expect_that(margins(vars, "a")[[2]], equals(c("a", "b", "c")))

  expect_that(margins(vars, "f")[[2]], equals(c("f")))
  expect_that(margins(vars, "e")[[2]], equals(c("e", "f")))
  expect_that(margins(vars, "d")[[2]], equals(c("d", "e", "f")))
})

test_that("margins intersect", {
  expect_that(margins(vars, c("c", "f"))[-1],
    equals(list("c", "f", c("c", "f"))))

})

test_that("(all) comes after NA", {
  df <- data.frame(a = c("a", "b", NA), b = c("a", "b", NA), value = 1)

  df2 <- add_margins(df, "a")
  expect_that(levels(df2$a), equals(c("a", "b", NA, "(all)")))

  df3 <- add_margins(df, c("a", "b"))
  expect_that(levels(df3$a), equals(c("a", "b", NA, "(all)")))
  expect_that(levels(df3$b), equals(c("a", "b", NA, "(all)")))

  dc <- dcast(df, a ~ ., margins = TRUE, fun = length)
  expect_that(levels(dc$a), equals(c("a", "b", NA, "(all)")))
  expect_that(as.character(dc$a), equals(c("a", "b", NA, "(all)")))

})
