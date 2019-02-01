source("helper/helper.R")

if (exists("startsWith", envir = baseenv())) {
  f = get("startsWith", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::startsWith)

  expect_same("a", "a")
  expect_same(NA, "a")
  expect_same("a", NA)
  expect_same("a", "")
  expect_same("", "a")
  expect_same("", "")
  expect_same(c("a", NA, "b"), "b")
  expect_same("b", c("a", NA, "b"))
  expect_same(letters, c("m", NA, ""))
}

if (exists("endsWith", envir = baseenv())) {
  f = get("endsWith", envir = baseenv())
  expect_same = makeCompareFun(f, backports:::endsWith)

  expect_same("a", "a")
  expect_same(NA, "a")
  expect_same("a", NA)
  expect_same("a", "")
  expect_same("", "a")
  expect_same("", "")
  expect_same(c("a", NA, "b"), "b")
  expect_same("b", c("a", NA, "b"))
  expect_same(letters, c("m", NA, ""))
}

# adapted from R's unit tests
t1 = c("Foobar", "bla bla", "something", "another", "blu", "brown", "blau blüht der Enzian")
t2 = c("some text", "any text")
t3 = c("Martin", "Zürich", "Mächler")

expect_true(all(backports:::startsWith(t1, "")))
expect_true(all(backports:::endsWith(t1, "")))
expect_true(all(backports:::endsWith(t2, "")))
expect_true(all(backports:::startsWith(t2, "")))
expect_true(all(backports:::endsWith(t3, "")))
expect_true(all(backports:::startsWith(t3, "")))
expect_true(all(backports:::endsWith(t2, "text")))
expect_true(all(backports:::endsWith(t2, " text")))
expect_identical(backports:::startsWith(t1, "b" ), c(FALSE, TRUE, FALSE, FALSE, TRUE,  TRUE, TRUE))
expect_identical(backports:::startsWith(t1, "bl"), c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))
expect_identical(backports:::startsWith(t1, "bla"),c(FALSE, TRUE, FALSE, FALSE,FALSE, FALSE, TRUE))
expect_identical(backports:::endsWith(t1, "n"),  c(FALSE,FALSE, FALSE, FALSE,FALSE,  TRUE, TRUE))
expect_identical(backports:::endsWith(t1, "an"), c(FALSE,FALSE, FALSE, FALSE,FALSE, FALSE, TRUE))
expect_identical(backports:::startsWith(t3, "M" ), c( TRUE, FALSE, TRUE))
expect_identical(backports:::startsWith(t3, "Ma"), c( TRUE, FALSE, FALSE))
expect_identical(backports:::startsWith(t3, "Mä"), c(FALSE, FALSE, TRUE))
