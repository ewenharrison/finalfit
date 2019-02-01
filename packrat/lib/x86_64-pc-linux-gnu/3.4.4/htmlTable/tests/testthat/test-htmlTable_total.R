library(testthat)

context("htmlTable - the total argument")
test_that("Throws errors",{
  mx <- matrix(1, ncol=3, nrow=6)
  expect_error(htmlTable(mx, total = c(TRUE, TRUE)))
  expect_error(htmlTable(mx, total = c(TRUE, TRUE),
                         tspanner = letters[1:3], n.tspanner = rep(2, times = 3)))
  expect_error(htmlTable(mx, total = -1))
  expect_error(htmlTable(mx, total = nrow(mx) + 1))
  expect_error(htmlTable(mx, total = "asdasd"))
})

test_that("Correct rows",{
  mx <- matrix(1:6, ncol=3, nrow=6)
  table_str <- htmlTable(mx,
                         css.total = "color: red",
                         total=TRUE)
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>6</td>")

  table_str <- htmlTable(mx,
                         css.total = "color: red",
                         total=4)
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>4</td>")

  table_str <- htmlTable(mx,
                         css.total = "color: red",
                         total=c(4, 2))
  expect_false(grepl("<tr[^>]*>[^>]+color: red[^>]+>[1356789]+</td>", table_str))
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>2</td>")
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>4</td>")

  table_str <- htmlTable(mx,
                         css.total = "color: red",
                         total=c(4, 2))
  expect_false(grepl("<tr[^>]*>[^>]+color: red[^>]+>[1356789]+</td>", table_str))
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>2</td>")
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>4</td>")
})

test_that("Check tspanner", {
  mx <- matrix(1:6, ncol=3, nrow=6)
  table_str <- htmlTable(mx, tspanner = letters[1:2], n.tspanner = c(3, 3),
                         css.total = "color: red",
                         total="tspanner")
  expect_false(grepl("<tr[^>]*>[^>]+color: red[^>]+>[1245789]+</td>", table_str))
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>3</td>")
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>6</td>")
})

test_that("Check choosing css.style", {
  mx <- matrix(1:6, ncol=3, nrow=6)
  table_str <- htmlTable(mx, tspanner = letters[1:2], n.tspanner = c(3, 3),
                         css.total = c("color: red", "color: green"),
                         total="tspanner")
  expect_false(grepl("<tr[^>]*>[^>]+color: red[^>]+>[1245789]+</td>", table_str))
  expect_match(table_str, "<tr[^>]*>[^>]+color: red[^>]+>3</td>")
  expect_match(table_str, "<tr[^>]*>[^>]+color: green[^>]+>6</td>")
})

test_that("The total should be added to the output if used with addmargins", {
  var1 <- LETTERS[1:3]
  var2 <- LETTERS[c(4:5, 5)]
  total_out <-
    table(var1, var2) %>%
    addmargins %>%
    htmlTable(css.total = "background: purple")

  expect_match(total_out, "<td[^>]+background: purple[^>]+>[^>]*Sum</td>",
               info = "Expect the variable name to appear as a cgroup")

  expect_match(total_out, "<th colspan='2'[^>]*>var2",
               info = "Expect the variable name to appear as a cgroup")
})
