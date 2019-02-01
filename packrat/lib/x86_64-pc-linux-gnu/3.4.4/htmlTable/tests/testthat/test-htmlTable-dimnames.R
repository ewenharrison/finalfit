library(testthat)

context("htmlTable - dimnames")

test_that("First dimname should be converted to rgroup, tspanner or rowlabel", {
  var1 <- LETTERS[1:3]
  var2 <- LETTERS[c(4:5, 5)]
  basic_label <-
    table(var1, var2) %>%
    htmlTable(css.rgroup = "background: blue")

  expect_match(basic_label, "<tr><td[^>]+background: blue[^>]+>var1</td></tr>",
               info = "Expect the variable name to appear as an rgroup")
  expect_match(basic_label, "<td[^>]+>&nbsp;&nbsp;A</td>",
               info = "Expect the variable name to appear as an rgroup")
  expect_match(basic_label, "<td[^>]+>&nbsp;&nbsp;B</td>",
               info = "Expect the variable name to appear as an rgroup")
  expect_match(basic_label, "<td[^>]+>&nbsp;&nbsp;C</td>",
               info = "Expect the variable name to appear as an rgroup")

  tspanner_label <-
    table(var1, var2) %>%
    htmlTable(rgroup=c("alt"),
              n.rgroup=c(3),
              css.tspanner = "background: red",
              css.rgroup = "background: blue")

  expect_match(tspanner_label, "<tr><td[^>]+background: red[^>]+>var1</td></tr>",
               info = "Expect the variable name to appear as an tspanner")
  expect_match(tspanner_label, "<tr><td[^>]+background: blue[^>]+>alt</td></tr>",
               info = "Expect the rgroup name to appear as usual")
  expect_match(tspanner_label, "<td[^>]+>&nbsp;&nbsp;A</td>")
  expect_match(tspanner_label, "<td[^>]+>&nbsp;&nbsp;B</td>")
  expect_match(tspanner_label, "<td[^>]+>&nbsp;&nbsp;C</td>")


  rowlabel_label <-
    table(var1, var2) %>%
    htmlTable(rgroup=c("alt"),
              n.rgroup=c(3),
              tspanner=c("alt2"),
              n.tspanner = c(3),
              css.tspanner = "background: red",
              css.rgroup = "background: blue")

  expect_match(rowlabel_label, "<tr><td[^>]+background: red[^>]+>alt2</td></tr>",
               info = "Expect the variable name to appear as an tspanner")
  expect_match(rowlabel_label, "<tr><td[^>]+background: blue[^>]+>alt</td></tr>",
               info = "Expect the rgroup name to appear as usual")
  expect_match(rowlabel_label, "<td[^>]+>&nbsp;&nbsp;A</td>")
  expect_match(rowlabel_label, "<td[^>]+>&nbsp;&nbsp;B</td>")
  expect_match(rowlabel_label, "<td[^>]+>&nbsp;&nbsp;C</td>")
})

test_that("Second dimname should be converted to cgroup", {
  var1 <- LETTERS[1:3]
  var2 <- LETTERS[c(4:5, 5)]
  basic_label <-
    table(var1, var2) %>%
    htmlTable

  expect_match(basic_label, "<th[^>]+>var2</th>",
               info = "Expect the variable name to appear as a cgroup")
})
