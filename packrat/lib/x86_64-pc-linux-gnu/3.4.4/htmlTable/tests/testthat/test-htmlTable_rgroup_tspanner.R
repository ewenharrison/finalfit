library(testthat)
library(XML)

context("htmlTable - the rgroup argument")

test_that("Check that rgroup has the appropriate padding",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- sprintf("Row %s", LETTERS[1:NROW(mx)])
  out <- htmlTable(mx, rgroup = paste("rgroup", 1:2), n.rgroup = rep(1, 2))

  expect_match(out,
               "<tr[^>]*><td[^>]*>rgroup 1")

  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>&nbsp;&nbsp;Row A")

  expect_match(out,
               "<tr[^>]*><td[^>]*>rgroup 2")
  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>&nbsp;&nbsp;Row B")

  out <- htmlTable(mx, rgroup = paste("rgroup", 1:2), n.rgroup = rep(1, 2), padding.rgroup = "ll")

  expect_match(out,
               "<tr[^>]*><td[^>]*>rgroup 1")

  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>llRow A")

  out <- htmlTable(mx, rgroup = paste("rgroup", 1:2), n.rgroup = rep(1, 2), tspanner = paste("tspanner", 1:2), n.tspanner = rep(1, 2), padding.tspanner = "ii", padding.rgroup = "ll")

  expect_match(out,
               "<tr[^>]*><td[^>]*>iirgroup 1")

  expect_match(out,
               "<tr[^>]*>[^<]*<td[^>]*>iillRow A")

})

test_that("Check that dimensions are correct with rgroup usage",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <-
    suppressWarnings(htmlTable(mx,
                               rgroup=c("test1", "test2"),
                               n.rgroup=c(1,1)))
  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(ncol(parsed_table), ncol(mx), info="Cols did not match")
  expect_equal(nrow(parsed_table), nrow(mx) + 2, info="Rows did not match")
  expect_equal(as.character(parsed_table[1,1]),
               "test1", info="The rgroup did not match")
  expect_equal(as.character(parsed_table[3,1]),
               "test2", info="The rgroup did not match")
  expect_equal(as.character(parsed_table[2,1]),
               as.character(mx[1,1]), info="The row values did not match")
  expect_equal(as.character(parsed_table[4,1]),
               as.character(mx[2,1]), info="The row values did not match")


  expect_warning(htmlTable(mx,
                           rgroup=c("test1", "test2", "test3"),
                           n.rgroup=c(1,1, 0)))

  expect_error(suppressWarnings(htmlTable(mx,
                                          roup=c("test1", "test2", "test3"),
                                          rgroup=c(1,1, 10))))

  mx[2,1] <- "second row"
  table_str <- htmlTable(mx,
                         rnames=letters[1:2],
                         rgroup=c("test1", ""),
                         n.rgroup=c(1,1))
  expect_match(table_str, "<td[^>]*>second row",
               info="The second row should not have any spacers")

  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(nrow(parsed_table), nrow(mx) + 1, info="Rows did not match")
})



test_that("Check rgroup attribute",{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- sprintf("Row %s", LETTERS[1:NROW(mx)])

  rgroup <- paste("rgroup", 1:2)
  attr(rgroup, "add") <- "test"
  expect_error(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)))

  attr(rgroup, "add") <- c("test 1", "test 2")
  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)),
               "<td[^>]+colspan[ ]*=[ ]*'3'[^>]+>rgroup 1</td>[^<]*<td[^>]*>test 1")

  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)),
               "<td[^>]+colspan[ ]*=[ ]*'3'[^>]+>rgroup 1</td>[^<]*<td[^>]*>test 1")

  attr(rgroup, "add") <- c(`1` = "test c")
  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)),
               "<td[^>]+colspan[ ]*=[ ]*'3'[^>]+>rgroup 1</td>[^<]*<td[^>]*>test c")

  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2), css.rgroup = ""),
               "<td[^>]+colspan[ ]*=[ ]*'3'[^>]+>rgroup 1</td>[^<]*<td[^>]*>test c")

  attr(rgroup, "add") <- list(`2` = "test d")
  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)),
               "<td[^>]+colspan[ ]*=[ ]*'3'[^>]+>rgroup 2</td>[^<]*<td[^>]*>test d")


  attr(rgroup, "add") <- list(`1` = list(`2` = "test d"))
  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)),
               "<td[^>]+colspan[ ]*=[ ]*'2'[^>]+>rgroup 1</td>[^<]*<td[^>]*>test d")

  attr(rgroup, "add") <- list(`1` = list(`2` = "test d", `3` = "test e"))
  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)),
               "<td[^>]+colspan[ ]*=[ ]*'2'[^>]+>rgroup 1</td>[^<]*<td[^>]*>test d</td>[^<]*<td[^>]*>test e")

  attr(rgroup, "add") <- list(`1` = list(`44` = "test d"))
  expect_error(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)))

  attr(rgroup, "add") <- list(`1` = list(`asda` = "test d"))
  expect_error(suppressWarnings(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2))))

  attr(rgroup, "add") <- list(`1` = list(`-23` = "test d"))
  expect_error(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)))

  attr(rgroup, "add") <- list(`-1` = list(`3` = "test d"))
  expect_error(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)))

  attr(rgroup, "add") <- list(`23` = list(`3` = "test d"))
  expect_error(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)))


  rgroup[2] <- ""
  attr(rgroup, "add") <- list(`2` = "test d")
  expect_error(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)))

  attr(rgroup, "add") <- list("test d")
  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)),
               "test d")

  attr(rgroup, "add") <- list("test d", "test e")
  expect_error(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2)))
})


test_that("Check rgroup attribute without CSS",{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- sprintf("Row %s", LETTERS[1:NROW(mx)])

  rgroup <- paste("rgroup", 1:2)
  attr(rgroup, "add") <- list(`1` = "test d")
  expect_match(htmlTable(mx, rgroup = rgroup, n.rgroup = rep(1, 2), css.rgroup = ""),
               "<td[^>]+>rgroup 1</td>[^<]*<td[^>]*>test d</td>")
})

test_that("Check rgroup attribute with matrix",{
  mx <- matrix(1:6, ncol=2)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- sprintf("Row %s", LETTERS[1:NROW(mx)])

  rgroup <- c(paste("rgroup", 1:2), "")
  attr(rgroup, "add") <- matrix(c("test a",
                                  "test b"),
                                ncol = 1)
  out <- htmlTable(mx,
                   rgroup = rgroup,
                   n.rgroup = c(1, 1),
                   css.rgroup = "")

  expect_match(out,
               "<td[^>]+>rgroup 1</td>[^<]*<td[^>]*>test a</td>")
  expect_match(out,
               "<td[^>]+>rgroup 2</td>[^<]*<td[^>]*>test b</td>")


  rgroup <- c(paste("rgroup", 1:2), "")
  add_mtrx <- matrix(1:4,
                     ncol = 2)
  attr(rgroup, "add") <- add_mtrx
  out <- htmlTable(mx,
                   rgroup = rgroup,
                   n.rgroup = c(1, 1),
                   css.rgroup = "")
  expect_match(out,
               paste0("<td[^>]+>rgroup 1</td>",
                      paste(sprintf("[^<]*<td[^>]*>%d</td>", add_mtrx[1,]),
                            collapse = ""),
                      "[^<]*</tr"))
  expect_match(out,
               paste0("<td[^>]+>rgroup 2</td>",
                      paste(sprintf("[^<]*<td[^>]*>%d</td>", add_mtrx[2,]),
                            collapse = ""),
                      "[^<]*</tr"))

  add_mtrx <- matrix(1:2,
                     ncol = 2)
  rownames(add_mtrx) <- c("rgroup 2")
  attr(rgroup, "add") <- add_mtrx
  out <- htmlTable(mx,
                   rgroup = rgroup,
                   n.rgroup = c(1, 1),
                   css.rgroup = "")
  expect_match(out,
               paste0("<td[^>]+>rgroup 2</td>",
                      paste(sprintf("[^<]*<td[^>]*>%d</td>", add_mtrx[1,]),
                            collapse = ""),
                      "[^<]*</tr"))
  expect_match(out,
               paste0("<td[^>]+>rgroup 1</td>",
                      "[^<]*</tr"))

  add_mtrx <- matrix(1:3,
                     ncol = 3)
  rownames(add_mtrx) <- c("rgroup 2")
  attr(rgroup, "add") <- add_mtrx
  expect_error(htmlTable(mx,
                         rgroup = rgroup,
                         n.rgroup = c(1, 1)))

  add_mtrx <- matrix(1:2,
                     ncol = 2)
  rownames(add_mtrx) <- c("rgroup 223")
  attr(rgroup, "add") <- add_mtrx
  expect_error(htmlTable(mx,
                         rgroup = rgroup,
                         n.rgroup = c(1, 1)))
})

test_that("Matrix with rownames and rgroup with 1 element", {
  a <- matrix(1:4, ncol=2)
  rownames(a) <- LETTERS[3:4]
  expect_silent(
    htmlTable(a,
              rgroup = LETTERS[1:2],
              n.rgroup = c(1,1),
              caption="Site for UVR & Visibility for any mortality indicating melanoma"))
})
